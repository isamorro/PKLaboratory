# ============================================================
# Archivo: builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Gestiona la creación (con el editor de código de mrgsolve y el asistente
# por componentes) y el borrado de modelos propios, mantiene el catálogo
# local y valida, compila y guarda cada modelo antes de pasarlo a la
# simulación.
# ============================================================

# Gestiona la creación y el borrado de modelos propios.
# Recibe: entradas, salidas y sesión de Shiny, reactiveVal con los modelos
# propios y con el catálogo compartido, reactiveVal con el modelo
# seleccionado, conexión a la base de datos, reactiveVal con el usuario
# actual y la caché de modelos compilados de la sesión.
# Devuelve: ninguna salida, pero actualiza los reactives del estado.
builder_controller <- function(input, output, session, user_models,
                               shared_models, selected_model_id, db,
                               current_user, shared_cache) {

  # Prefijo de los nombres internos de los modelos de esta sesión. mrgsolve
  # compila en una carpeta temporal compartida por todas las sesiones, así
  # que el nombre interno incluye el identificador de la sesión.
  prefix <- paste0("usr_", gsub("[^A-Za-z0-9]", "", session$token), "_")
  counter <- 0L

  # Al terminar la sesión se borran los archivos compilados.
  session$onSessionEnded(function() remove_compiled_files(prefix))

  model_editor_controller(
    input,
    output,
    session,
    user_models,
    shared_models,
    db,
    current_user,
    shared_cache
  )

  errors <- reactiveVal(character())
  wizard_errors <- reactiveVal(character())
  reject_save <- error_reporter(errors, "No se ha guardado el modelo")
  reject_file <- error_reporter(errors, "No se ha cargado el archivo")
  reject_wizard <- error_reporter(wizard_errors, "No se ha generado el código")

  model_spec <- reactive(read_model_form(input, "bld_"))

  checked_code <- reactive({
    spec <- model_spec()
    problems <- validate_model_spec(spec)

    if (length(problems) > 0) {
      stop(paste(problems, collapse = "\n"), call. = FALSE)
    }

    spec$code
  })

  # Bloques encontrados en el código, o los problemas que hay que corregir.
  output$bld_code <- renderText({
    tryCatch(
      paste0(
        "Sin problemas detectados.\n\nBloques:\n",
        paste0("  $", model_code_blocks(checked_code()), collapse = "\n"),
        "\n\nAl guardar se compila y se prueba con una dosis."
      ),
      error = function(e) {
        paste0("Revisa el código:\n- ",
               gsub("\n", "\n- ", conditionMessage(e)))
      }
    )
  })

  # Abre el asistente por componentes.
  observeEvent(input$bld_wizard, {
    wizard_errors(character())
    shiny::showModal(component_wizard_modal())
  })

  output$wiz_errors <- renderUI(error_box(wizard_errors()))

  # Genera el código con los componentes del asistente y lo lleva al editor.
  observeEvent(input$wiz_generate, {
    code <- tryCatch(
      generate_component_code(read_component_form(input, "wiz_")),
      error = function(e) {
        reject_wizard(conditionMessage(e))
        NULL
      }
    )
    if (is.null(code)) {
      return()
    }
    updateTextAreaInput(session, "bld_code_text", value = code)
    errors(character())
    removeModal()
  })

  # Carga un archivo .cpp en el editor de código.
  observeEvent(input$bld_code_file, {
    file <- input$bld_code_file
    shiny::req(file)

    if (file$size > max_code_length) {
      reject_file(paste0("El archivo no puede superar ", max_code_length,
                         " caracteres."))
      return()
    }
    code <- tryCatch(
      paste(readLines(file$datapath, warn = FALSE, encoding = "UTF-8"),
            collapse = "\n"),
      error = function(e) NULL
    )
    if (is.null(code) || !validUTF8(code)) {
      reject_file("No se pudo leer el archivo. Debe ser un archivo de texto.")
      return()
    }

    errors(character())
    updateTextAreaInput(session, "bld_code_text", value = code)
    if (!nzchar(clean_text(input$bld_name))) {
      name <- sub("\\.[^.]*$", "", file$name)
      if (is_valid_model_name(name)) {
        updateTextInput(session, "bld_name", value = name)
      }
    }
  })

  # Único lugar donde se muestran los errores de esta página (caja roja).
  output$bld_errors <- renderUI(error_box(errors()))

  output$bld_download <- downloadHandler(
    filename = function() {
      name <- model_id_from_name(clean_text(input$bld_name))
      paste0(if (nzchar(name)) name else "modelo", ".cpp")
    },
    content = function(file) {
      writeLines(checked_code(), file, useBytes = TRUE)
    }
  )

  # Valida, compila y guarda el modelo en la cuenta del usuario.
  observeEvent(input$bld_add, {
    user <- current_user()
    shiny::req(user)
    models <- user_models()

    spec <- model_spec()
    problems <- validate_model_spec(
      spec,
      existing_ids = c(
        list_models(),
        vapply(models, function(m) model_id_from_name(m$label), character(1))
      )
    )

    if (length(models) >= max_user_models) {
      problems <- c(
        problems,
        "Has alcanzado el máximo de modelos propios."
      )
    }

    if (length(problems) > 0) {
      reject_save(problems)
      return()
    }

    counter <<- counter + 1L
    mod <- tryCatch(
      withProgress(
        message = "Compilando y probando el modelo…",
        value = 0.5,
        compile_user_model(spec, paste0(prefix, counter))
      ),
      error = function(e) {
        reject_save(conditionMessage(e))
        NULL
      }
    )
    if (is.null(mod)) {
      return()
    }

    saved_id <- tryCatch(
      save_user_model(db, spec, mod, user$username),
      error = function(e) {
        message("[modelos] ", conditionMessage(e))
        NULL
      }
    )
    saved <- !is.null(saved_id)
    id <- if (saved) paste0("db_", saved_id) else paste0("tmp_", counter)

    models[[id]] <- new_user_model(id, spec, mod, saved = saved)
    user_models(models)
    errors(character())
    selected_model_id(id)
    updateNavbarPage(session, "main_nav", selected = "simulation")

    showNotification(
      if (saved) {
        "Modelo guardado en tu cuenta."
      } else {
        paste(
          "No se pudo guardar en la cuenta.",
          "El modelo solo estará disponible en esta sesión."
        )
      },
      type = if (saved) "message" else "warning",
      duration = if (saved) 5 else NULL
    )
  })

  # Crea el botón de acción para cada modelo del listado.
  # Recibe: identificador, evento, texto visible y clase del botón.
  # Devuelve: elemento HTML de tipo botón.
  row_button <- function(id, event, label, style) {
    tags$button(
      type = "button",
      class = paste("btn btn-sm", style),
      onclick = sprintf(
        "Shiny.setInputValue('%s', %s, {priority: 'event'})",
        event,
        jsonlite::toJSON(id, auto_unbox = TRUE)
      ),
      label
    )
  }

  # Lista los modelos creados en esta sesión (o guardados en la cuenta).
  output$bld_my_models <- renderUI({
    models <- user_models()
    if (!length(models)) {
      return(page_empty(
        "Aún no has creado ningún modelo",
        "Crea uno con «+ Crear modelo nuevo»: aparecerá aquí y en Simulación."
      ))
    }

    # Una tarjeta por modelo: nombre y estado, descripción y acciones.
    div(class = "pk-model-grid", lapply(names(models), function(id) {
      m <- models[[id]]
      shared <- isTRUE(m$is_shared)
      status <- if (!isTRUE(m$saved)) {
        c("Solo en esta sesión", "")
      } else if (shared) {
        c("En el catálogo", "pk-pop-badge-ok")
      } else {
        c("Privado", "")
      }

      div(
        class = "pk-model-card",
        div(
          class = "pk-model-card-head",
          h3(m$label),
          span(class = paste("pk-pop-badge", status[[2]]), status[[1]])
        ),
        p(class = "pk-model-summary", m$summary),
        if (nzchar(m$description)) {
          p(class = "pk-model-summary pk-model-description", m$description)
        },
        div(
          class = "pk-model-card-actions",
          row_button(id, "model_edit", "Editar", "btn-outline-primary"),
          row_button(
            id,
            if (shared) "model_unpublish" else "model_publish",
            if (shared) "Retirar del catálogo" else "Añadir al catálogo",
            "btn-outline-secondary"
          ),
          row_button(id, "model_delete", "Eliminar", "btn-outline-danger")
        )
      )
    }))
  })

  # Elimina el modelo de la cuenta (y de la base de datos si estaba guardado).
  observeEvent(input$model_delete, {
    user <- current_user()
    models <- user_models()
    id <- input$model_delete
    shiny::req(user, length(id) == 1L, id %in% names(models))

    if (isTRUE(models[[id]]$saved)) {
      deleted <- tryCatch({
        delete_user_model(db, sub("^db_", "", id), user$username)
        TRUE
      }, error = function(e) {
        message("[modelos] No se pudo eliminar: ", conditionMessage(e))
        FALSE
      })

      if (!deleted) {
        showNotification(
          "No se pudo eliminar el modelo. Inténtalo de nuevo.",
          type = "error"
        )
        return()
      }

      shared <- shared_models()
      shared_models(shared[paste0("db_", shared$id) != id, , drop = FALSE])
      if (exists(id, envir = shared_cache, inherits = FALSE)) {
        rm(list = id, envir = shared_cache)
      }
    }

    models[[id]] <- NULL
    user_models(models)

    # Si estaba seleccionado, se vuelve al modelo por defecto. El selector se
    # actualiza solo (ver shared_catalog_controller).
    if (identical(selected_model_id(), id)) {
      selected_model_id(default_model_id)
    }
  })
}
