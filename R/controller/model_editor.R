# ============================================================
# Archivo: model_editor.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Permite editar modelos propios desde un diálogo modal sin crear
# duplicados ni destruir el catálogo local. Reusa la misma lógica del
# formulario por componentes para compilar y validar la edición.
# ============================================================

# Edita un modelo propio en una ventana modal, sin crear duplicados.
# Recibe: entradas, salidas y sesión de Shiny, reactives con los modelos,
# el catálogo compartido, la conexión a la base de datos, el usuario actual
# y la caché de modelos compilados.
# Devuelve: ninguna salida, pero actualiza los reactives del estado.
model_editor_controller <- function(
  input,
  output,
  session,
  user_models,
  shared_models,
  db,
  current_user,
  shared_cache
) {
  editing <- reactiveVal(NULL)
  problems <- reactiveVal(character())
  serial <- 0L
  prefix <- paste0("edit_", gsub("[^A-Za-z0-9]", "", session$token), "_")
  session$onSessionEnded(function() remove_compiled_files(prefix))

  # Abre la ventana de edición para un modelo propio del usuario.
  observeEvent(input$model_edit, {
    id <- input$model_edit
    models <- user_models()
    shiny::req(current_user(), length(id) == 1L, id %in% names(models))

    serial <<- serial + 1L
    # Los identificadores cambian en cada apertura para evitar valores antiguos.
    ns <- paste0("edit", serial, "_")
    editing(list(id = id, original = models[[id]]$spec, ns = ns))
    problems(character())

    spec <- models[[id]]$spec
    modal <- shiny::modalDialog(
      title = "Editar modelo por componentes",
      size = "l",
      easyClose = FALSE,
      shiny::uiOutput("edit_errors"),
      component_form(ns, spec),
      footer = shiny::tagList(
        shiny::modalButton("Cancelar"),
        shiny::actionButton(
          "edit_save",
          "Guardar cambios",
          class = "btn-primary"
        )
      )
    )
    shiny::showModal(
      htmltools::tagAppendAttributes(modal, class = "pk-editor-modal")
    )
  })

  # Muestra errores de validación durante la edición.
  output$edit_errors <- renderUI({
    messages <- problems()
    if (length(messages) == 0) {
      return(NULL)
    }

    div(
      class = "alert alert-danger",
      role = "alert",
      lapply(messages, div)
    )
  })

  # Guarda los cambios del modelo editado.
  observeEvent(input$edit_save, {
    e <- editing()
    user <- current_user()
    models <- user_models()
    shiny::req(e, user)

    if (!(e$id %in% names(models))) {
      problems("Este modelo ya no está disponible.")
      return()
    }

    previous <- models[[e$id]]
    spec <- tryCatch(
      read_component_form(input, e$ns),
      error = function(err) {
        problems(conditionMessage(err))
        NULL
      }
    )
    if (is.null(spec)) {
      return()
    }

    others <- models[setdiff(names(models), e$id)]
    invalid <- validate_model_spec(
      spec,
      existing_ids = c(
        list_models(),
        vapply(others, function(m) model_id_from_name(m$label), character(1))
      )
    )
    problems(invalid)
    if (length(invalid)) {
      return()
    }

    serial <<- serial + 1L
    result <- tryCatch({
      mod <- withProgress(
        message = "Comprobando cambios…",
        value = 0.5,
        compile_user_model(spec, paste0(prefix, serial))
      )

      if (isTRUE(previous$saved)) {
        update_user_model(
          db,
          sub("^db_", "", e$id),
          spec,
          mod,
          user$username
        )
      }

      list(mod = mod)
    }, error = function(err) {
      problems(paste("No se han guardado los cambios:", conditionMessage(err)))
      NULL
    })
    if (is.null(result)) {
      return()
    }

    models[[e$id]] <- new_user_model(
      e$id,
      spec,
      result$mod,
      saved = isTRUE(previous$saved),
      is_shared = isTRUE(previous$is_shared)
    )

    if (exists(e$id, envir = shared_cache, inherits = FALSE)) {
      rm(list = e$id, envir = shared_cache)
    }

    user_models(models)
    remote <- shared_models()
    match <- which(paste0("db_", remote$id) == e$id)
    if (length(match)) {
      remote$name[match] <- trimws(spec$name)
      remote$description[match] <- spec$description
      shared_models(remote)
    }

    removeModal()
    editing(NULL)
    showNotification(
      if (isTRUE(previous$saved)) "Cambios guardados."
      else "Cambios aplicados a este modelo de sesión.",
      type = "message"
    )
  })
}
