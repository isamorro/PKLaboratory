# ============================================================
# Archivo: shared_catalog.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Gestiona el catálogo de modelos de la base de datos: carga los modelos del
# usuario y los compartidos por otros, mantiene el selector de modelos y
# permite compartir o dejar de compartir los modelos propios.
# ============================================================

# Gestiona el catálogo de modelos guardados en la base de datos.
# Recibe: entradas, salidas y sesión de Shiny, reactiveVal con los modelos
# propios y con los compartidos, conexión a la base de datos, reactiveVal con
# el modelo seleccionado y reactiveVal con el usuario actual.
# Devuelve: función refresh() que vuelve a consultar el catálogo del usuario
# actual (TRUE si lo consigue). Se llama al iniciar sesión.
shared_catalog_controller <- function(input, output, session, user_models,
                                      shared_models, db, selected_model_id,
                                      current_user) {
  # Consulta la base de datos y actualiza los modelos propios y los
  # compartidos del usuario actual.
  refresh <- function() {
    user <- shiny::isolate(current_user())
    if (is.null(user)) return(FALSE)

    tryCatch({
      rows <- list_user_models(db, user$username)
      mine <- rows$owner_username %in% user$username
      shared <- rows$is_shared %in% TRUE

      shared_models(rows[shared, names(empty_shared_models()), drop = FALSE])
      user_models(merge_own_models(
        shiny::isolate(user_models()),
        rows[mine, , drop = FALSE]
      ))
      TRUE
    }, error = function(e) {
      message("[catálogo] ", conditionMessage(e))
      FALSE
    })
  }

  # El modelo elegido en el selector es el modelo seleccionado. Así, cuando el
  # catálogo cambia, el selector se actualiza sin perder lo que había elegido.
  observeEvent(input$model_id, selected_model_id(input$model_id))

  # Mantiene las opciones del selector de modelos al día.
  observe({
    shiny::req(current_user())
    local <- user_models()
    remote <- shared_models()
    available <- c(list_models(), names(local), paste0("db_", remote$id))
    current <- shiny::isolate(selected_model_id())
    selected <- if (length(current) == 1L && current %in% available) {
      current
    } else {
      default_model_id
    }
    selected_model_id(selected)
    updateSelectInput(
      session,
      "model_id",
      choices = model_choices(local, remote),
      selected = selected
    )
  })

  # Cambia si un modelo propio y guardado está compartido.
  change_sharing <- function(id, shared) {
    user <- current_user()
    models <- user_models()
    shiny::req(user, length(id) == 1L, id %in% names(models))

    # Si solo existía en la sesión, se guarda antes de publicarlo.
    if (!isTRUE(models[[id]]$saved)) {
      entry <- models[[id]]
      saved_id <- tryCatch(
        save_user_model(db, entry$spec, entry$mod, user$username),
        error = function(e) {
          showNotification(
            "No se pudo guardar el modelo para añadirlo al catálogo.",
            type = "error"
          )
          NULL
        }
      )
      if (is.null(saved_id)) return()
      new_id <- paste0("db_", saved_id)
      entry$id <- new_id
      entry$saved <- TRUE
      models[[new_id]] <- entry
      models[[id]] <- NULL
      user_models(models)
      if (identical(selected_model_id(), id)) selected_model_id(new_id)
      id <- new_id
    }

    changed <- tryCatch({
      set_model_shared(db, sub("^db_", "", id), user$username, shared)
      TRUE
    }, error = function(e) {
      message("[catálogo] ", conditionMessage(e))
      showNotification(
        "No se pudo cambiar el catálogo. Inténtalo de nuevo.",
        type = "error"
      )
      FALSE
    })
    if (!changed) return()

    models[[id]]$is_shared <- shared
    user_models(models)
    refresh()
    showNotification(
      if (shared) "Modelo compartido con el resto de usuarios."
      else "Has dejado de compartir el modelo.",
      type = "message"
    )
  }

  observeEvent(input$model_publish, {
    change_sharing(input$model_publish, TRUE)
  })
  observeEvent(input$model_unpublish, {
    change_sharing(input$model_unpublish, FALSE)
  })

  refresh
}
