# ============================================================
# Archivo: model.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador que gestiona el modelo seleccionado:
# lo carga, genera el formulario de parámetros y actualiza los
# compartimentos de administración.
# ============================================================

# Gestiona el modelo seleccionado: lo carga, genera el formulario de
# parámetros y actualiza los compartimentos de administración.
# Recibe: entradas, salidas, sesión de Shiny, lista de modelos propios, modelos
# compartidos, caché de modelos compilados, conexión a la base de datos,
# prefijo de compilación y usuario actual.
# Devuelve: reactive con el modelo cargado (NULL si falla la carga).
model_controller <- function(input, output, session, user_models,
                             shared_models, shared_cache, db,
                             compile_prefix, current_user) {

  # Se compila la primera vez y luego se reutiliza (caché del modelo).
  # Si falla la carga, se avisa al usuario en lugar de tumbar la sesión.
  # Solo depende del selector: los modelos propios y compartidos se leen con
  # isolate() para que actualizar el catálogo o compartir un modelo no
  # reinicie los parámetros que el usuario está editando.
  current_definition <- reactive({
    id <- input$model_id
    shiny::req(id)
    user_models()[[id]]$spec
  })
  model <- reactive({
    current_definition()
    id <- input$model_id
    shiny::req(id)

    own <- shiny::isolate(user_models())
    shared <- shiny::isolate(shared_models())
    user <- shiny::isolate(current_user())

    valid_ids <- c(list_models(), names(own), paste0("db_", shared$id))

    if (!(id %in% valid_ids)) {
      return(NULL)
    }

    tryCatch(
      resolve_model(id, own, db, shared_cache, compile_prefix, user$username),
      error = function(e) {
        message("[modelos] ", conditionMessage(e))
        showNotification(
          paste("No se pudo cargar el modelo:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
  })

  # Descripción del modelo seleccionado, junto al selector
  output$model_description <- renderUI({
    text <- model_description(input$model_id, user_models(), shared_models())
    if (nzchar(text)) shiny::p(text)
  })

  # Ventana con el código del modelo seleccionado. Antes se carga el modelo,
  # para que los compartidos estén en la caché.
  observeEvent(input$model_code_view, {
    id <- input$model_id
    shiny::req(id)
    model()
    showModal(model_code_modal(
      model_label(id, user_models(), shared_models()),
      model_description(id, user_models(), shared_models()),
      model_source_code(id, user_models(), shared_cache)
    ))
  })

  # Formulario de parámetros según el modelo
  output$param_inputs <- renderUI({
    shiny::req(model())
    build_param_inputs(get_model_params(model()))
  })

  # Compartimentos donde se puede administrar la dosis
  observeEvent(model(), {
    shiny::req(model())
    cmts <- get_model_cmt(model())

    if (length(cmts) == 0L) {
      updateSelectInput(
        session, "dose_cmt",
        choices = c("Sin administración" = ""),
        selected = ""
      )
      return()
    }

    updateSelectInput(
      session, "dose_cmt",
      choices = cmts,
      selected = utils::head(cmts, 1)
    )
  })

  model
}
