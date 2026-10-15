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
# Recibe: entradas, salidas y sesión de Shiny.
# Devuelve: reactive con el modelo cargado (NULL si falla la carga).
model_controller <- function(input, output, session) {

  # Se compila la primera vez y luego se reutiliza (caché del modelo).
  # Si falla la carga, se avisa al usuario en lugar de tumbar la sesión.
  model <- reactive({
    shiny::req(input$model_id)

    tryCatch(
      load_model(input$model_id),
      error = function(e) {
        showNotification(
          paste("No se pudo cargar el modelo:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
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
    updateSelectInput(
      session, "dose_cmt",
      choices = cmts,
      selected = utils::head(cmts, 1)
    )
  })

  model
}
