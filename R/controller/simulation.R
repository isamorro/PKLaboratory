# ============================================================
# Archivo: simulation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador que gestiona la simulación: valida los
# datos, ejecuta el modelo y muestra los errores al usuario.
# ============================================================

# Gestiona la simulación: valida los datos, ejecuta el modelo y muestra
# los errores al usuario.
# Recibe: entradas y salidas de Shiny y reactive con el modelo cargado.
# Devuelve: reactive con los datos simulados (vacío si hubo errores).
simulation_controller <- function(input, output, model) {

  # Resultado de pulsar "Simular": o bien datos, o bien mensajes de error.
  sim_result <- eventReactive(input$simulate, {

    shiny::req(model())
    params <- read_param_inputs(input, names(get_model_params(model())))

    # Validación previa (mensajes en español)
    errors <- validate_simulation(
      params = params,
      dose = input$dose,
      dose_cmt = input$dose_cmt,
      model_cmts = get_model_cmt(model()),
      n_doses = input$n_doses,
      interval = input$interval,
      end_time = input$end_time,
      apply_param_rules = !isTRUE(attr(model(), "pk_components"))
    )
    if (length(errors) > 0) {
      return(list(data = NULL, errors = errors))
    }

    # Simulación (los errores de mrgsolve llegan ya traducidos)
    tryCatch(
      list(
        data = simulate_model(
          mod = model(),
          params = params,
          dose = input$dose,
          dose_cmt = input$dose_cmt,
          n_doses = input$n_doses,
          interval = input$interval,
          end_time = input$end_time
        ),
        errors = character(0)
      ),
      error = function(e) list(data = NULL, errors = conditionMessage(e))
    )
  })

  # Único lugar donde se muestran los errores (caja roja).
  output$sim_errors <- renderUI({
    errors <- sim_result()$errors
    if (length(errors) == 0) {
      return(NULL)
    }

    div(
      class = "alert alert-danger",
      role = "alert",
      lapply(errors, div)
    )
  })

  # Datos de la simulación; si hubo errores, las salidas quedan vacías.
  reactive({
    result <- sim_result()
    shiny::req(length(result$errors) == 0)
    result$data
  })
}
