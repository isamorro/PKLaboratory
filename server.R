# ============================================================
# Archivo: server.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la lógica del servidor y gestiona el funcionamiento
# reactivo de la aplicación PKLaboratory.
# ============================================================

# Gestiona la lógica reactiva y conecta las entradas del usuario
# con las simulaciones y la presentación de los resultados.
# Recibe: entradas, salidas y sesión de Shiny.
server <- function(input, output, session) {

  # Modelo seleccionado (se compila la primera vez y luego se reutiliza).
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

  output$param_inputs <- renderUI({
    shiny::req(model())
    build_param_inputs(get_model_params(model()))
  })

  observeEvent(model(), {
    shiny::req(model())
    cmts <- get_model_cmt(model())
    updateSelectInput(session, "dose_cmt", choices = cmts,
                      selected = utils::head(cmts, 1))
  })

  # Resultado de pulsar "Simular": o bien datos, o bien mensajes de error.
  sim_result <- eventReactive(input$simulate, {

    shiny::req(model())
    params <- read_param_inputs(input, names(get_model_params(model())))

    # 1. Validación previa (mensajes en español)
    errors <- validate_simulation(
      model_id = input$model_id,
      params = params,
      dose = input$dose,
      dose_cmt = input$dose_cmt,
      model_cmts = get_model_cmt(model()),
      n_doses = input$n_doses,
      interval = input$interval,
      end_time = input$end_time
    )
    if (length(errors) > 0) {
      return(list(data = NULL, errors = errors))
    }

    # 2. Simulación (los errores de mrgsolve llegan ya traducidos)
    tryCatch(
      list(
        data = simulate_model(
          model_id = input$model_id,
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

  # Datos de la simulación; si hubo errores, las salidas quedan vacías.
  simulation <- reactive({
    result <- sim_result()
    shiny::req(length(result$errors) == 0)
    result$data
  })

  # Se muestran los errores.
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

  # Variables disponibles tras simular
  # Se mantiene la selección si sigue siendo válida.
  observeEvent(simulation(), {
    vars <- setdiff(names(simulation()), c("ID", "time"))
    kept <- intersect(input$plot_vars, vars)
    selected <- if (length(kept) > 0) kept else vars

    updateSelectInput(session, "plot_vars", choices = vars, selected = selected)
  })

  output$pk_plot <- renderPlot(
    {
      shiny::req(input$simulate > 0)
      data <- simulation()
      shiny::req(length(input$plot_vars) > 0, all(input$plot_vars
                                                  %in% names(data)))

      create_pk_plot(data, input$plot_vars, input$model_id)
    },
    height = function() plot_height(length(input$plot_vars))
  )

  output$results <- renderTable({
    shiny::req(input$simulate > 0)

    data <- simulation()
    head(data, 10)
  })
}