# ============================================================
# Archivo: results.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador que gestiona los resultados de la
# simulación: selector de variables, gráfico y tabla.
# ============================================================

# Nombre de archivo para las descargas.
# Recibe: nombre del modelo y extensión.
# Devuelve: nombre con modelo y fecha, por ejemplo simulacion_pk1_20261014.csv.
# El nombre del modelo se limpia para que sea válido como nombre de archivo.
download_name <- function(model_name, ext) {
  id <- model_id_from_name(model_name)
  if (!nzchar(id)) id <- "modelo"
  paste0("simulacion_", id, "_", format(Sys.Date(), "%Y%m%d"), ".", ext)
}

# Gestiona los resultados de la simulación: selector de variables,
# gráfico, tabla y descargas.
# Recibe: entradas, salidas y sesión de Shiny, reactive con los datos
# simulados y función que devuelve el nombre de un modelo a partir de su id.
results_controller <- function(input, output, session, simulation,
                               model_label = identity) {

  # Modelo con el que se obtuvieron los resultados: el título del gráfico y
  # los nombres de las descargas no cambian si después se elige otro modelo.
  simulated_model <- eventReactive(simulation(), isolate(input$model_id))

  # Variables disponibles tras simular; se mantiene la selección si sigue
  # siendo válida.
  observeEvent(simulation(), {
    vars <- setdiff(names(simulation()), c("ID", "time"))
    kept <- intersect(input$plot_vars, vars)
    selected <- if (length(kept) > 0) kept else vars

    updateSelectInput(
      session, "plot_vars",
      choices = vars,
      selected = selected
    )
  })

  output$pk_plot <- renderPlot(
    {
      shiny::req(input$simulate > 0)
      data <- simulation()
      shiny::req(
        length(input$plot_vars) > 0,
        all(input$plot_vars %in% names(data))
      )

      create_pk_plot(
        data, input$plot_vars, model_label(simulated_model()),
        narrow = plot_is_narrow(session, "pk_plot")
      )
    },
    height = function() {
      plot_height(
        length(input$plot_vars),
        narrow = plot_is_narrow(session, "pk_plot")
      )
    }
  )

  output$results <- renderTable({
    shiny::req(input$simulate > 0)

    data <- simulation()
    head(data, 150)
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

  # Descarga de todos los resultados.
  output$download_csv <- downloadHandler(
    filename = function() download_name(model_label(simulated_model()), "csv"),
    content = function(file) {
      utils::write.csv(simulation(), file, row.names = FALSE)
    }
  )

  # Misma descarga con separador ";" y coma decimal, para Excel en español.
  output$download_csv_excel <- downloadHandler(
    filename = function() download_name(model_label(simulated_model()), "csv"),
    content = function(file) {
      utils::write.csv2(simulation(), file, row.names = FALSE)
    }
  )

  # Descarga del gráfico.
  output$download_plot <- downloadHandler(
    filename = function() download_name(model_label(simulated_model()), "png"),
    content = function(file) {
      data <- simulation()
      shiny::req(
        length(input$plot_vars) > 0,
        all(input$plot_vars %in% names(data))
      )

      ggplot2::ggsave(
        file,
        plot = create_pk_plot(
          data, input$plot_vars, model_label(simulated_model())
        ),
        device = "png",
        width = 10,
        height = plot_height(length(input$plot_vars)) / 100,
        dpi = 150,
        bg = "white"
      )
    }
  )
}
