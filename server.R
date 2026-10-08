server <- function(input, output, session) {

  simulation <- eventReactive(input$simulate, {

    validate_parameters(
      dose = input$dose,
      cl = input$cl,
      v = input$v,
      ka = input$ka,
      end_time = input$end_time
    )

    simulate_pk1(
      dose = input$dose,
      cl = input$cl,
      v = input$v,
      ka = input$ka,
      end_time = input$end_time
    )
  })

  output$pk_plot <- renderPlot({
    shiny::req(input$simulate > 0)

    create_pk_plot(simulation())
  })

  output$results <- renderTable({
    shiny::req(input$simulate > 0)

    head(simulation(), 10)
  })
}