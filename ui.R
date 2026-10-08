ui <- fluidPage(

  titlePanel("Simulación farmacocinética"),

  sidebarLayout(

    sidebarPanel(

      numericInput(
        "dose",
        "Dosis",
        value = 500,
        min = 1
      ),

      numericInput(
        "cl",
        "Aclaramiento CL",
        value = 5,
        min = 0.01
      ),

      numericInput(
        "v",
        "Volumen V",
        value = 50,
        min = 0.01
      ),

      numericInput(
        "ka",
        "Constante de absorción",
        value = 1,
        min = 0.01
      ),

      numericInput(
        "end_time",
        "Tiempo final",
        value = 24,
        min = 1
      ),

      actionButton(
        "simulate",
        "Simular"
      )
    ),

    mainPanel(
      plotOutput("pk_plot"),
      tableOutput("results")
    )
  )
)