# ============================================================
# Archivo: ui.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la interfaz de usuario de PKLaboratory y la distribución
# de sus controles y resultados.
# ============================================================

ui <- fluidPage(

  titlePanel("Simulación farmacocinética"),

  sidebarLayout(

    sidebarPanel(

      selectInput(
        "model_id",
        "Modelo",
        choices = list_models(),
        selected = "pk1"
      ),

      h4("Parámetros del modelo"),
      uiOutput("param_inputs"),

      h4("Administración"),

      selectInput(
        "dose_cmt",
        "Compartimento de administración",
        choices = NULL
      ),

      numericInput(
        "dose",
        "Dosis",
        value = 100,
        min = 0
      ),

      numericInput(
        "n_doses",
        "Número de dosis",
        value = 10,
        min = 1,
        step = 1
      ),

      numericInput(
        "interval",
        "Intervalo entre dosis (si hay más de una)",
        value = 24,
        min = 0
      ),

      numericInput(
        "end_time",
        "Tiempo final",
        value = 480,
        min = 0
      ),

      actionButton(
        "simulate",
        "Simular"
      )
    ),

    mainPanel(
      uiOutput("sim_errors"),
      selectInput(
        "plot_vars",
        "Variables a representar",
        choices = NULL,
        multiple = TRUE
      ),
      plotOutput("pk_plot", height = "auto"),
      tableOutput("results")
    )
  )
)