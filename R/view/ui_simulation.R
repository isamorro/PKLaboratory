# ============================================================
# Archivo: ui_simulation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la función para construir la página de simulación, con el
# formulario de configuración y la zona de resultados.
# ============================================================

# Construye la página de simulación.
# Devuelve: formulario lateral y tarjetas con los resultados.
simulation_ui <- function() {
  shiny::div(
    class = "pk-sim",

    bslib::layout_sidebar(
      fillable = FALSE,

      sidebar = bslib::sidebar(
        title = "Configuración",
        width = 340,
        class = "pk-sidebar",

        shiny::div(
          class = "pk-form-section",
          shiny::selectInput(
            "model_id",
            "Modelo",
            choices = list_models(),
            selected = "pk1"
          )
        ),

        shiny::div(
          class = "pk-form-section",
          bslib::accordion(
            id = "params_accordion",
            class = "pk-params",
            open = "params",
            bslib::accordion_panel(
              title = "Parámetros del modelo",
              value = "params",
              shiny::uiOutput("param_inputs")
            )
          )
        ),

        shiny::div(
          class = "pk-form-section",
          shiny::h4("Administración"),

          shiny::selectInput(
            "dose_cmt",
            "Compartimento de administración",
            choices = NULL
          ),

          shiny::numericInput(
            "dose", "Dosis",
            value = 100, min = 0
          ),

          shiny::numericInput(
            "n_doses", "Número de dosis",
            value = 10, min = 1, step = 1
          ),

          shiny::numericInput(
            "interval",
            "Intervalo entre dosis (si hay más de una)",
            value = 24, min = 0
          ),

          shiny::numericInput(
            "end_time", "Tiempo final",
            value = 480, min = 0
          )
        ),

        shiny::actionButton(
          "simulate",
          "Simular",
          class = "btn-primary w-100 pk-simulate"
        )
      ),

      shiny::uiOutput("sim_errors"),

      # Mensaje inicial, visible hasta la primera simulación.
      shiny::conditionalPanel(
        condition = "input.simulate == 0",
        shiny::div(
          class = "pk-empty",
          shiny::div(class = "pk-empty-icon", "PK"),
          shiny::h3("Aún no hay resultados"),
          shiny::p(
            "Elige un modelo, ajusta la administración en el panel",
            "lateral y pulsa Simular para ver la evolución temporal."
          )
        )
      ),

      bslib::card(
        class = "pk-card",
        fill = FALSE,
        bslib::card_header(
          class = "pk-card-header",
          shiny::span("Evolución temporal"),
          shiny::conditionalPanel(
            condition = "input.simulate > 0",
            shiny::downloadButton(
              "download_plot", "Gráfico PNG",
              class = "btn-sm pk-download pk-download-indigo"
            )
          )
        ),

        shiny::selectInput(
          "plot_vars",
          "Variables a representar",
          choices = NULL,
          multiple = TRUE
        ),

        shiny::plotOutput("pk_plot", height = "auto")
      ),

      bslib::card(
        class = "pk-card",
        fill = FALSE,
        bslib::card_header(
          class = "pk-card-header",
          shiny::span("Resultados de la simulación"),
          shiny::conditionalPanel(
            condition = "input.simulate > 0",
            shiny::div(
              class = "pk-download-group",
              shiny::downloadButton(
                "download_csv", "CSV",
                class = "btn-sm pk-download pk-download-amber"
              ),
              shiny::downloadButton(
                "download_csv_excel", "CSV para Excel",
                class = "btn-sm pk-download pk-download-amber"
              )
            )
          )
        ),
        shiny::div(
          class = "pk-table-wrap",
          shiny::tableOutput("results")
        ),
        shiny::p(
          class = "pk-table-note",
          "Se muestran las 10 primeras filas; la descarga incluye",
          "todos los resultados."
        )
      )
    )
  )
}
