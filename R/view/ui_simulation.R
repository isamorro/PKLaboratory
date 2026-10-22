# ============================================================
# Archivo: ui_simulation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la función para construir la página de simulación, con la
# disposición común de las páginas de trabajo (layout.R): el modelo en la
# barra fija, a la izquierda un panel con los parámetros y la
# administración (con el botón Simular siempre a la vista) y a la derecha
# la gráfica y la tabla.
# ============================================================

# Construye la página de simulación.
# Recibe: opciones del selector de modelos y modelo seleccionado.
# Devuelve: barra, panel de configuración y tarjetas con los resultados.
simulation_ui <- function(model_choices = list_models(),
                          selected_model = default_model_id) {
  page_container(
    class = "pk-builder-studio",

    page_bar(
      "Simulación",
      shiny::div(
        class = "pk-pagebar-select",
        shiny::selectInput(
          "model_id",
          label = "Modelo",
          choices = model_choices,
          selected = selected_model
        )
      )
    ),

    page_workspace(
      page_panel(
        page_section(
          "Parámetros",
          shiny::uiOutput("param_inputs")
        ),

        page_section(
          "Administración",
          shiny::selectInput(
            "dose_cmt",
            "Compartimento de administración",
            choices = NULL
          ),
          shiny::div(
            class = "pk-pop-inline",
            shiny::numericInput(
              "dose", "Dosis",
              value = 100, min = 0
            ),
            shiny::numericInput(
              "n_doses", "Número de dosis",
              value = 10, min = 1, step = 1
            )
          ),
          shiny::div(
            class = "pk-pop-inline",
            shiny::numericInput(
              "interval",
              "Intervalo entre dosis",
              value = 24, min = 0
            ),
            shiny::numericInput(
              "end_time", "Tiempo final",
              value = 480, min = 0
            )
          ),
          shiny::numericInput(
            "infusion", "Duración de la infusión (0 = bolo)",
            value = 0, min = 0
          )
        ),

        footer = shiny::actionButton(
          "simulate",
          "Simular",
          class = "btn-primary w-100"
        )
      ),

      shiny::uiOutput("sim_errors"),

      # Mensaje inicial, visible hasta la primera simulación.
      shiny::conditionalPanel(
        condition = "input.simulate == 0",
        page_empty(
          "Aún no hay resultados",
          paste(
            "Elige un modelo, ajusta sus parámetros y la administración",
            "en el panel de la izquierda y pulsa Simular para ver la",
            "evolución temporal."
          )
        )
      ),

      shiny::conditionalPanel(
        condition = "input.simulate > 0",

        page_card(
          "Evolución temporal",
          header_extra = shiny::downloadButton(
            "download_plot", "Gráfico PNG",
            class = "btn-sm pk-download pk-download-indigo"
          ),
          shiny::selectInput(
            "plot_vars",
            "Variables a representar",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          ),
          shiny::plotOutput("pk_plot", height = "auto")
        ),

        page_card(
          "Resultados de la simulación",
          header_extra = shiny::div(
            class = "pk-download-group",
            shiny::downloadButton(
              "download_csv", "CSV",
              class = "btn-sm pk-download pk-download-amber"
            ),
            shiny::downloadButton(
              "download_csv_excel", "CSV para Excel",
              class = "btn-sm pk-download pk-download-amber"
            )
          ),
          shiny::div(
            class = "pk-pop-table pk-pop-scroll",
            shiny::tableOutput("results")
          ),
          shiny::p(
            class = "pk-component-note",
            "Se muestran las 150 primeras filas; la descarga incluye",
            "todos los resultados."
          )
        )
      )
    )
  )
}
