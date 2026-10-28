# ============================================================
# Archivo: ui_population.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la función para construir la página de análisis poblacional,
# organizada en tres pasos:
#   1. Datos: un CSV con las medidas y las características de cada
#      paciente; se detectan las columnas que pueden ser covariables.
#   2. Análisis: qué parámetros cambian entre pacientes, covariables y
#      método.
#   3. Resultados: intervalos de confianza y desviaciones.
# La aplicación no valora si las elecciones son adecuadas: eso lo decide
# el usuario.
#
# Usa la disposición común de las páginas de trabajo (layout.R): cada paso
# tiene a la izquierda un panel con las opciones y a la derecha lo que se
# obtiene; el modelo y los pasos están en la barra fija de la página.
# ============================================================

# Construye la página de análisis poblacional.
# Devuelve: barra con el modelo y los pasos, y un panel oculto por paso.
population_ui <- function() {

  # Identificadores de los campos y condiciones de los conditionalPanel.
  id <- function(s) paste0("pop_nlme_", s)
  has_data <- sprintf("output['%s']", id("has_data"))
  data_ok <- sprintf("output['%s']", id("data_ok"))
  csv_ok <- sprintf("output['%s']", id("csv_ok"))
  has_fit <- sprintf("input['%s'] > 0", id("fit"))
  fit_ok <- sprintf("output['%s']", id("fit_ok"))
  has_individual <- sprintf("input['%s'] > 0", id("individual"))

  # Etiqueta de un paso del selector: número y texto.
  step_label <- function(n, text) {
    shiny::span(
      class = "pk-pop-step",
      shiny::span(class = "pk-pop-step-n", n),
      text
    )
  }

  page_container(
    class = "pk-builder-studio",

    page_bar(
      "Análisis poblacional",
      shiny::uiOutput(id("catalog_selector")),
      shiny::actionButton(
        id("model_code_view"), "Ver código",
        class = "btn-outline-primary btn-sm"
      ),
      shiny::uiOutput(id("model_details"), class = "pk-pop-modelbar-info"),
      shiny::uiOutput(id("model_description"),
                      class = "pk-pagebar-description"),
      right = shiny::div(
        class = "pk-pop-segmented pk-pop-stepper",
        shiny::radioButtons(
          id("stage"), NULL,
          inline = TRUE,
          choiceNames = list(
            step_label("1", "Datos"),
            step_label("2", "Análisis"),
            step_label("3", "Resultados")
          ),
          choiceValues = c("data", "analysis", "results")
        )
      )
    ),

    bslib::navset_hidden(
      id = id("tabs"),

      # ---- 1. Datos ---------------------------------------------------
      bslib::nav_panel_hidden(
        "data",
        page_workspace(
          page_panel(
            page_section(
              "Archivo",
              shiny::fileInput(
                id("file"), NULL,
                accept = c(".csv", ".txt"),
                buttonLabel = "Elegir…",
                placeholder = "Ningún archivo"
              )
            ),
            shiny::uiOutput(id("dataset_message")),
            # Si al CSV le falta algo que necesita el modelo, solo se deja
            # el archivo para subir otro.
            shiny::conditionalPanel(
              csv_ok,
              page_section(
                "Columnas de los datos",
                shiny::uiOutput(id("variables")),
                shiny::uiOutput(id("dose_inputs")),
                shiny::uiOutput(id("conversion_message"))
              )
            ),
            footer = shiny::conditionalPanel(
              data_ok,
              shiny::actionButton(
                id("go_analysis"), "Continuar con el análisis →",
                class = "btn-primary w-100"
              )
            )
          ),
          shiny::conditionalPanel(
            paste0("!", has_data),
            page_empty(
              "Aún no hay datos",
              paste(
                "Sube un CSV para ver aquí las curvas de cada paciente, las",
                "posibles covariables y sus primeras filas."
              )
            )
          ),
          shiny::conditionalPanel(
            has_data,
            shiny::uiOutput(id("data_check"))
          ),
          shiny::conditionalPanel(
            csv_ok,
            shiny::uiOutput(id("data_summary")),
            page_card(
              "Posibles covariables",
              shiny::uiOutput(id("covariates"))
            ),
            shiny::div(
              class = "pk-pop-duo",
              page_card(
                "Concentraciones observadas",
                shiny::plotOutput(id("data_plot"), height = "330px")
              ),
              page_card(
                "Vista previa de los datos",
                shiny::div(
                  class = "pk-pop-table pk-pop-scroll",
                  shiny::tableOutput(id("data_preview"))
                )
              )
            )
          )
        )
      ),

      # ---- 2. Análisis ------------------------------------------------
      bslib::nav_panel_hidden(
        "analysis",
        shiny::conditionalPanel(
          paste0("!", data_ok),
          page_empty(
            "Primero necesitas datos",
            paste(
              "Vuelve al paso 1 y sube un CSV con los datos que necesita el",
              "modelo elegido."
            ),
            shiny::actionButton(
              id("back_data"), "← Ir a los datos",
              class = "btn-outline-primary"
            )
          )
        ),
        shiny::conditionalPanel(
          data_ok,
          page_workspace(
            # Primero los ajustes individuales, en su propio recuadro, y
            # debajo la elección del modelo mixto.
            shiny::div(
              class = "pk-pop-sidestack",
              page_panel(
                page_section(
                  "Ajustes individuales",
                  shiny::uiOutput(id("individual_params"))
                ),
                footer = shiny::actionButton(
                  id("individual"), "Ajustar cada paciente",
                  class = "btn-outline-primary w-100"
                )
              ),
              page_panel(
                shiny::uiOutput(id("scales")),
                shiny::uiOutput(id("fit_options")),
                footer = shiny::tagList(
                  shiny::actionButton(
                    id("fit"), "Estimar parámetros →",
                    class = "btn-primary w-100"
                  ),
                  shiny::p(
                    class = "pk-pop-panel-hint",
                    "La estimación puede tardar de segundos a varios minutos."
                  )
                )
              )
            ),
            shiny::conditionalPanel(
              paste0("!", has_individual),
              page_card(
                "Cómo seguir",
                shiny::tags$ol(
                  class = "pk-pop-guide",
                  shiny::tags$li(
                    shiny::strong("Ajusta cada paciente por separado"),
                    " con nlsList() de nlme, sin efectos aleatorios ",
                    "(«Ajustar cada paciente»). ",
                  ),
                  shiny::tags$li(
                    shiny::strong("Usa la mediana de esas estimaciones"),
                    " como valores iniciales del modelo mixto, como hacen ",
                    "Pinheiro y Bates."
                  ),
                  shiny::tags$li(
                    shiny::strong("Elige el modelo mixto:"),
                    " efectos aleatorios, matriz Ψ, covariables y error de ",
                    "medida, y pulsa «Estimar parámetros»."
                  )
                )
              )
            ),
            shiny::conditionalPanel(
              has_individual,
              page_card(
                "Estimaciones individuales (nlsList)",
                shiny::uiOutput(id("individual_status")),
                shiny::plotOutput(id("individual_plot"), height = "auto"),
                shiny::tags$details(
                  class = "pk-pop-details",
                  shiny::tags$summary(
                    "Tabla con errores estándar y p-valores"
                  ),
                  shiny::div(
                    class = "pk-pop-details-body",
                    shiny::p(
                      class = "pk-component-note",
                      paste(
                        "Intervalos como intervals() de nlsList (con la",
                        "desviación residual conjunta de todos los",
                        "pacientes). El p-valor contrasta H0: β = 0, es",
                        "decir, que el parámetro valga 1 si está en escala",
                        "log o 0 si está en escala natural."
                      )
                    ),
                    shiny::div(
                      class = "pk-pop-table pk-pop-scroll",
                      shiny::tableOutput(id("individual_table"))
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # ---- 3. Resultados ----------------------------------------------
      bslib::nav_panel_hidden(
        "results",
        shiny::conditionalPanel(
          paste0("!", has_fit),
          page_empty(
            "Aún no hay resultados",
            "Configura el análisis en el paso 2 y pulsa «Estimar parámetros».",
            shiny::actionButton(
              id("back_analysis_empty"), "← Ir al análisis",
              class = "btn-outline-primary"
            )
          )
        ),
        shiny::conditionalPanel(
          has_fit,
          shiny::div(
            class = "pk-pop-resultbar",
            shiny::uiOutput(id("fit_status"), class = "pk-pop-resultbar-stats"),
            shiny::div(
              class = "pk-pop-resultbar-actions",
              shiny::actionButton(
                id("back_analysis"), "← Volver al análisis",
                class = "btn-sm btn-outline-primary"
              ),
              # Solo si el ajuste ha terminado bien.
              shiny::conditionalPanel(
                fit_ok,
                shiny::downloadButton(
                  id("fit_download"), "Descargar resultados (CSV)",
                  class = "btn-sm pk-download pk-download-indigo"
                )
              )
            )
          ),
          # Resultados, uno debajo de otro y a todo el ancho.
          shiny::conditionalPanel(
            fit_ok,
            shiny::div(
              class = "pk-pop-results",
              page_card(
                "Efectos fijos",
                shiny::div(
                  class = "pk-pop-table",
                  shiny::tableOutput(id("fit_fixed"))
                )
              ),
              page_card(
                "Variabilidad",
                shiny::div(
                  class = "pk-pop-table",
                  shiny::tableOutput(id("fit_variability"))
                )
              ),
              page_card(
                "Estimaciones de cada paciente",
                shiny::div(
                  class = "pk-pop-table pk-pop-scroll",
                  shiny::tableOutput(id("fit_individual"))
                )
              ),
              page_card(
                "Resumen (summary de nlme)",
                header_extra = shiny::downloadButton(
                  id("fit_summary_download"), "TXT",
                  class = "btn-sm pk-download pk-download-indigo"
                ),
                shiny::verbatimTextOutput(
                  id("fit_summary"), placeholder = FALSE
                )
              )
            )
          ),
          # Si el ajuste falla, el registro del proceso.
          shiny::conditionalPanel(
            paste0("!", fit_ok),
            shiny::div(
              class = "pk-pop-results",
              page_card(
                "Registro del ajuste",
                header_extra = shiny::downloadButton(
                  id("fit_log_download"), "TXT",
                  class = "btn-sm pk-download pk-download-indigo"
                ),
                shiny::verbatimTextOutput(id("fit_log"), placeholder = FALSE)
              )
            )
          ),
          shiny::p(
            class = "pk-component-note pk-pop-disclaimer",
            "La aplicación no valora si lo elegido es adecuado: interpreta ",
            "los resultados según tu criterio."
          )
        )
      )
    )
  )
}
