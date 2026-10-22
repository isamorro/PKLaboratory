# ============================================================
# Archivo: ui.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la interfaz de usuario de PKLaboratory y organiza sus
# páginas de navegación. El contenido de cada página está en R/view/.
# ============================================================

# Tema de la aplicación.
pk_theme <- bslib::bs_theme(
  version = 5,
  primary = "#187A5B",
  secondary = "#64746D",
  bg = "#F7FAF8",
  fg = "#243D33",
  base_font = bslib::font_google("Inter", local = FALSE),
  heading_font = bslib::font_google("Inter", local = FALSE)
)

# Interfaz de usuario de la aplicación principal.
# Recibe: opciones del selector de modelos y modelo seleccionado, para que
# los modelos del usuario ya estén en el selector desde el primer momento.
# Devuelve: barra de navegación con las páginas de la aplicación.
main_app_ui <- function(model_choices = list_models(),
                        selected_model = default_model_id) {
  bslib::page_navbar(
    id = "main_nav",
    title = shiny::span(
      class = "pk-brand",
      shiny::span(class = "pk-brand-mark", "PK"),
      shiny::span(class = "pk-brand-name", "PKLaboratory")
    ),
    window_title = "PKLaboratory",
    lang = "es",
    fillable = FALSE,

    theme = pk_theme,

    header = NULL,

    bslib::nav_panel(
      title = "Inicio",
      value = "home",
      home_ui()
    ),

    bslib::nav_panel(
      title = "Simulación",
      value = "simulation",
      simulation_ui(model_choices, selected_model)
    ),

    bslib::nav_panel(
      title = "Crear modelo",
      value = "builder",
      builder_ui()
    ),

    bslib::nav_panel(
      title = "Análisis poblacional",
      value = "population",
      population_ui()
    ),

    bslib::nav_panel(
      title = "Mis modelos",
      value = "models",
      models_ui()
    ),

    bslib::nav_panel(
      title = "Guía de uso",
      value = "help",
      help_ui()
    ),

    bslib::nav_spacer(),

    bslib::nav_item(
      shiny::actionButton(
        "logout",
        "Cerrar sesión",
        class = "btn btn-primary btn-sm pk-cta"
      )
    ),

    footer = shiny::tags$footer(
      class = "pk-footer",
      "PKLaboratory · Proyecto TFG"
    )
  )
}

# Interfaz de usuario de la aplicación principal, con login.
# Devuelve: página de navegación con las páginas de la aplicación.
ui <- shiny::fluidPage(
  theme = pk_theme,
  title = "PKLaboratory",
  lang = "es",
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "styles.css"),
    shiny::tags$script(src = "intro.js", defer = NA)
  ),
  # Fuera de app_page para no repetir la animación al entrar o salir.
  shiny::div(
    id = "pk-intro",
    class = "pk-intro",
    shiny::div(class = "pk-intro-panel pk-intro-top", `aria-hidden` = "true"),
    shiny::div(
      class = "pk-intro-panel pk-intro-bottom",
      `aria-hidden` = "true"
    ),
    shiny::div(
      class = "pk-intro-word",
      `aria-label` = "PKLaboratory",
      lapply(seq_along(strsplit("PKLaboratory", "")[[1]]), function(i) {
        shiny::span(
          class = "pk-intro-letter",
          `aria-hidden` = "true",
          style = paste0("--pk-letter-delay:", (i - 1) * 65, "ms"),
          strsplit("PKLaboratory", "")[[1]][i]
        )
      })
    )
  ),
  shiny::uiOutput("app_page")
)
