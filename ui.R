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

ui <- bslib::page_navbar(
  id = "main_nav",
  title = shiny::span(
    class = "pk-brand",
    shiny::span(class = "pk-brand-mark", "PK"),
    shiny::span(class = "pk-brand-name", "PKLaboratory")
  ),
  window_title = "PKLaboratory",
  lang = "es",
  fillable = FALSE,

  theme = bslib::bs_theme(
    version = 5,
    primary = "#187A5B",
    secondary = "#64746D",
    bg = "#F7FAF8",
    fg = "#243D33",
    base_font = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Inter", local = FALSE)
  ),

  header = shiny::tags$link(rel = "stylesheet", href = "styles.css"),

  bslib::nav_panel(
    title = "Inicio",
    value = "home",
    home_ui()
  ),

  bslib::nav_panel(
    title = "Simulación",
    value = "simulation",
    simulation_ui()
  ),

  footer = shiny::tags$footer(
    class = "pk-footer",
    "PKLaboratory · Proyecto TFG · Simulaciones con mrgsolve"
  )
)
