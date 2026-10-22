# ============================================================
# Archivo: navigation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador que gestiona la navegación entre las
# páginas de la aplicación.
# ============================================================

# Gestiona la navegación entre las páginas de la aplicación.
# Recibe: entradas y sesión de Shiny.
navigation_controller <- function(input, session) {

  # Tarjetas de Inicio (cada una abre su sección) y botón de Mis modelos
  # para crear uno nuevo.
  sections <- c(
    home_open_simulation = "simulation",
    home_open_builder = "builder",
    home_open_population = "population",
    home_open_models = "models",
    models_new = "builder"
  )
  for (name in names(sections)) local({
    link <- name
    target <- sections[[link]]
    observeEvent(input[[link]], {
      updateNavbarPage(session, "main_nav", selected = target)
    })
  })
}
