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

  # Botón de la página de inicio: lleva a la pestaña de simulación.
  observeEvent(input$go_to_simulation, {
    updateNavbarPage(session, "main_nav", selected = "simulation")
  })
  # Acceso desde Inicio al constructor de modelos.
  observeEvent(input$go_to_builder, {
    updateNavbarPage(session, "main_nav", selected = "builder")
  })
}
