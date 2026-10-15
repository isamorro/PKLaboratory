# ============================================================
# Archivo: server.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la lógica del servidor y gestiona el funcionamiento
# reactivo de la aplicación PKLaboratory.
# ============================================================

# Gestiona la lógica reactiva y conecta las entradas del usuario
# con las simulaciones y la presentación de los resultados.
# Recibe: entradas, salidas y sesión de Shiny.
server <- function(input, output, session) {

  navigation_controller(input, session)

  model      <- model_controller(input, output, session)
  simulation <- simulation_controller(input, output, model)

  results_controller(input, output, session, simulation)
}
