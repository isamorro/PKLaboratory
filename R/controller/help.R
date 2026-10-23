# ============================================================
# Archivo: help.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador de la guía de uso: la descarga del CSV de ejemplo
# para el análisis poblacional.
# ============================================================

# Prepara la descarga del CSV de ejemplo: Theoph (dosis oral en mg/kg, con
# el peso), de los datos de R que usan Pinheiro y Bates.
# Recibe: salidas de Shiny.
help_controller <- function(output) {

  output$help_example_theoph <- shiny::downloadHandler(
    filename = function() "theoph.csv",
    content = function(file) {
      d <- as.data.frame(datasets::Theoph)
      d$Subject <- as.integer(as.character(d$Subject))
      d <- d[order(d$Subject, d$Time), c("Subject", "Wt", "Dose", "Time", "conc")]
      utils::write.csv(d, file, row.names = FALSE)
    }
  )
}
