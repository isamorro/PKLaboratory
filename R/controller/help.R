# ============================================================
# Archivo: help.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador de la guía de uso: las descargas de los CSV de
# ejemplo para el análisis poblacional.
# ============================================================

# Prepara las descargas de los CSV de ejemplo: Theoph (dosis oral en
# mg/kg, con el peso) e Indometh (25 mg intravenosos, con una columna de
# dosis añadida), de los datos de R que usan Pinheiro y Bates.
# Recibe: salidas de Shiny.
help_controller <- function(output) {

  # Descarga de un data.frame como CSV.
  # Recibe: nombre del archivo y función que devuelve los datos.
  example_csv <- function(filename, data) {
    shiny::downloadHandler(
      filename = function() filename,
      content = function(file) {
        utils::write.csv(data(), file, row.names = FALSE)
      }
    )
  }

  output$help_example_theoph <- example_csv("theoph.csv", function() {
    d <- as.data.frame(datasets::Theoph)
    d$Subject <- as.integer(as.character(d$Subject))
    d[order(d$Subject, d$Time), c("Subject", "Wt", "Dose", "Time", "conc")]
  })

  output$help_example_indometh <- example_csv("indometh.csv", function() {
    d <- as.data.frame(datasets::Indometh)
    d$Subject <- as.integer(as.character(d$Subject))
    d$Dose <- 25
    d[order(d$Subject, d$time), c("Subject", "Dose", "time", "conc")]
  })
}
