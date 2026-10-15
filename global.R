# ============================================================
# Archivo: global.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Carga las bibliotecas y los archivos de funciones necesarios
# para el funcionamiento de la aplicación PKLaboratory.
# ============================================================

library(shiny)
library(mrgsolve)
library(ggplot2)

# Carga todos los archivos .R de una carpeta, en orden alfabético.
# Recibe: ruta de la carpeta.
source_dir <- function(dir) {
  for (file in sort(list.files(dir, pattern = "\\.[rR]$", full.names = TRUE))) {
    source(file)
  }
}

# Modelo: lógica de dominio, sin Shiny.
source_dir("R/model")

# Vista: páginas, formulario y gráficos.
source_dir("R/view")

# Controlador: reactividad que conecta la vista con el modelo.
source_dir("R/controller")
