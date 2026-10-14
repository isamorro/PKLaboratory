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

source("R/catalog.R")
source("R/param_rules.R")
source("R/validation.R")
source("R/errors.R")
source("R/simulation.R")
source("R/form.R")
source("R/plots.R")