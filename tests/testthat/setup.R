# ============================================================
# Archivo: setup.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Prepara el entorno de las pruebas: carga las bibliotecas y el código de
# la aplicación igual que global.R, pero sin conectarse a la base de datos.
# testthat lo ejecuta antes que los archivos test-*.R.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(mrgsolve)
})

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

# Archivos de la aplicación, en el orden en que los carga global.R.
# Devuelve: rutas de los archivos.
app_files <- function(root = app_root) {
  files <- unlist(lapply(c("R/model", "R/view", "R/controller"), function(d) {
    sort(list.files(file.path(root, d), pattern = "\\.[rR]$",
                    full.names = TRUE))
  }))
  c(files, file.path(root, "server.R"))
}

# tests/run_tests.R ya los carga antes para medir la cobertura; volver a
# cargarlos aquí borraría esa medición.
if (!isTRUE(getOption("pklab.app_loaded"))) {
  for (file in app_files()) source(file, local = FALSE)
}

# Sin base de datos: la aplicación funciona igual que cuando no está
# configurada (ver global.R). Van al entorno global, donde los busca
# server(), igual que con global.R.
assign("database_pool", NULL, envir = globalenv())
assign("help_page", help_ui(), envir = globalenv())

# Parámetros por defecto de un modelo de la biblioteca.
# Recibe: identificador del modelo.
# Devuelve: lista con los parámetros numéricos.
default_params <- function(model_id) {
  get_model_params(load_model(model_id))
}
