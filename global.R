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

# Carga todos los archivos .R de una carpeta, en orden alfabético.
# Recibe: ruta de la carpeta.
source_dir <- function(dir) {
  for (file in sort(list.files(dir, pattern = "\\.[rR]$", full.names = TRUE))) {
    source(file)
  }
}

source_dir("R/model")
source_dir("R/view")
source_dir("R/controller")

# Base de datos.
# Guarda los usuarios, sus modelos y el catálogo compartido.
database_pool <- tryCatch(
  create_database_pool(),
  error = function(e) {
    message(
      "Base de datos no configurada. No se podrá iniciar sesión ni ",
      "guardar los modelos creados en la cuenta."
    )
    NULL
  }
)
if (!is.null(database_pool)) {
  tryCatch(
    ensure_user_models_schema(database_pool),
    error = function(e) {
      message("No se pudo preparar la tabla user_models: ", conditionMessage(e))
    }
  )
  tryCatch(
    ensure_auth_users_table(database_pool),
    error = function(e) {
      message("No se pudo preparar la tabla app_users: ", conditionMessage(e))
    }
  )
}

shiny::onStop(function() {
  if (!is.null(database_pool)) pool::poolClose(database_pool)
})
