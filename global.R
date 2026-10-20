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

# Base de datos: un único pool para toda la aplicación (no uno por sesión).
# Guarda los modelos de los usuarios y el catálogo compartido.
database_pool <- tryCatch(
  create_database_pool(),
  error = function(e) {
    message(
      "Base de datos de modelos no configurada. ",
      "La biblioteca sigue disponible, pero los modelos creados ",
      "no se guardarán en la cuenta."
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
}

# Autenticación: usa el mismo pool de PostgreSQL que el catálogo.
auth_pool <- tryCatch(
  create_auth_database_pool(),
  error = function(e) {
    message(
      "Base de datos de autenticación no configurada. ",
      "La aplicación no podrá iniciar sesión con usuarios.",
      call. = FALSE
    )
    NULL
  }
)
if (!is.null(auth_pool)) {
  tryCatch(
    ensure_auth_users_table(auth_pool),
    error = function(e) {
      message("No se pudo preparar la tabla app_users: ", conditionMessage(e))
    }
  )
}

shiny::onStop(function() {
  if (!is.null(auth_pool) && !identical(auth_pool, database_pool)) {
    pool::poolClose(auth_pool)
  }
  if (!is.null(database_pool)) pool::poolClose(database_pool)
})
