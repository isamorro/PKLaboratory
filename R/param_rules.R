# ============================================================
# Archivo: param_rules.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las reglas sobre parámetros de los modelos predefinidos.
# ============================================================

# Parámetros que deben ser estrictamente positivos.
positive_params <- c(
  CL = "aclaramiento",
  V  = "volumen de distribución",
  V1 = "volumen del compartimento central",
  V2 = "volumen del compartimento 2",
  V3 = "volumen del compartimento 3",
  V4 = "volumen del compartimento 4",
  Q  = "aclaramiento intercompartimental",
  Q2 = "aclaramiento intercompartimental 2",
  Q3 = "aclaramiento intercompartimental 3",
  KA = "constante de absorción"
)

# Comprueba que los parámetros incluidos en positive_params sean positivos.
# Recibe: lista de parámetros con sus nombres y valores.
# Devuelve: vector de mensajes de error, vacío si no se detectan
# incumplimientos.
check_param_rules <- function(params) {

  errors <- character(0)

  for (name in intersect(names(params), names(positive_params))) {
    value <- params[[name]]

    if (is_valid_number(value) && value <= 0) {
      errors <- c(
        errors,
        sprintf(
          "El parámetro %s (%s) debe ser mayor que 0.",
          name, positive_params[[name]]
        )
      )
    }
  }

  errors
}