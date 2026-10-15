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
  VC = "volumen del compartimento central",
  VP = "volumen del compartimento periférico",
  KA = "constante de absorción"
)

# Parámetros que no pueden ser negativos, pero sí 0.
# (Q = 0 significa "sin intercambio con el compartimento periférico";
#  algunos modelos, como emax o irm4, lo traen así por defecto.)
non_negative_params <- c(
  Q  = "aclaramiento intercompartimental",
  Q2 = "aclaramiento intercompartimental 2",
  Q3 = "aclaramiento intercompartimental 3"
)

# Comprueba que los parámetros cumplan las reglas de positive_params
# (mayores que 0) y de non_negative_params (no negativos).
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

  for (name in intersect(names(params), names(non_negative_params))) {
    value <- params[[name]]
    if (is_valid_number(value) && value < 0) {
      errors <- c(
        errors,
        sprintf("El parámetro %s (%s) no puede ser negativo.",
                name, non_negative_params[[name]])
      )
    }
  }

  errors
}
