# ============================================================
# Archivo: validation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para validar los datos de entrada de
# las simulaciones y generar mensajes de error cuando el modelo,
# los parámetros o las condiciones de administración no son válidos.
# ============================================================

# Límite superior para el número de dosis.
max_doses <- 1000

# Comprueba si un valor es un único número finito.
# Recibe: valor que se desea comprobar.
# Devuelve: TRUE si es válido y FALSE en caso contrario.
is_valid_number <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x)
}

# Comprueba los datos de entrada antes de ejecutar la simulación.
# Recibe: parámetros, dosis, compartimento de administración, compartimentos
# del modelo, número de dosis, intervalo entre dosis, tiempo final, si se
# aplican las reglas de los parámetros y duración de la infusión.
# Devuelve: vector de mensajes de error, vacío si no se detectan errores.
validate_simulation <- function(params, dose, dose_cmt, model_cmts,
                                n_doses, interval, end_time,
                                apply_param_rules = TRUE, infusion = 0) {
  errors <- character(0)

  if (length(model_cmts) == 0) {
    if (!is_valid_number(dose) || dose < 0) {
      errors <- c(
        errors,
        paste(
          "La dosis debe ser un número mayor o igual que 0",
          "para modelos sin compartimentos."
        )
      )
    }
  } else if (!is_valid_number(dose) || dose <= 0) {
    errors <- c(errors, "La dosis debe ser un número mayor que 0.")
  }

  if (!is_valid_number(end_time) || end_time <= 0) {
    errors <- c(errors, "El tiempo final debe ser un número mayor que 0.")
  }

  valid_n <- is_valid_number(n_doses) && n_doses >= 1 &&
    n_doses == round(n_doses)
  if (!valid_n) {
    errors <- c(
      errors,
      "El número de dosis debe ser un entero mayor o igual que 1."
    )
  }

  if (valid_n && n_doses > max_doses) {
    errors <- c(
      errors,
      paste0("El número de dosis no puede superar ", max_doses, ".")
    )
  }

  if (valid_n && n_doses > 1) {
    if (!is_valid_number(interval) || interval <= 0) {
      errors <- c(
        errors,
        paste(
          "El intervalo entre dosis debe ser mayor que 0",
          "cuando hay más de una dosis."
        )
      )
    } else if (
      is_valid_number(end_time) &&
        (n_doses - 1) * interval > end_time
    ) {
      errors <- c(
        errors,
        sprintf(
          paste(
            "La última dosis se administra en t = %s,",
            "fuera del tiempo de simulación.",
            "Aumenta el tiempo final."
          ),
          format((n_doses - 1) * interval)
        )
      )
    }
  }

  # Duración de la infusión: 0 es un bolo. Con varias dosis, cada infusión
  # debe terminar antes de la siguiente.
  if (length(model_cmts) > 0) {
    if (!is_valid_number(infusion) || infusion < 0) {
      errors <- c(
        errors,
        "La duración de la infusión debe ser 0 (bolo) o un número positivo."
      )
    } else if (
      valid_n && n_doses > 1 && is_valid_number(interval) &&
        infusion > interval
    ) {
      errors <- c(
        errors,
        paste(
          "La infusión no puede durar más que el intervalo entre dosis:",
          "cada una debe terminar antes de la siguiente."
        )
      )
    }
  }

  if (length(model_cmts) > 0 && !isTRUE(dose_cmt %in% model_cmts)) {
    errors <- c(
      errors,
      "Selecciona el compartimento donde se administra la dosis."
    )
  }

  # Parámetros vacíos o no numéricos.
  invalid <- names(params)[!vapply(params, is_valid_number, logical(1))]
  if (length(invalid) > 0) {
    errors <- c(
      errors,
      paste0(
        "Estos parámetros están vacíos o no son números: ",
        paste(invalid, collapse = ", "), "."
      )
    )
  }

  # Reglas específicas de parámetros.
  c(
    errors,
    if (apply_param_rules) check_param_rules(params) else character()
  )
}
