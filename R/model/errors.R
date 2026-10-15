# ============================================================
# Archivo: errors.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para traducir los errores de mrgsolve a
# mensajes en español para el usuario.
# ============================================================

# Mensaje para los modelos que no tienen compartimentos.
no_compartments_error <- paste(
  "Este modelo no tiene compartimentos,",
  "por lo que no admite administración de dosis."
)

# Patrones de errores conocidos y mensajes correspondientes en español.
error_patterns <- list(
  list(
    pattern = paste0(
      "pred_([A-Za-z0-9_]+)\\s+has\\s+a\\s+0\\s+or\\s+",
      "negative\\s+value"
    ),
    message = function(m) {
      sprintf("El parámetro %s debe ser mayor que 0 en este modelo.", m[2])
    }
  ),
  list(
    pattern = "missing value in cmt",
    message = function(m) no_compartments_error
  )
)

# Mensaje para los errores que no coinciden con ningún patrón conocido.
generic_simulation_error <- paste(
  "La simulación no se pudo completar con los valores indicados.",
  "Revisa los parámetros, la dosis y el tiempo final."
)

# Convierte un error de simulación en un mensaje en español para el usuario.
# El mensaje original solo se escribe en la consola, para depuración.
# Recibe: objeto de error.
# Devuelve: mensaje específico si reconoce el error o un mensaje genérico.
translate_simulation_error <- function(e) {

  raw <- conditionMessage(e)
  message("[mrgsolve] ", raw)

  for (rule in error_patterns) {
    match <- regmatches(raw, regexec(rule$pattern, raw))[[1]]
    if (length(match) > 0) {
      return(rule$message(match))
    }
  }

  generic_simulation_error
}
