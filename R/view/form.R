# ============================================================
# Archivo: form.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para construir y leer el formulario dinámico
# de parámetros de los modelos farmacocinéticos.
# ============================================================

# Prefijo utilizado para identificar los campos de los parámetros.
param_input_prefix <- "param_"

# Genera los campos numéricos para introducir los parámetros del modelo.
# Recibe: lista de parámetros con sus nombres y valores por defecto.
# Devuelve: conjunto de elementos de la interfaz de Shiny.
build_param_inputs <- function(params) {
  shiny::div(
    class = "pk-param-grid",
    lapply(names(params), function(name) {
      shiny::numericInput(
        inputId = paste0(param_input_prefix, name),
        label = name,
        value = params[[name]],
        width = "100%"
      )
    })
  )
}

# Obtiene los valores actuales de los parámetros introducidos por el usuario.
# Recibe: entradas de Shiny y vector con los nombres de los parámetros.
# Devuelve: lista de valores con los nombres de los parámetros.
read_param_inputs <- function(input, param_names) {
  stats::setNames(
    lapply(param_names, function(name) {
      input[[paste0(param_input_prefix, name)]]
    }),
    param_names
  )
}
