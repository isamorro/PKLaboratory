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
# Recibe: lista de parámetros con sus nombres y valores por defecto, prefijo
# de los identificadores de los campos y, opcionalmente, vector con las
# etiquetas a mostrar (por defecto, el nombre del parámetro).
# Devuelve: conjunto de elementos de la interfaz de Shiny.
build_param_inputs <- function(params, prefix = param_input_prefix,
                               labels = NULL) {
  shiny::div(
    class = "pk-param-grid",
    lapply(names(params), function(name) {
      shiny::numericInput(
        inputId = paste0(prefix, name),
        label = if (is.null(labels)) name else labels[[name]],
        value = params[[name]],
        width = "100%"
      )
    })
  )
}

# Obtiene los valores actuales de los parámetros introducidos por el usuario.
# Recibe: entradas de Shiny, vector con los nombres de los parámetros y
# prefijo de los identificadores de los campos.
# Devuelve: lista de valores con los nombres de los parámetros.
read_param_inputs <- function(input, param_names,
                              prefix = param_input_prefix) {
  stats::setNames(
    lapply(param_names, function(name) {
      input[[paste0(prefix, name)]]
    }),
    param_names
  )
}
