# ============================================================
# Archivo: component_form.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la lectura y validación del bloque de formulario de un componente.
# Se encarga de convertir cadenas tipo "nombre = valor" en listas de
# parámetros, condiciones iniciales, ecuaciones y salidas de salida,
# manteniendo el mismo patrón de estilo que el resto de controladores.
#
# El prefijo "a_" hace que este archivo se cargue antes que los demás
# controladores, dejando definidas las funciones comunes de lectura
# del formulario que utilizan el constructor y el editor de modelos.
# ============================================================

# Lee filas nombre = valor o nombre = expresión sin evaluar el contenido.
# Recibe: texto con varias líneas y si los valores deben convertirse a número.
# Devuelve: lista con claves y valores del formulario.
read_component_pairs <- function(text, numeric_values = FALSE) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list())
  }
  if (nchar(text) > 60000L) {
    stop("El formulario es demasiado largo.", call. = FALSE)
  }

  rows <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  rows <- rows[nzchar(rows)]
  if (length(rows) > 50L) {
    stop("Máximo 50 filas por sección.", call. = FALSE)
  }

  result <- list()
  for (row in rows) {
    # Solo el primer '=' separa nombre y expresión; admite ==, <=, >= dentro.
    at <- regexpr("=", row, fixed = TRUE)[1]
    if (at < 2L) {
      stop("Cada fila debe tener la forma nombre = valor o expresión.",
           call. = FALSE)
    }

    n <- trimws(substr(row, 1, at - 1L))
    value <- trimws(substr(row, at + 1L, nchar(row)))
    if (!nzchar(value) || n %in% names(result)) {
      stop("Valor vacío o nombre repetido en una sección.", call. = FALSE)
    }

    if (numeric_values) {
      value <- suppressWarnings(as.numeric(value))
      if (!is_valid_number(value)) {
        stop(
          "Los valores iniciales deben ser números finitos con punto decimal.",
          call. = FALSE
        )
      }
    }

    result[[n]] <- value
  }
  result
}

# Lee un bloque del formulario de un componente y normaliza la salida.
# Recibe: objeto input de Shiny y prefijo del bloque actual.
# Devuelve: lista con parámetros, condiciones iniciales y ecuaciones.
read_component_form <- function(input, prefix) {
  get <- function(n) input[[paste0(prefix, n)]]

  outputs <- trimws(strsplit(
    if (is.null(get("outputs"))) "" else get("outputs"),
    ",",
    fixed = TRUE
  )[[1]])

  list(
    name = clean_text(get("name")),
    description = clean_text(get("description")),
    params = read_component_pairs(get("params"), TRUE),
    initials = read_component_pairs(get("initials"), TRUE),
    auxiliary = read_component_pairs(get("auxiliary")),
    equations = read_component_pairs(get("equations")),
    outputs = outputs[nzchar(outputs)]
  )
}
