# ============================================================
# Archivo: a_component_form.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la lectura de los formularios de modelos: el del modelo (nombre,
# descripción y código de mrgsolve) y el del asistente por componentes, que
# convierte cadenas tipo "nombre = valor" en listas de parámetros,
# condiciones iniciales, ecuaciones y variables de salida. También define
# cómo se muestran los errores de los formularios.
#
# El prefijo "a_" hace que este archivo se cargue antes que los demás
# controladores, dejando definidas las funciones comunes de los
# formularios que utilizan el constructor, el editor de modelos y la
# simulación.
# ============================================================

# Limpia el valor de un campo de texto.
# Recibe: valor de un campo de texto (puede ser NULL).
# Devuelve: el texto sin espacios sobrantes, o "" si no hay valor.
clean_text <- function(x) {
  if (is.null(x)) "" else trimws(x)
}

# Caja roja con los errores de un formulario.
# Recibe: vector de mensajes.
# Devuelve: alerta de Bootstrap, o NULL si no hay mensajes.
error_box <- function(messages) {
  if (length(messages) == 0) {
    return(NULL)
  }
  div(class = "alert alert-danger", role = "alert", lapply(messages, div))
}

# Crea la función que informa de los errores de un formulario: los muestra
# en su caja roja y en una notificación, que se ve aunque la caja haya
# quedado fuera de la vista. Los errores de validación y de compilación
# llegan con un mensaje por línea, así que se separan aquí.
# Recibe: reactiveVal con los mensajes de la caja y título del aviso.
# Devuelve: función que recibe un texto o un vector de mensajes.
error_reporter <- function(box, title) {
  function(messages) {
    messages <- unlist(strsplit(messages, "\n", fixed = TRUE))
    box(messages)
    showNotification(
      tagList(strong(title), lapply(utils::head(messages, 5L), div)),
      type = "error",
      duration = 8
    )
  }
}

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

# Lee el formulario del asistente por componentes y normaliza la salida.
# Recibe: objeto input de Shiny y prefijo del formulario.
# Devuelve: lista con parámetros, condiciones iniciales y ecuaciones.
read_component_form <- function(input, prefix) {
  get <- function(n) input[[paste0(prefix, n)]]

  outputs <- trimws(strsplit(
    if (is.null(get("outputs"))) "" else get("outputs"),
    ",",
    fixed = TRUE
  )[[1]])

  list(
    params = read_component_pairs(get("params"), TRUE),
    initials = read_component_pairs(get("initials"), TRUE),
    auxiliary = read_component_pairs(get("auxiliary")),
    equations = read_component_pairs(get("equations")),
    outputs = outputs[nzchar(outputs)]
  )
}

# Lee el formulario de un modelo: nombre, descripción y código de mrgsolve.
# Recibe: objeto input de Shiny y prefijo del formulario.
# Devuelve: especificación del modelo.
read_model_form <- function(input, prefix) {
  code <- input[[paste0(prefix, "code_text")]]
  list(
    name = clean_text(input[[paste0(prefix, "name")]]),
    description = clean_text(input[[paste0(prefix, "description")]]),
    code = if (is.null(code)) "" else gsub("\r\n?", "\n", code)
  )
}
