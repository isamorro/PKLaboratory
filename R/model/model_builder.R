# ============================================================
# Archivo: model_builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para crear modelos propios: validar la
# especificación que elige el usuario, generar el código de mrgsolve
# a partir de ella y compilarlo. El usuario nunca escribe código: solo
# elige opciones de listas cerradas y valores numéricos, de modo que
# el código que se compila lo genera siempre la aplicación.
# ============================================================

# Opciones de estructura del modelo (etiqueta = valor).
absorption_options <- c(
  "Intravenosa (bolo)" = "iv",
  "Oral (primer orden)" = "oral"
)

ncmt_options <- c(
  "1 compartimento" = "1",
  "2 compartimentos" = "2"
)

elimination_options <- c(
  "Lineal" = "linear",
  "Michaelis-Menten (saturable)" = "mm"
)

# Parámetros que puede tener un modelo: descripción corta y valor inicial.
builder_params <- list(
  CL   = list(short = "aclaramiento", default = 5),
  VMAX = list(short = "velocidad máxima", default = 50),
  KM   = list(short = "constante de M-M", default = 10),
  V    = list(short = "volumen central", default = 50),
  Q    = list(short = "aclar. intercomp.", default = 10),
  VP   = list(short = "volumen periférico", default = 100),
  KA   = list(short = "absorción", default = 1)
)

# Límites de los datos que introduce el usuario.
max_param_value <- 1e6
max_name_length <- 30
max_description_length <- 200

# Un nombre empieza por una letra y solo admite letras sin tilde, números,
# espacios, guiones y guiones bajos.
name_pattern <- "^[A-Za-z][A-Za-z0-9 _-]*$"

# Obtiene los parámetros que necesita un modelo según su estructura.
# Recibe: especificación (con absorption, ncmt y elimination).
# Devuelve: vector con los nombres de los parámetros.
builder_param_names <- function(spec) {
  c(
    if (spec$elimination == "mm") c("VMAX", "KM") else "CL",
    "V",
    if (as.integer(spec$ncmt) == 2L) c("Q", "VP"),
    if (spec$absorption == "oral") "KA"
  )
}

# Obtiene los valores iniciales de unos parámetros.
# Recibe: vector con los nombres de los parámetros.
# Devuelve: lista con los nombres y valores iniciales.
builder_default_params <- function(param_names) {
  stats::setNames(
    lapply(param_names, function(name) builder_params[[name]]$default),
    param_names
  )
}

# Obtiene las etiquetas para mostrar los parámetros en el formulario.
# Recibe: vector con los nombres de los parámetros.
# Devuelve: vector con las etiquetas, con los nombres como nombres.
builder_param_labels <- function(param_names) {
  stats::setNames(
    vapply(
      param_names,
      function(name) paste0(name, " (", builder_params[[name]]$short, ")"),
      character(1)
    ),
    param_names
  )
}

# Obtiene el identificador de un modelo a partir de su nombre. Solo contiene
# letras, números y guiones bajos, así que se puede usar en nombres de archivo.
# Recibe: nombre del modelo.
# Devuelve: identificador.
model_id_from_name <- function(name) {
  id <- gsub("[^A-Za-z0-9_]+", "_", trimws(name))
  gsub("^_+|_+$", "", id)
}

# Comprueba si un nombre de modelo tiene un formato válido.
# Recibe: nombre del modelo.
# Devuelve: TRUE si es válido y FALSE en caso contrario.
is_valid_model_name <- function(name) {
  is.character(name) && length(name) == 1 && !is.na(name) &&
    grepl(name_pattern, trimws(name)) &&
    nchar(trimws(name)) <= max_name_length
}

# Comprueba si la descripción de un modelo es válida (puede no haberla).
# Recibe: descripción del modelo.
# Devuelve: TRUE si es válida y FALSE en caso contrario.
is_valid_description <- function(description) {
  is.null(description) ||
    (is.character(description) && length(description) == 1 &&
       !is.na(description) && nchar(description) <= max_description_length)
}

# Comprueba el nombre y la descripción de un modelo.
# Recibe: nombre, descripción e identificadores ya existentes.
# Devuelve: vector de mensajes de error, vacío si no se detectan errores.
check_model_name <- function(name, description, existing_ids) {

  errors <- character(0)

  if (!is_valid_model_name(name)) {
    errors <- c(
      errors,
      paste0(
        "El nombre debe empezar por una letra y usar solo letras sin tilde, ",
        "números, espacios, guiones o guiones bajos (máximo ",
        max_name_length, " caracteres)."
      )
    )
  } else if (tolower(model_id_from_name(name)) %in% tolower(existing_ids)) {
    errors <- c(
      errors,
      paste0("Ya existe un modelo con el nombre «", trimws(name), "». ",
             "Elige otro.")
    )
  }

  if (!is_valid_description(description)) {
    errors <- c(
      errors,
      paste0("La descripción no puede superar ", max_description_length,
             " caracteres.")
    )
  }

  errors
}

# Comprueba la especificación de un modelo antes de generar su código.
# Recibe: especificación (lista con absorption, ncmt, elimination, params y,
# si se comprueba el nombre, name y description) e identificadores ya
# existentes. Si existing_ids es NULL no se comprueba el nombre.
# Devuelve: vector de mensajes de error, vacío si no se detectan errores.
validate_model_spec <- function(spec, existing_ids = NULL) {

  errors <- character(0)

  if (!is.null(existing_ids)) {
    errors <- check_model_name(spec$name, spec$description, existing_ids)
  }

  valid_structure <-
    isTRUE(spec$absorption %in% absorption_options) &&
    isTRUE(as.character(spec$ncmt) %in% ncmt_options) &&
    isTRUE(spec$elimination %in% elimination_options)

  if (!valid_structure) {
    return(c(errors, "Elige una estructura de modelo válida."))
  }

  needed <- builder_param_names(spec)
  params <- stats::setNames(
    lapply(needed, function(name) spec$params[[name]]),
    needed
  )

  invalid <- needed[!vapply(params, is_valid_number, logical(1))]
  if (length(invalid) > 0) {
    errors <- c(
      errors,
      paste0(
        "Estos parámetros están vacíos o no son números: ",
        paste(invalid, collapse = ", "), "."
      )
    )
  }

  valid <- params[!names(params) %in% invalid]
  errors <- c(errors, check_param_rules(valid))

  too_big <- names(valid)[vapply(
    valid, function(x) x > max_param_value, logical(1)
  )]
  if (length(too_big) > 0) {
    errors <- c(
      errors,
      paste0(
        "Estos parámetros no pueden superar ", format(max_param_value),
        ": ", paste(too_big, collapse = ", "), "."
      )
    )
  }

  errors
}

# Genera el código de mrgsolve de un modelo. Solo debe llamarse con una
# especificación que haya pasado validate_model_spec().
# Recibe: especificación del modelo.
# Devuelve: texto con el código del modelo.
generate_model_code <- function(spec) {

  oral <- spec$absorption == "oral"
  two_cmt <- as.integer(spec$ncmt) == 2L
  saturable <- spec$elimination == "mm"

  param_names <- builder_param_names(spec)
  param_lines <- vapply(
    param_names,
    function(name) sprintf("%s = %.15g", name, spec$params[[name]]),
    character(1)
  )

  cmts <- c(if (oral) "DEPOT", "CENT", if (two_cmt) "PERIPH")

  # Ecuación del compartimento central: se escribe término a término.
  elimination <- if (saturable) {
    "- VMAX*CONC/(KM + CONC)"
  } else {
    "- (CL/V)*CENT"
  }
  central_terms <- c(
    if (oral) "KA*DEPOT",
    elimination,
    if (two_cmt) c("- (Q/V)*CENT", "+ (Q/VP)*PERIPH")
  )
  central <- sub("^- ", "-", paste(central_terms, collapse = " "))

  ode <- c(
    if (saturable) "double CONC = CENT/V;",
    if (oral) "dxdt_DEPOT = -KA*DEPOT;",
    paste0("dxdt_CENT = ", central, ";"),
    if (two_cmt) "dxdt_PERIPH = (Q/V)*CENT - (Q/VP)*PERIPH;"
  )

  paste(
    c(
      "$PARAM", param_lines,
      "",
      paste("$CMT", paste(cmts, collapse = " ")),
      "",
      "$ODE", ode,
      "",
      "$TABLE",
      "double CP = CENT/V;",
      "",
      "$CAPTURE CP"
    ),
    collapse = "\n"
  )
}

# Resume la estructura de un modelo en una frase corta.
# Recibe: especificación del modelo.
# Devuelve: texto, por ejemplo "Oral · 2 compartimentos · eliminación lineal".
describe_spec <- function(spec) {
  n_cmt <- as.integer(spec$ncmt)

  paste(
    c(iv = "Intravenosa", oral = "Oral")[[spec$absorption]],
    paste(n_cmt, if (n_cmt == 1L) "compartimento" else "compartimentos"),
    c(linear = "eliminación lineal", mm = "eliminación saturable")[[
      spec$elimination
    ]],
    sep = " · "
  )
}

# Compila un modelo creado por el usuario y comprueba que se puede simular.
# Si algo falla, lanza un error con el mensaje ya en español; el mensaje
# original del compilador solo se escribe en la consola, para depuración.
# Recibe: especificación del modelo (ya validada) y nombre interno único con
# el que se compila (mrgsolve compila en una carpeta temporal compartida por
# todas las sesiones, así que no puede repetirse).
# Devuelve: modelo de mrgsolve compilado.
compile_user_model <- function(spec, internal_name) {

  mod <- tryCatch(
    mrgsolve::mcode(internal_name, generate_model_code(spec), quiet = TRUE),
    error = function(e) {
      message("[mrgsolve] ", conditionMessage(e))
      stop(compile_model_error, call. = FALSE)
    }
  )

  # Prueba rápida: una dosis y 24 unidades de tiempo.
  result <- simulate_model(
    mod,
    params = list(),
    dose = 100,
    dose_cmt = get_model_cmt(mod)[1],
    interval = 0,
    n_doses = 1,
    end_time = 24
  )

  if (!all(is.finite(unlist(result)))) {
    stop(unstable_model_error, call. = FALSE)
  }

  mod
}

# Construye la entrada de un modelo del usuario para guardarla en la lista de
# modelos de la sesión.
# Recibe: identificador, especificación, modelo compilado (NULL si aún no se
# ha compilado), si está guardado en la cuenta y si está compartido.
# Devuelve: lista con los datos del modelo.
new_user_model <- function(id, spec, mod, saved = FALSE, is_shared = FALSE) {
  list(
    id = id,
    label = trimws(spec$name),
    description = if (is.null(spec$description)) "" else spec$description,
    summary = describe_spec(spec),
    spec = spec,
    mod = mod,
    saved = saved,
    is_shared = is_shared
  )
}

# Borra los archivos que mrgsolve ha generado al compilar los modelos de una
# sesión, para no acumularlos en la carpeta temporal.
# Recibe: prefijo de los nombres internos de los modelos de la sesión.
remove_compiled_files <- function(prefix) {
  files <- list.files(
    tempdir(),
    pattern = paste0("^", prefix),
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  unlink(files, recursive = TRUE)
}
