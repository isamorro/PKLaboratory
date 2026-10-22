# ============================================================
# Archivo: model_builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Construye y valida modelos farmacocinéticos por componentes, con expresiones
# matemáticas analizadas y convertidas a código C++ sin evaluar código R, y
# compila el código de mrgsolve generado a partir de ellos.
# ============================================================

# Límites de los datos que introduce el usuario.
max_name_length <- 30
max_description_length <- 200

# Un nombre empieza por una letra y solo admite letras sin tilde, números,
# espacios, guiones y guiones bajos.
name_pattern <- "^[A-Za-z][A-Za-z0-9 _-]*$"

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

# Convierte una expresión matemática a C++ a partir de un árbol validado.
# No admite asignaciones, índices, namespaces, llamadas arbitrarias ni cadenas.
# Recibe: cadena con la expresión matemática y vector de símbolos autorizados.
# Devuelve: texto equivalente en C++ listo para inyectarse en el modelo.
math_cpp <- function(text, symbols) {
  if (
    !is.character(text) || length(text) != 1L || is.na(text) ||
      nchar(text) > 1000L
  ) {
    stop("Expresión vacía o demasiado larga.")
  }
  ast <- tryCatch(parse(text = text, keep.source = FALSE),
    error = function(e) stop("Expresión matemática no válida.")
  )
  if (length(ast) != 1L) stop("Introduce una sola expresión matemática.")
  count <- 0L
  visit <- function(x, depth = 0L) {
    count <<- count + 1L
    if (depth > 40L || count > 250L) stop("Expresión demasiado compleja.")
    if ((is.double(x) || is.integer(x)) && length(x) == 1L && is.finite(x)) {
      return(format(x,
        digits = 17, scientific = TRUE, trim = TRUE,
        decimal.mark = "."
      ))
    }
    if (is.name(x)) {
      name <- as.character(x)
      if (!name %in% symbols) stop(paste("Variable desconocida:", name))
      return(name)
    }
    if (!is.call(x) || !is.name(x[[1]])) stop("Elemento no admitido.")
    op <- as.character(x[[1]])
    args <- as.list(x)[-1L]
    if (!is.null(names(args)) && any(nzchar(names(args)))) {
      stop("No se admiten argumentos con nombre.")
    }
    arity <- length(args)
    binary <- c(
      "+", "-", "*", "/", "^", "<", "<=", ">", ">=",
      "==", "!=", "&&", "||"
    )
    unary <- c("+", "-", "!")
    funcs <- c(
      exp = 1L, log = 1L, log10 = 1L, sqrt = 1L, abs = 1L,
      sin = 1L, cos = 1L, tan = 1L, floor = 1L, ceiling = 1L,
      min = 2L, max = 2L, pow = 2L, ifelse = 3L
    )
    if (op == "(" && arity == 1L) {
      return(paste0("(", visit(args[[1]], depth + 1L), ")"))
    }
    valid <- (op %in% binary && arity == 2L) ||
      (op %in% unary && arity == 1L) ||
      (op %in% names(funcs) && arity == funcs[[op]])
    if (!valid) stop(paste("Operación o número de argumentos no admitido:", op))
    a <- vapply(args, visit, character(1), depth = depth + 1L)
    if (op %in% unary && arity == 1L) {
      return(paste0("(", op, a, ")"))
    }
    if (op == "^" || op == "pow") {
      return(paste0("std::pow(", paste(a, collapse = ", "), ")"))
    }
    if (op %in% binary) {
      return(paste0("(", a[1], " ", op, " ", a[2], ")"))
    }
    if (op == "ifelse") {
      return(paste0("(", a[1], " ? ", a[2], " : ", a[3], ")"))
    }
    cpp <- switch(op,
      abs = "fabs",
      ceiling = "ceil",
      min = "fmin",
      max = "fmax",
      op
    )
    paste0("std::", cpp, "(", paste(a, collapse = ", "), ")")
  }
  visit(ast[[1]])
}

# Nombres deliberadamente acotados para no colisionar con C++ o mrgsolve.
# Recibe: vector con nombres de variables, parámetros y compartimentos.
# Devuelve: valor lógico indicando qué nombres son válidos para el modelo.
valid_component_names <- function(n) {
  reserved <- c(
    "TIME", "SOLVERTIME", "ID", "EVID", "AMT", "CMT", "NEWIND", "SS",
    "F", "R", "D", "ALAG", "double", "int", "float", "bool", "void", "char",
    "long", "short", "unsigned", "signed", "auto", "const", "static", "extern",
    "class", "struct", "enum", "union", "typedef", "using", "namespace", "std",
    "if", "else", "for", "while", "do", "switch", "case", "default", "break",
    "continue", "return", "goto", "new", "delete", "this", "true", "false",
    "nullptr", "sizeof", "template", "typename",
    "public", "private", "protected",
    "try", "catch", "throw", "operator", "virtual", "inline", "volatile",
    "register", "restrict", "asm", "explicit", "friend", "mutable", "constexpr",
    "thread_local", "noexcept", "alignas", "alignof", "decltype", "wchar_t",
    "and", "or", "not", "xor", "bitand", "bitor", "compl", "and_eq", "or_eq",
    "not_eq", "xor_eq", "NULL", "NA", "Inf", "NaN", "capture", "self",
    "simeta", "simeps", "ETA", "EPS", "THETA", "TAD", "SYSTEM", "table",
    "exp", "log", "log10", "sqrt", "abs", "sin", "cos", "tan", "floor",
    "ceiling", "min", "max", "pow", "ifelse"
  )
  grepl("^[A-Za-z][A-Za-z0-9_]{0,29}$", n) & !n %in% reserved &
    !grepl("^(dxdt_|F_|R_|D_|ALAG_|N_|A_0_|pred_)", n)
}

# Valida una especificación de modelo por componentes.
# Recibe: especificación del modelo y, opcionalmente, identificadores ya
# usados para comprobar nombres duplicados.
# Devuelve: vector de errores de validación; si todo es correcto, devuelve
# un vector vacío.
validate_model_spec <- function(spec, existing_ids = NULL) {
  errors <- character()
  if (!is.null(existing_ids)) {
    errors <- check_model_name(spec$name, spec$description, existing_ids)
  }

  groups <- list(spec$params, spec$initials, spec$auxiliary, spec$equations)
  if (!all(vapply(groups, is.list, logical(1)))) {
    return(c(errors, "Componentes no válidos."))
  }

  if (
    length(spec$params) > 50L || length(spec$initials) < 1L ||
      length(spec$initials) > 30L || length(spec$auxiliary) > 50L
  ) {
    return(c(
      errors,
      paste(
        "Máximos: 50 parámetros, 30 compartimentos y 50 auxiliares;",
        "mínimo un compartimento."
      )
    ))
  }

  declared <- c(names(spec$params), names(spec$initials), names(spec$auxiliary))
  if (
    length(declared) != sum(vapply(groups[1:3], length, integer(1))) ||
      any(!valid_component_names(declared)) || anyDuplicated(declared)
  ) {
    return(c(
      errors,
      paste(
        "Usa nombres únicos, no reservados, de hasta 30 caracteres:",
        "letras, números y guion bajo; empieza por letra."
      )
    ))
  }

  numeric_values <- c(spec$params, spec$initials)
  if (any(!vapply(numeric_values, is_valid_number, logical(1)))) {
    return(c(
      errors,
      "Los parámetros y las cantidades iniciales deben ser números finitos."
    ))
  }

  if (
    !setequal(names(spec$initials), names(spec$equations)) ||
      anyDuplicated(names(spec$equations)) ||
      length(spec$equations) != length(spec$initials)
  ) {
    return(c(
      errors,
      "Define exactamente una derivada para cada compartimento."
    ))
  }

  symbols <- c(names(spec$params), names(spec$initials), "TIME", "SOLVERTIME")
  check <- function(text, label) {
    tryCatch(
      {
        math_cpp(text, symbols)
        character()
      },
      error = function(e) paste0(label, ": ", conditionMessage(e))
    )
  }

  for (n in names(spec$auxiliary)) {
    errors <- c(errors, check(spec$auxiliary[[n]], paste("Auxiliar", n)))
    symbols <- c(symbols, n)
  }
  for (n in names(spec$equations)) {
    errors <- c(errors, check(spec$equations[[n]], paste("Derivada", n)))
  }

  if (
    !is.character(spec$outputs) || !length(spec$outputs) ||
      anyDuplicated(spec$outputs) || !all(spec$outputs %in% declared)
  ) {
    errors <- c(
      errors,
      "Selecciona al menos una salida existente, sin repeticiones."
    )
  }

  errors
}

# Genera el código mrgsolve a partir de una especificación por componentes,
# manteniendo los cálculos auxiliares y las ecuaciones diferenciales en el
# formato que acepta el compilador del modelo.
# Recibe: especificación del modelo construida por componentes.
# Devuelve: cadena de texto con el bloque de modelo listo para compilar.
generate_model_code <- function(spec) {
  errors <- validate_model_spec(spec)
  if (length(errors)) stop(paste(errors, collapse = "\n"))

  values <- function(x) {
    vapply(names(x), function(n) {
      paste0(
        n, " = ",
        format(
          x[[n]],
          digits = 17,
          scientific = TRUE,
          trim = TRUE,
          decimal.mark = "."
        )
      )
    }, character(1))
  }

  symbols <- c(names(spec$params), names(spec$initials), "TIME", "SOLVERTIME")
  aux <- character()
  for (n in names(spec$auxiliary)) {
    aux <- c(aux, paste0(n, " = ", math_cpp(spec$auxiliary[[n]], symbols), ";"))
    symbols <- c(symbols, n)
  }

  ode <- vapply(names(spec$equations), function(n) {
    paste0(
      "dxdt_", n, " = ",
      math_cpp(spec$equations[[n]], symbols), ";"
    )
  }, character(1))

  # Auxiliares declarados una sola vez y recalculados en ODE y TABLE.
  paste(
    c(
      "// Generado por PKLaboratory: expresiones matemáticas validadas",
      if (length(spec$params)) c("$PARAM", values(spec$params), ""),
      "$INIT",
      values(spec$initials),
      "",
      if (length(aux)) {
        c("$GLOBAL", paste0("double ", names(spec$auxiliary), ";"), "")
      },
      "$ODE",
      gsub("\\bTIME\\b", "SOLVERTIME", aux),
      gsub("\\bTIME\\b", "SOLVERTIME", ode),
      "",
      "$TABLE",
      gsub("\\bSOLVERTIME\\b", "TIME", aux),
      "",
      if (length(setdiff(spec$outputs, names(spec$initials)))) {
        c(
          "$CAPTURE",
          paste(setdiff(spec$outputs, names(spec$initials)), collapse = " ")
        )
      }
    ),
    collapse = "\n"
  )
}

# Devuelve un resumen legible de la especificación para la interfaz y la
# navegación entre modelos guardados.
# Recibe: especificación del modelo.
# Devuelve: texto descriptivo con número de compartimentos y parámetros.
describe_spec <- function(spec) {
  sprintf(
    "Ecuaciones propias · %d compartimentos · %d parámetros",
    length(spec$initials),
    length(spec$params)
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

  # Las ecuaciones propias no dan significado a nombres como CL o V, así que
  # al simular no se aplican las reglas de los parámetros del catálogo.
  attr(mod, "pk_components") <- TRUE
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
