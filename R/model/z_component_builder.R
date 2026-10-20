# ============================================================
# Archivo: component_builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Construye y valida modelos farmacocinéticos por componentes, con expresiones
# matemáticas analizadas y convertidas a código C++ sin evaluar código R.
#
# El prefijo "z_" hace que este archivo se cargue después de los demás
# archivos del modelo, ya que conserva referencias a las funciones
# originales antes de ampliarlas para admitir modelos por componentes.
# ============================================================

# Constructor por componentes. Las expresiones se analizan,
# nunca se evalúan en R.
# Versión 2 de la especificación; conserva la lectura de los modelos
# versión 1.
legacy_validate_spec <- validate_model_spec
legacy_generate_code <- generate_model_code
legacy_normalize_spec <- normalize_saved_spec
legacy_describe_spec <- describe_spec

# Comprueba si una especificación pertenece al formato nuevo de modelos
# construidos por componentes en lugar del esquema clásico de mrgsolve.
# Recibe: lista con la especificación del modelo.
# Devuelve: TRUE si la estructura corresponde a un modelo por componentes.
is_component_spec <- function(spec) is.list(spec) && !is.null(spec$equations)

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

# Valida una especificación de modelo por componentes y reutiliza la
# validación heredada cuando el modelo sigue el esquema clásico.
# Recibe: especificación del modelo y, opcionalmente, identificadores ya
# usados para comprobar nombres duplicados.
# Devuelve: vector de errores de validación; si todo es correcto, devuelve
# un vector vacío.
validate_model_spec <- function(spec, existing_ids = NULL) {
  if (!is_component_spec(spec)) {
    return(legacy_validate_spec(spec, existing_ids))
  }

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
  if (!is_component_spec(spec)) {
    return(legacy_generate_code(spec))
  }

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
  if (!is_component_spec(spec)) {
    return(legacy_describe_spec(spec))
  }
  sprintf(
    "Ecuaciones propias · %d compartimentos · %d parámetros",
    length(spec$initials),
    length(spec$params)
  )
}

# Serializa la especificación del modelo en un formato persistente y
# compatible con la carga posterior desde la base de datos.
# Recibe: especificación del modelo.
# Devuelve: lista serializable con la versión y el contenido del modelo.
serialize_model_spec <- function(spec) {
  if (is_component_spec(spec)) {
    return(list(
      version = 2L,
      params = spec$params,
      initials = spec$initials,
      auxiliary = spec$auxiliary,
      equations = spec$equations,
      outputs = spec$outputs
    ))
  }
  list(
    version = 1L,
    absorption = spec$absorption,
    ncmt = as.character(spec$ncmt),
    elimination = spec$elimination,
    params = spec$params
  )
}

# Normaliza una especificación guardada y la valida antes de reutilizarla,
# para mantener compatibilidad con modelos antiguos y nuevos.
# Recibe: especificación guardada o importada por el usuario.
# Devuelve: la misma estructura ya normalizada y validada.
normalize_saved_spec <- function(spec) {
  if (!is.list(spec) || !identical(spec$version, 2L)) {
    return(legacy_normalize_spec(spec))
  }
  spec$version <- NULL
  problems <- validate_model_spec(spec)
  if (length(problems)) stop(paste(problems, collapse = "\n"))
  spec
}

# Transforma los modelos clásicos en una especificación por componentes para
# poder editarlos y validarlos con el sistema nuevo.
# Recibe: modelo clásico con absorción, número de compartimentos y eliminación.
# Devuelve: una especificación equivalente en formato por componentes.
as_component_spec <- function(spec) {
  if (is_component_spec(spec)) {
    return(spec)
  }

  oral <- spec$absorption == "oral"
  two <- as.integer(spec$ncmt) == 2L
  mm <- spec$elimination == "mm"
  initials <- as.list(stats::setNames(
    rep(0, 1L + oral + two),
    c(if (oral) "DEPOT", "CENT", if (two) "PERIPH")
  ))
  eq <- list()
  if (oral) eq$DEPOT <- "-KA * DEPOT"
  eq$CENT <- paste(c(
    if (oral) "KA * DEPOT",
    if (mm) "- VMAX * CP / (KM + CP)" else "- CL / V * CENT",
    if (two) "- Q / V * CENT + Q / VP * PERIPH"
  ), collapse = " ")
  if (two) eq$PERIPH <- "Q / V * CENT - Q / VP * PERIPH"
  list(
    name = spec$name,
    description = spec$description,
    params = spec$params,
    initials = initials,
    auxiliary = list(CP = "CENT / V"),
    equations = eq,
    outputs = "CP"
  )
}

# Las ecuaciones de componentes no imponen significado por el nombre CL, V, etc.
# Compila el modelo del usuario y marca si se ha generado con la estrategia de
# componentes para conservar la metadata del modelo en la salida.
# Recibe: especificación del usuario y nombre interno del modelo.
# Devuelve: objeto mrgsolve compilado con la marca de componentes.
legacy_compile_model <- compile_user_model
compile_user_model <- function(spec, internal_name) {
  mod <- legacy_compile_model(spec, internal_name)
  if (is_component_spec(spec)) attr(mod, "pk_components") <- TRUE
  mod
}
