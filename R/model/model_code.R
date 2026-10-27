# ============================================================
# Archivo: model_code.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Valida modelos escritos directamente con la sintaxis de mrgsolve (archivo
# .cpp con bloques $PARAM, $CMT, $MAIN, $ODE, $TABLE...). El código se
# compila y se ejecuta en el servidor, así que antes se comprueba sin
# evaluarlo:
#   - Los bloques que mrgsolve evalúa como código R ($PARAM, $INIT, $OMEGA,
#     $SET...) solo pueden contener valores constantes.
#   - Los bloques de C++ ($MAIN, $ODE, $TABLE, $EVENT...) solo pueden llamar
#     a funciones matemáticas y a las de mrgsolve, sin #include, cadenas,
#     punteros, bucles ni definiciones de funciones.
#   - No se admiten los bloques que leen archivos o ejecutan código R
#     ($ENV, $INCLUDE, $NMXML, $NMEXT...) ni los plugins que dan acceso a R.
# Documentación de los bloques: https://mrgsolve.org/user-guide/specification.html
# ============================================================

# Guía de mrgsolve donde se explican los bloques del modelo.
mrgsolve_spec_url <- "https://mrgsolve.org/user-guide/specification.html#pkpd-model"

# Nota de mrgsolve 2.0.1 que presenta el argumento advan de $PKMODEL.
mrgsolve_advan_url <- paste0(
  "https://mrgsolve.org/blog/posts/2026-new-2-0-1.html",
  "#pkmodel-gains-advan-argument"
)

# Longitud máxima del código de un modelo.
max_code_length <- 30000L

# Bloques admitidos, agrupados según cómo los interpreta mrgsolve. Los alias
# ($PK, $DES, $ERROR) se tratan igual que su bloque principal.
code_block_types <- c(
  PROB = "text",
  PARAM = "values", INIT = "values", FIXED = "values", INPUT = "values",
  THETA = "values", SET = "values", PKMODEL = "values",
  OMEGA = "matrix", SIGMA = "matrix",
  CMT = "names", VCMT = "names", CAPTURE = "names", CMTN = "names",
  PLUGIN = "plugin",
  MAIN = "cpp", PK = "cpp", ODE = "cpp", DES = "cpp", TABLE = "cpp",
  ERROR = "cpp", EVENT = "cpp", PRED = "cpp", GLOBAL = "cpp",
  PREAMBLE = "cpp"
)

# Bloques de mrgsolve que no se admiten porque ejecutan código R o leen
# archivos del servidor.
blocked_code_blocks <- c(
  "ENV", "INCLUDE", "NMXML", "NMEXT", "NAMESPACE", "YAML", "TRANSIT", "BLOCK"
)

# Plugins admitidos (los demás dan acceso a R o a bibliotecas externas).
allowed_plugins <- c(
  "autodec", "nm-vars", "nm-like", "tad", "evtools", "Rcpp", "CXX11",
  "N_CMT", "semicolons"
)

# Opciones de bloque (@opción) admitidas. Las demás, como @code, @param,
# @etas o @as_object, hacen que mrgsolve evalúe código R.
allowed_block_options <- c(
  "annotated", "covariates", "input", "tag", "name", "labels", "block",
  "correlation", "cor", "prefix", "audit"
)

# Funciones que se pueden llamar en los bloques de C++, sin espacio de
# nombres o con std::.
cpp_math_functions <- c(
  "exp", "log", "log10", "log2", "log1p", "expm1", "sqrt", "cbrt", "pow",
  "fabs", "abs", "floor", "ceil", "round", "trunc", "fmin", "fmax", "fmod",
  "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
  "tanh", "erf", "erfc", "tgamma", "lgamma", "hypot", "isnan", "isfinite",
  "isinf", "min", "max"
)

# Macros y funciones propias de mrgsolve y de sus plugins.
cpp_model_functions <- c(
  "THETA", "ETA", "EPS", "SIGMA", "ERR", "A", "A_0", "DADT", "EXP", "DEXP",
  "LOG", "LOG10", "SQRT", "COS", "SIN", "simeta", "simeps", "mtime",
  "SYSTEMSTOPADVANCING", "STOPADVANCING", "CFONSTOP", "DXDTZERO"
)

# Funciones con espacio de nombres: números aleatorios y distribuciones de
# R (plugin Rcpp), eventos (plugin evtools) y tiempo tras dosis (plugin tad).
cpp_qualified_functions <- c(
  paste0("std::", cpp_math_functions),
  paste0("R::", c(
    "rnorm", "runif", "rlnorm", "rexp", "rgamma", "rbeta", "rbinom", "rpois",
    "rweibull", "rlogis", "rcauchy", "rchisq", "rt", "pnorm", "qnorm",
    "dnorm", "plnorm", "qlnorm", "dlnorm", "pexp", "qexp", "punif", "pgamma",
    "qgamma", "pbinom", "dbinom", "ppois", "dpois", "pt", "qt"
  )),
  paste0("evt::", c(
    "bolus", "infuse", "replace", "reset", "retime", "addl", "ii", "ss",
    "amt", "rate", "cmt", "evid", "now", "tgrid", "push", "near"
  ))
)

# Tipos con los que se pueden declarar variables.
cpp_types <- c(
  "double", "int", "bool", "float", "long", "short", "unsigned", "signed",
  "char", "capture"
)
cpp_qualified_types <- c(
  "evt::ev", "evt::regimen", "mrg::tadose", "mrgsolve::tadose",
  "mrg::evdata", "mrgsolve::evdata"
)

# Métodos que se pueden llamar con objeto.método(...): los de self y los de
# los objetos de evtools y tad.
cpp_methods <- c(
  "stop", "stop_id", "stop_id_cf", "mevent", "mtime", "tad", "push", "init",
  "reset", "execute", "cmt", "amt", "ii", "rate", "time_next", "until",
  "flagnext"
)

# Palabras clave de C++ que no se admiten: bucles (podrían no terminar),
# definiciones de tipos o funciones, memoria dinámica, conversiones de
# punteros y extensiones del compilador.
cpp_forbidden_words <- c(
  "for", "while", "do", "goto", "asm", "auto", "class", "struct", "union",
  "enum", "namespace", "using", "typedef", "template", "typename",
  "operator", "new", "delete", "this", "sizeof", "alignof", "alignas",
  "decltype", "extern", "friend", "virtual", "explicit", "mutable",
  "volatile", "register", "thread_local", "inline", "try", "catch", "throw",
  "static_cast", "dynamic_cast", "const_cast", "reinterpret_cast",
  "noexcept", "constexpr", "consteval", "constinit", "static_assert",
  "typeid", "co_await", "co_yield", "co_return", "requires", "concept",
  "export", "import", "module", "_Pragma"
)

# Divide el código en bloques igual que lo hace mrgsolve al compilarlo, para
# validar exactamente lo mismo que después se compila.
# Recibe: código del modelo.
# Devuelve: lista con nombre (etiqueta del bloque) de vectores de líneas.
split_model_blocks <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  blocks <- mrgsolve:::modelparse(lines)
  names(blocks) <- toupper(names(blocks))
  blocks
}

# Separa las opciones de bloque (líneas que empiezan por @) del contenido y
# comprueba que solo se usan las admitidas.
# Recibe: líneas de un bloque y opciones admitidas en ese bloque.
# Devuelve: lista con las líneas de contenido (data), si el bloque está
# anotado (annotated) y los errores encontrados.
scrape_block_options <- function(lines, allowed) {
  errors <- character()
  is_option <- grepl("^\\s*@", lines)
  annotated <- FALSE

  if (any(grepl("^\\s*>>", lines))) {
    errors <- c(errors, "las opciones con >> no están admitidas")
  }

  for (line in lines[is_option]) {
    for (option in trimws(strsplit(line, "@", fixed = TRUE)[[1]])) {
      if (!nzchar(option)) next
      name <- sub("^!", "", sub("\\s.*$", "", option))
      value <- trimws(sub("^\\S+", "", option))
      if (!name %in% allowed) {
        errors <- c(errors, paste0("la opción @", name, " no está admitida"))
      } else if (!grepl("^[A-Za-z0-9_., -]*$", value)) {
        errors <- c(errors, paste0("valor no válido en la opción @", name))
      }
      if (name == "annotated") annotated <- TRUE
    }
  }

  list(
    data = lines[!is_option & !grepl("^\\s*>>", lines)],
    annotated = annotated,
    errors = errors
  )
}

# Comprueba que una expresión de R es un valor constante: números, cadenas
# sencillas, TRUE o FALSE, operaciones aritméticas y unas pocas funciones
# matemáticas. Es lo único que se deja evaluar a mrgsolve.
# Recibe: expresión de R ya analizada (sin evaluar) y si admite cadenas.
# Devuelve: TRUE si es constante y FALSE en caso contrario.
is_constant_expr <- function(x, strings = FALSE) {
  if ((is.double(x) || is.integer(x)) && length(x) == 1L) return(TRUE)
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(strings)
  if (is.character(x) && length(x) == 1L) return(strings)
  if (is.name(x)) return(as.character(x) %in% c("TRUE", "FALSE") && strings)
  if (!is.call(x) || !is.name(x[[1]])) return(FALSE)

  op <- as.character(x[[1]])
  args <- as.list(x)[-1L]
  ok <- (op %in% c("+", "-") && length(args) %in% 1:2) ||
    (op %in% c("*", "/", "^") && length(args) == 2L) ||
    (op == "(" && length(args) == 1L) ||
    (op %in% c("exp", "log", "sqrt") && length(args) == 1L)
  ok && is.null(names(args)) &&
    all(vapply(args, is_constant_expr, logical(1), strings = FALSE))
}

# Comprueba una lista de valores con el mismo formato que mrgsolve evalúa
# (nombre = valor, separados por comas o saltos de línea).
# Recibe: líneas del bloque, si los valores deben tener nombre y si se
# admiten cadenas.
# Devuelve: vector de errores.
check_value_lines <- function(lines, named = TRUE, strings = FALSE) {
  # Comprobación línea a línea: solo nombres, números, operadores y cadenas
  # sencillas, para que ninguna cadena pueda ocultar código entre líneas.
  plain <- if (strings) gsub('"[A-Za-z0-9_ ,.-]*"', "0", lines) else lines
  if (any(!grepl("^[A-Za-z0-9_ .,=+*/^()-]*$", plain))) {
    return("solo admite nombres, números y operaciones aritméticas")
  }

  # Mismo texto que evalúa mrgsolve: las líneas unidas dentro de list().
  lines <- gsub("(,|\\s)+$", "", lines, perl = TRUE)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(character())

  ast <- tryCatch(
    parse(
      text = paste0("list(", paste(lines, collapse = ","), ")"),
      keep.source = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ast) || length(ast) != 1L) {
    return("formato no válido; usa nombre = valor")
  }

  args <- as.list(ast[[1]])[-1L]
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  if (named && any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", arg_names))) {
    return("cada valor necesita un nombre: nombre = valor")
  }
  if (!all(vapply(args, is_constant_expr, logical(1), strings = strings))) {
    return("los valores deben ser números (no se evalúa código R)")
  }
  character()
}

# Comprueba las líneas anotadas (nombre : valor : descripción).
# Recibe: líneas del bloque, si llevan nombre y si llevan valor.
# Devuelve: vector de errores.
check_annotated_lines <- function(lines, has_name = TRUE, has_value = TRUE) {
  lines <- lines[nzchar(trimws(lines))]
  for (line in lines) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    if (length(parts) < has_name + has_value) {
      return("formato anotado no válido; usa nombre : valor : descripción")
    }
    if (has_name && !grepl("^\\s*[A-Za-z_][A-Za-z0-9_.]*\\s*$", parts[[1]])) {
      return(paste("nombre no válido:", trimws(parts[[1]])))
    }
    if (has_value) {
      value <- parts[[1L + has_name]]
      if (length(check_value_lines(value, named = FALSE))) {
        return(paste("el valor debe ser un número:", trimws(value)))
      }
    }
  }
  character()
}

# Separa las líneas de un bloque en palabras (separadas por espacios o
# comas).
# Recibe: líneas del bloque.
# Devuelve: vector de palabras, sin vacías.
split_words <- function(lines) {
  words <- unlist(strsplit(trimws(lines), "[[:space:],]+"))
  words[nzchar(words)]
}

# Comprueba un bloque de matriz ($OMEGA, $SIGMA): solo números.
# Recibe: líneas del bloque.
# Devuelve: vector de errores.
check_matrix_lines <- function(lines) {
  if (any(is.na(suppressWarnings(as.numeric(split_words(lines)))))) {
    return("solo admite números separados por espacios o comas")
  }
  character()
}

# Comprueba un bloque de nombres ($CMT, $CAPTURE...).
# Recibe: líneas del bloque.
# Devuelve: vector de errores.
check_name_lines <- function(lines) {
  if (any(!grepl("^[A-Za-z0-9_ ,=\t]*$", lines))) {
    return("solo admite nombres separados por espacios o comas")
  }
  character()
}

# Separa el código C++ en símbolos (identificadores, números y operadores).
# Recibe: texto del bloque, sin comentarios.
# Devuelve: vector de símbolos o un error con el carácter no admitido.
tokenize_cpp <- function(text) {
  pattern <- paste0(
    "\\s+|",
    "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?[fFlLuU]*|",
    "[A-Za-z_][A-Za-z0-9_]*|",
    "::|->|\\+\\+|--|<<=|>>=|<=|>=|==|!=|&&|\\|\\||[-+*/%&|^]=|<<|>>|",
    "[-+*/%<>=!&|^~?:;,.(){}\\[\\]]"
  )
  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
  tokens <- regmatches(text, list(matches))[[1]]
  if (sum(nchar(tokens)) != nchar(text)) {
    rest <- gsub(pattern, "", text, perl = TRUE)
    stop(paste0("carácter no admitido: ", substr(rest, 1, 1)), call. = FALSE)
  }
  tokens[!grepl("^\\s+$", tokens)]
}

# Comprueba un bloque de C++. Las reglas impiden ejecutar algo que no sea el
# cálculo del modelo:
#   - solo se llaman funciones de las listas permitidas (ni system(), ni
#     exit(), ni funciones de R);
#   - no hay #include, cadenas, punteros, corchetes ni bucles;
#   - no se definen funciones;
#   - no se usan los nombres internos de mrgsolve (los que empiezan por _);
#   - A(n), A_0(n) y DADT(n), que acceden a la memoria sin comprobar el
#     índice, solo admiten un número entre 1 y el de compartimentos.
# Recibe: líneas del bloque (sin comentarios //, que ya quita mrgsolve) y
# número de compartimentos del modelo.
# Devuelve: vector de errores.
check_cpp_lines <- function(lines, n_cmt) {
  # Cada error se guarda una sola vez, aunque se repita en muchos símbolos.
  errors <- character()
  fail <- function(...) {
    message <- paste0(...)
    if (!message %in% errors) errors <<- c(errors, message)
  }

  # Solo se admite #define NOMBRE valor (sin argumentos).
  directive <- grepl("^\\s*#", lines)
  define_re <- "^\\s*#\\s*define\\s+[A-Za-z_][A-Za-z0-9_]*(\\s+(.*))?$"
  if (any(directive & !grepl(define_re, lines))) {
    fail("solo se admite la directiva #define NOMBRE valor (sin #include)")
  }
  lines[directive] <- sub(
    "^\\s*#\\s*define\\s+[A-Za-z_][A-Za-z0-9_]*", "", lines[directive]
  )

  text <- paste(lines, collapse = "\n")
  text <- gsub("/\\*.*?\\*/", " ", text, perl = TRUE)
  if (grepl("/*", text, fixed = TRUE) || grepl("*/", text, fixed = TRUE)) {
    fail("comentario /* */ sin cerrar")
  }
  if (grepl("[\"'\\\\`$@#]", text) || grepl("[^\\x01-\\x7F]", text, perl = TRUE)) {
    fail("no se admiten cadenas, caracteres especiales ni letras con tilde",
         " fuera de los comentarios")
  }
  if (grepl("%:|<%|%>|<:|:>|\\?\\?", text)) {
    fail("secuencia de caracteres no admitida")
  }
  if (length(errors)) return(errors)

  tokens <- tryCatch(tokenize_cpp(text), error = function(e) {
    fail(conditionMessage(e))
    NULL
  })
  if (is.null(tokens)) return(errors)

  n <- length(tokens)
  is_ident <- grepl("^[A-Za-z_]", tokens)
  # Pila de paréntesis abiertos: TRUE si son de if/switch (pueden ir
  # seguidos de {).
  parens <- logical(n)
  depth <- 0L

  # Une los nombres con espacio de nombres (evt::bolus) en un solo símbolo.
  qualified <- tokens
  for (i in seq_len(n)) {
    if (tokens[i] != "::") next
    if (i == 1L || i == n || !is_ident[i - 1L] || !is_ident[i + 1L]) {
      fail("uso no admitido de ::")
      next
    }
    name <- paste0(tokens[i - 1L], "::", tokens[i + 1L])
    if (!name %in% c(cpp_qualified_functions, cpp_qualified_types)) {
      fail("no se admite ", name)
    }
    qualified[i + 1L] <- name
  }
  if (length(errors)) return(errors)

  previous <- function(i) if (i > 1L) qualified[i - 1L] else ""
  is_type <- function(x) x %in% c(cpp_types, cpp_qualified_types, "const",
                                  "static")

  for (i in seq_len(n)) {
    # validate_model_code() muestra como mucho 10 errores.
    if (length(errors) >= 10L) break
    tok <- tokens[i]
    if (is_ident[i] && (tok %in% cpp_forbidden_words || startsWith(tok, "_"))) {
      fail("no se admite «", tok, "»")
    }
    if (tok %in% c("[", "]", "->", "&", "&=")) {
      fail("no se admiten corchetes, punteros ni referencias (", tok, ")")
    }
    if (tok == "*") {
      prev <- previous(i)
      operand <- grepl("^[A-Za-z0-9_.]", prev) && !is_type(prev) &&
        !prev %in% c("return", "case", "else")
      if (!operand && prev != ")") fail("no se admiten punteros (*)")
    }
    if (tok == ".") {
      if (i == n || !is_ident[i + 1L]) fail("uso no admitido de .")
    }

    if (tok == "(") {
      prev <- previous(i)
      control <- prev %in% c("if", "switch")
      depth <- depth + 1L
      parens[depth] <- control
      if (prev == ")") {
        fail("no se admite llamar al resultado de una expresión")
      } else if (i > 1L && is_ident[i - 1L] && !control &&
                 prev != "return") {
        member <- i > 2L && tokens[i - 2L] == "."
        declared <- i > 2L && is_type(previous(i - 1L))
        callable <- if (member) {
          prev %in% cpp_methods
        } else {
          prev %in% c(cpp_math_functions, cpp_model_functions,
                      cpp_qualified_functions, cpp_types,
                      cpp_qualified_types) || declared
        }
        if (!callable) fail("no se admite llamar a «", prev, "»")
        if (!member && prev %in% c("A", "A_0", "DADT")) {
          index <- if (i + 2L <= n && tokens[i + 2L] == ")") {
            suppressWarnings(as.integer(tokens[i + 1L]))
          } else {
            NA
          }
          if (is.na(index) || index < 1L || index > n_cmt) {
            fail(prev, "(n) necesita un número entre 1 y ", n_cmt,
                 " (los compartimentos del modelo)")
          }
        }
      }
    }
    if (tok == ")") {
      control <- depth > 0L && parens[depth]
      depth <- max(depth - 1L, 0L)
      if (i < n && tokens[i + 1L] == "{" && !control) {
        fail("no se pueden definir funciones")
      }
    }
  }
  errors
}

# Cuenta los compartimentos declarados en $CMT, $VCMT e $INIT.
# Recibe: bloques del modelo (ver split_model_blocks()).
# Devuelve: número de compartimentos.
count_model_cmts <- function(blocks) {
  total <- 0L
  for (i in which(names(blocks) %in% c("CMT", "VCMT", "INIT"))) {
    opts <- scrape_block_options(blocks[[i]], allowed_block_options)
    data <- trimws(opts$data)
    data <- data[nzchar(data)]
    total <- total + if (opts$annotated) {
      length(data)
    } else if (names(blocks)[[i]] == "INIT") {
      length(gregexpr("=", paste(data, collapse = ","), fixed = TRUE)[[1]]) *
        any(grepl("=", data, fixed = TRUE))
    } else {
      length(split_words(data))
    }
  }
  total
}

# Valida el código de un modelo de mrgsolve sin compilarlo ni evaluarlo.
# Recibe: código del modelo.
# Devuelve: vector de mensajes de error, vacío si el código es admisible.
validate_model_code <- function(code) {
  if (!is.character(code) || length(code) != 1L || is.na(code) ||
      !nzchar(trimws(code))) {
    return("Escribe el código del modelo.")
  }
  if (nchar(code) > max_code_length) {
    return(paste0("El código no puede superar ", max_code_length,
                  " caracteres."))
  }

  blocks <- tryCatch(split_model_blocks(code), error = function(e) NULL)
  if (is.null(blocks) || !length(blocks)) {
    return(paste(
      "No se ha encontrado ningún bloque. Cada bloque empieza por su",
      "nombre, por ejemplo $PARAM, $CMT u $ODE."
    ))
  }

  errors <- character()
  n_cmt <- count_model_cmts(blocks)
  for (i in seq_along(blocks)) {
    label <- names(blocks)[[i]]
    lines <- blocks[[i]]
    type <- code_block_types[label]
    where <- paste0("$", label, ": ")

    if (label %in% blocked_code_blocks) {
      errors <- c(errors, paste0(
        where, "este bloque no está disponible en PKLaboratory porque lee ",
        "archivos o ejecuta código R en el servidor."
      ))
      next
    }
    if (is.na(type)) {
      errors <- c(errors, paste0(where, "bloque desconocido."))
      next
    }
    if (type == "text") next

    allowed <- switch(type,
      cpp = "audit",
      plugin = character(),
      allowed_block_options
    )
    opts <- scrape_block_options(lines, allowed)
    data <- opts$data
    problems <- opts$errors

    problems <- c(problems, switch(type,
      values = if (opts$annotated) {
        check_annotated_lines(data, has_name = label != "THETA")
      } else {
        check_value_lines(
          data,
          named = label != "THETA",
          strings = label %in% c("SET", "PKMODEL")
        )
      },
      matrix = if (opts$annotated) {
        check_annotated_lines(data)
      } else {
        check_matrix_lines(data)
      },
      names = if (opts$annotated) {
        check_annotated_lines(data, has_value = FALSE)
      } else {
        check_name_lines(data)
      },
      plugin = {
        bad <- setdiff(split_words(data), allowed_plugins)
        if (length(bad)) {
          paste0(
            "plugin no admitido: ", paste(bad, collapse = ", "),
            " (se admiten ", paste(allowed_plugins, collapse = ", "), ")"
          )
        }
      },
      cpp = check_cpp_lines(data, n_cmt)
    ))

    if (length(problems)) {
      errors <- c(errors, paste0(where, problems, "."))
    }
  }

  utils::head(unique(errors), 10L)
}

# Resume los bloques de un modelo escrito con código.
# Recibe: código del modelo.
# Devuelve: vector con los nombres de los bloques, sin repetir.
model_code_blocks <- function(code) {
  blocks <- tryCatch(split_model_blocks(code), error = function(e) list())
  unique(names(blocks))
}

# Extrae los errores del compilador de C++ de los mensajes de mrgsolve, para
# que el usuario sepa qué línea de su código corregir.
# Recibe: mensajes emitidos durante la compilación.
# Devuelve: vector con, como máximo, cinco errores legibles.
compiler_errors <- function(messages) {
  lines <- unlist(strsplit(messages, "\n", fixed = TRUE))
  at <- grep("error:", lines, fixed = TRUE)
  at <- at[!grepl("^make:", lines[at])]
  found <- vapply(at, function(i) {
    text <- sub("^.*?error:\\s*", "", lines[i], perl = TRUE)
    code <- if (i < length(lines)) {
      trimws(sub("^\\s*\\d+\\s*\\|", "", lines[i + 1L]))
    } else {
      ""
    }
    if (nzchar(code)) paste0(text, " — en: ", code) else text
  }, character(1))
  utils::head(unique(found), 5L)
}
