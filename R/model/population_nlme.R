# ============================================================
# Archivo: population_nlme.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para ajustar modelos mixtos no lineales para
# medidas repetidas de Lindstrom y Bates (1990), con su algoritmo
# (nlme::nlme), como en los ejemplos Theoph o Indometh, para cualquier
# modelo de la aplicación (sin dependencias de Shiny). El modelo tiene dos
# etapas y un único nivel de agrupación, el individuo i, con observaciones
# repetidas j en el tiempo:
#   1. Dentro del individuo: y_ij = f(phi_i, t_ij) + e_ij, donde f es el
#      modelo farmacocinético y e_ij el error residual (aditivo,
#      proporcional o combinado).
#   2. Entre individuos: phi_i = A_i beta + b_i, b_i ~ N(0, Psi), con cada
#      parámetro en escala logarítmica (log CL, como lKe, lKa y lCl en
#      Theoph) o natural (como las amplitudes A1 y A2 de SSbiexp en
#      Indometh); las covariables entran en A_i y Psi es diagonal o
#      completa.
# El usuario decide:
#   - la escala de cada parámetro (logarítmica o natural); los modelos
#     con CL y volumen central se estiman siempre con KE en su lugar
#     (V = CL / KE), como SSfol,
#   - qué parámetros llevan efecto aleatorio y la estructura de Psi,
#   - qué covariables actúan sobre qué parámetros,
#   - qué parámetros se dejan fijos y qué salida del modelo se ha medido,
#   - el modelo de error residual.
# Solo se devuelven estimaciones con sus intervalos de confianza y
# desviaciones típicas; valorar si la elección es adecuada corresponde al
# usuario.
#
# La función de esperanza f es el propio modelo de mrgsolve, sin sus
# ETA/EPS: se puede ajustar cualquier modelo de
# la aplicación, de la biblioteca, del constructor o del catálogo
# compartido. Los ajustes individuales exploratorios se hacen con
# nlme::nlsList() y el modelo mixto con nlme::nlme(), como Pinheiro y
# Bates.
# ============================================================

# Modelos de error residual (etiqueta = valor).
population_error_models <- c(
  "Aditivo (error constante, en unidades de la salida)" = "add",
  "Proporcional (error relativo al valor)" = "prop",
  "Combinado (aditivo + proporcional)" = "comb"
)

# Forma del efecto de una covariable continua (etiqueta = valor).
population_covariate_forms <- c(
  "Potencia: parámetro × (covariable / mediana)^β" = "power",
  "Lineal en escala logarítmica: parámetro × exp(β·(covariable − mediana))" =
    "linear"
)

# Estructura de la matriz Psi de los efectos aleatorios (etiqueta = valor).
population_omega_structures <- c(
  "Diagonal: efectos aleatorios independientes (pdDiag)" = "diag",
  "Completa: efectos aleatorios correlacionados (pdSymm)" = "full"
)

# ------------------------------------------------------------
# Estructura del modelo
# ------------------------------------------------------------

# Indica que un modelo no se puede ajustar.
# Recibe: motivo.
# Devuelve: lista con ok = FALSE y el motivo.
population_fail <- function(reason) list(ok = FALSE, reason = reason)

# Parámetros del modelo que se pueden estimar: numéricos, finitos y
# positivos (por defecto se estiman en escala logarítmica).
# Recibe: lista de parámetros (get_model_params()).
# Devuelve: vector con sus nombres.
population_positive_params <- function(params) {
  ok <- vapply(params, function(v) {
    is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0
  }, logical(1))
  names(params)[ok]
}

# Obtiene la estructura ajustable de un modelo de la aplicación (biblioteca
# de mrgsolve, constructor o catálogo compartido). La función de esperanza
# f(phi_i, t) es el propio modelo de mrgsolve, sin sus ETA/EPS (los
# efectos aleatorios los pone nlme). Campos:
#   mod             modelo de mrgsolve
#   params          parámetros estimables (los de $PARAM con valor positivo)
#   values          sus valores iniciales
#   cmts            compartimentos; dose_cmt, el de administración por
#                   defecto
#   outputs         salidas que se pueden medir; observed, la de por defecto
#   central_volume  volumen central (CP = CENT / V...), o NULL
#   ke              nombre del volumen central si se estima KE en su lugar
#   log_params      parámetros que se estiman en escala logarítmica (los
#                   demás, en su escala natural); por defecto, todos
# Recibe: modelo de mrgsolve.
# Devuelve: estructura con ok = TRUE, o lista con ok = FALSE y el motivo.
population_structure <- function(mod) {
  if (!methods::is(mod, "mrgmod")) {
    return(population_fail("no es un modelo de mrgsolve"))
  }
  mod <- mrgsolve::zero_re(mod)
  all_params <- get_model_params(mod)
  params <- population_positive_params(all_params)
  if (!length(params)) {
    return(
      population_fail("el modelo no tiene parámetros positivos que estimar")
    )
  }
  if (any(params %in% c("ID", "time", "TIME", "amt", "evid", "cmt"))) {
    return(population_fail(
      "algún parámetro coincide con una columna de los datos de mrgsolve"
    ))
  }

  vars <- mrgsolve::outvars(mod)
  cmts <- as.character(vars$cmt)
  outputs <- unique(c(as.character(vars$capture), rev(cmts)))
  if (!length(outputs)) {
    return(population_fail("el modelo no tiene salidas"))
  }
  observed <- intersect(c("CP", "IPRED", "CC", "CONC"), outputs)
  observed <- if (length(observed)) observed[[1]] else outputs[[1]]

  st <- list(
    ok = TRUE,
    mod = mod,
    params = params,
    values = unlist(all_params[params]),
    cmts = cmts,
    dose_cmt = if (length(cmts)) cmts[[1]] else NA_character_,
    outputs = outputs,
    observed = observed,
    ke = NULL,
    log_params = params
  )
  central <- population_central(st)
  st$central_volume <- central$volume
  st$central_cmt <- central$cmt
  st
}

# Busca el compartimento y el volumen centrales: la salida medida por
# defecto es el compartimento dividido por el volumen (CP = CENT / V,
# #define CP (CENT/V2)...), leído del código del modelo.
# Recibe: estructura del modelo (population_structure()).
# Devuelve: lista con cmt y volume (NULL si no tiene esa forma).
population_central <- function(st) {
  code <- gsub("[ \t()]", "", st$mod@code)
  name <- "([A-Za-z_][A-Za-z0-9_]*)"
  pat <- sprintf(
    "^(#define|double)?%s=?%s/%s;?$", st$observed, name, name
  )
  for (m in regmatches(code, regexec(pat, code))) {
    if (length(m) == 4L && m[[3]] %in% st$cmts && m[[4]] %in% st$params) {
      return(list(cmt = m[[3]], volume = m[[4]]))
    }
  }
  list(cmt = NULL, volume = NULL)
}

# Obtiene la misma estructura con KE (constante de eliminación) en lugar del
# volumen central, que se calcula como CL / KE antes de resolver el modelo.
# Sirve para cualquier modelo con CL cuya salida sea CENT / volumen (pk1,
# pk2, pk2cmt, pkExample o los del constructor).
# Recibe: estructura del modelo (population_structure()).
# Devuelve: estructura con KE, o NULL si no tiene esa forma o ya tiene un
# parámetro KE.
population_ke_structure <- function(st) {
  if (!isTRUE(st$ok)) {
    return(NULL)
  }
  vc <- st$central_volume
  if (is.null(vc) || !"CL" %in% st$params ||
        "KE" %in% c(st$params, st$outputs)) {
    return(NULL)
  }
  st$values[[vc]] <- st$values[["CL"]] / st$values[[vc]]
  names(st$values)[names(st$values) == vc] <- "KE"
  st$params[st$params == vc] <- "KE"
  st$log_params[st$log_params == vc] <- "KE"
  st$ke <- vc
  st
}

# ------------------------------------------------------------
# Datos y diseño del estudio
# ------------------------------------------------------------

# Comprueba las columnas elegidas y los valores de los datos.
# Recibe: datos, columnas de concentración, paciente y tiempo y,
# opcionalmente, columna de dosis.
# Devuelve: los mismos datos si son válidos; si no, lanza un error.
population_observations <- function(data, response, group, time,
                                    dose_column = NULL) {

  required <- c(response, group, time)
  if (anyNA(required) || any(!nzchar(required)) ||
        any(!required %in% names(data))) {
    stop("Selecciona las columnas de concentración, paciente y tiempo.")
  }
  if (length(unique(required)) != 3L) {
    stop(
      "La concentración, el paciente y el tiempo deben ser columnas ",
      "distintas."
    )
  }
  if (!is.numeric(data[[response]]) || !is.numeric(data[[time]])) {
    stop("Selecciona columnas numéricas para el tiempo y la concentración.")
  }
  if (any(!is.finite(data[[time]]))) {
    stop("Los tiempos contienen NA, NaN o Inf; revisa la columna de tiempo.")
  }
  if (any(data[[time]] < 0)) {
    stop(
      "No se admiten tiempos negativos: el tiempo se cuenta desde la dosis."
    )
  }
  if (!is.null(dose_column)) {
    if (!dose_column %in% names(data) || !is.numeric(data[[dose_column]])) {
      stop("La columna de dosis debe existir y ser numérica.")
    }
    if (dose_column %in% required) {
      stop(
        "La columna de dosis debe ser distinta de las demás columnas ",
        "elegidas."
      )
    }
  }

  # Se conservan las observaciones en t = 0 (Theoph tiene una por
  # paciente): la dosis se administra antes y el modelo predice ahí su
  # valor inicial.
  if (!nrow(data)) {
    stop("No hay observaciones para ajustar el modelo.")
  }
  if (any(!is.finite(data[[response]]))) {
    stop("La concentración contiene NA, NaN o Inf.")
  }
  if (anyNA(data[[group]])) {
    stop("El identificador de paciente contiene valores ausentes.")
  }
  data
}

# Asigna un identificador numérico a cada individuo (el único nivel de
# agrupación) y comprueba que hay medidas repetidas suficientes en cada uno.
# Recibe: datos y columna del paciente.
# Devuelve: vector con el identificador de cada fila.
population_individual_ids <- function(data, group) {
  id <- as.integer(factor(data[[group]]))
  if (length(unique(id)) < 5L) {
    stop("Se necesitan al menos cinco pacientes.")
  }
  if (any(table(id) < 3L)) {
    stop("Se necesitan al menos tres observaciones por paciente.")
  }
  id
}

# Construye el texto con el que se muestra el coeficiente de una
# covariable.
# Recibe: término de covariable (population_covariate_design()).
# Devuelve: texto descriptivo.
population_term_label <- function(term) {
  if (identical(term$kind, "categorical")) {
    return(sprintf(
      paste(
        "Efecto de %s = %s sobre %s",
        "(frente a %s; coeficiente en escala log)"
      ),
      term$col, term$level, term$param, term$reference
    ))
  }
  if (identical(term$form, "power")) {
    return(sprintf(
      "Efecto de %s sobre %s (exponente de %s / %s)",
      term$col, term$param, term$col, signif(term$ref, 4)
    ))
  }
  sprintf(
    paste(
      "Efecto de %s sobre %s",
      "(pendiente en escala log por unidad; referencia %s)"
    ),
    term$col, term$param, signif(term$ref, 4)
  )
}

# Prepara las covariables elegidas por el usuario. Los nombres de las
# columnas del usuario nunca se escriben en el código del modelo: se usan
# nombres internos (COV1, COV2_1...).
# Recibe: datos, vector con el paciente de cada fila, vector
# "COLUMNA|PARAMETRO", parámetros estimados y forma del efecto de las
# covariables continuas.
# Devuelve: lista con las columnas internas (data.frame) y los términos de
# cada efecto.
population_covariate_design <- function(fdata, unit, effects, params,
                                        form = "power") {

  if (!length(effects)) {
    return(list(columns = NULL, terms = list()))
  }
  if (!form %in% c("power", "linear")) {
    stop("Forma de covariable no admitida.")
  }
  effects <- unique(as.character(effects))
  parts <- strsplit(effects, "|", fixed = TRUE)
  if (any(lengths(parts) != 2L)) {
    stop("Efecto de covariable no válido.")
  }
  cols <- vapply(parts, function(x) x[[1]], character(1))
  pars <- vapply(parts, function(x) x[[2]], character(1))
  if (any(!pars %in% params)) {
    stop(
      "Una covariable actúa sobre un parámetro que no se estima ",
      "(fijo o inexistente)."
    )
  }
  if (any(!cols %in% names(fdata))) {
    stop("Una covariable seleccionada no está en los datos.")
  }

  columns <- list()
  terms <- list()
  for (k in seq_along(unique(cols))) {
    col <- unique(cols)[[k]]
    x <- fdata[[col]]
    if (is.list(x) || anyNA(x)) {
      stop(sprintf(
        "La covariable '%s' tiene valores ausentes o no es válida.", col
      ))
    }
    if (!all(population_constant_by(x, unit))) {
      stop(sprintf(
        paste(
          "La covariable '%s' cambia dentro de un mismo paciente; solo se",
          "admiten covariables constantes en cada uno."
        ),
        col
      ))
    }
    internal <- paste0("COV", k)
    targets <- pars[cols == col]

    if (!is.numeric(x) || length(unique(x)) <= 2L) {
      # Categórica: una columna indicadora por nivel, frente al primero.
      lv <- sort(unique(as.character(x)))
      if (length(lv) < 2L) {
        stop(sprintf("La covariable '%s' no varía.", col))
      }
      if (length(lv) > 6L) {
        stop(sprintf(
          "La covariable '%s' tiene demasiadas categorías (máximo 6).", col
        ))
      }
      for (j in 2:length(lv)) {
        nm <- paste0(internal, "_", j - 1L)
        columns[[nm]] <- as.numeric(as.character(x) == lv[[j]])
        for (par in targets) {
          terms[[length(terms) + 1L]] <- list(
            param = par, col = col, kind = "categorical", internal = nm,
            beta = sprintf("beta.%s.%s", tolower(par), tolower(nm)),
            level = lv[[j]], reference = lv[[1]]
          )
        }
      }
    } else {
      # Continua: centrada en la mediana de los pacientes.
      if (any(!is.finite(x))) {
        stop(sprintf("La covariable '%s' contiene valores infinitos.", col))
      }
      if (length(unique(x)) < 2L) {
        stop(sprintf("La covariable '%s' no varía.", col))
      }
      first <- tapply(x, unit, function(z) z[[1]])
      ref <- stats::median(as.numeric(first))
      if (identical(form, "power") && (any(x <= 0) || ref <= 0)) {
        stop(sprintf(
          paste(
            "La covariable '%s' tiene valores no positivos:",
            "elige la forma lineal."
          ),
          col
        ))
      }
      columns[[internal]] <- as.numeric(x)
      for (par in targets) {
        terms[[length(terms) + 1L]] <- list(
          param = par, col = col, kind = "continuous", internal = internal,
          beta = sprintf("beta.%s.%s", tolower(par), tolower(internal)),
          ref = ref, form = form
        )
      }
    }
  }
  list(columns = as.data.frame(columns, check.names = FALSE), terms = terms)
}

# ------------------------------------------------------------
# Lindstrom-Bates con nlme
# ------------------------------------------------------------
# nlme::nlme() es la implementación de Lindstrom y Bates que usan Pinheiro y
# Bates con Theoph e Indometh. La función de esperanza f(phi_i, t_ij) es el
# modelo de la aplicación, que se resuelve con mrgsolve.

# Escribe una línea del registro de mrgsolve (en la salida del proceso,
# que se guarda como registro del ajuste). Para que el registro se pueda
# leer, se escriben como mucho 30 líneas.
# Recibe: texto de la línea.
population_log_mrgsolve <- function(text) {
  n <- getOption("pklab.mrgsolve_lines", 0L) + 1L
  options(pklab.mrgsolve_lines = n)
  if (n <= 30L) {
    cat("[mrgsolve]", text, "\n")
  } else if (n == 31L) {
    cat("[mrgsolve] (se omiten los siguientes mensajes de mrgsolve)\n")
  }
}

# Anota en el registro de mrgsolve, en una línea por llamada, los
# parámetros que no son números válidos (NA, NaN o infinitos) en algún
# paciente, con los valores de todos sus parámetros en el primero de ellos.
# Recibe: data.frame de parámetros (una fila por paciente) e identificador
# de cada fila.
population_log_mrgsolve_params <- function(params, ids) {
  call <- getOption("pklab.mrgsolve_calls", 0L) + 1L
  options(pklab.mrgsolve_calls = call)
  bad <- !vapply(params, function(v) all(is.finite(v)), logical(1))
  if (!any(bad)) {
    return(invisible())
  }
  rows <- which(!stats::complete.cases(lapply(params, function(v) {
    ifelse(is.finite(v), v, NA)
  })))
  first <- rows[[1]]
  population_log_mrgsolve(sprintf(
    paste(
      "llamada %d: %s no %s números válidos en %d de %d pacientes",
      "(paciente %s: %s)"
    ),
    call, paste(names(params)[bad], collapse = ", "),
    if (sum(bad) > 1L) "son" else "es", length(rows), nrow(params),
    ids[[first]],
    paste(
      sprintf("%s = %s", names(params), signif(unlist(params[first, ]), 4)),
      collapse = ", "
    )
  ))
}

# Escribe en el registro la configuración del ajuste y el código del
# modelo de mrgsolve.
# Recibe: título, estructura del modelo, líneas con la configuración y
# valores iniciales de los parámetros estimados.
population_log_header <- function(title, structure, lines, values) {
  options(pklab.mrgsolve_lines = 0L, pklab.mrgsolve_calls = 0L)
  cat(
    "== ", title, " ==\n",
    paste0(lines, "\n"),
    "Valores iniciales: ",
    paste(sprintf("%s = %s", names(values), signif(values, 4)),
          collapse = ", "),
    "\n\n== Código del modelo de mrgsolve ==\n",
    paste0(structure$mod@code, "\n"),
    "\n",
    sep = ""
  )
}

# Predice la salida observada en cada fila (id, times) tras una dosis
# amounts[id] en t = 0 en dose_cmt (sin dosis si el modelo no tiene
# compartimentos). El resto de parámetros del modelo toman su valor de
# $PARAM.
# Recibe: estructura del modelo, data.frame de parámetros (una fila por
# individuo, en el orden de sort(unique(id)), y una columna por parámetro
# estimable), identificador y tiempo de cada fila, dosis de cada individuo,
# compartimento de la dosis y salida medida.
# Devuelve: vector con la predicción de cada fila.
population_predict <- function(structure, params, id, times, amounts,
                               dose_cmt, observed) {
  ids <- sort(unique(id))
  if (!is.null(structure$ke)) {
    params[[structure$ke]] <- params$CL / params$KE
    params$KE <- NULL
  }
  cmt <- match(dose_cmt[1], structure$cmts)
  doses <- if (is.na(cmt)) {
    NULL
  } else {
    data.frame(ID = ids, time = 0, amt = amounts[ids], evid = 1L, cmt = cmt)
  }
  ev <- rbind(
    doses,
    data.frame(
      ID = id, time = times, amt = 0, evid = 0L,
      cmt = if (is.na(cmt)) 0L else cmt
    )
  )
  ev <- ev[order(ev$ID, ev$time, -ev$evid), , drop = FALSE]
  k <- match(ev$ID, ids)
  for (p in names(params)) {
    ev[[p]] <- params[[p]][k]
  }

  # Registro de mrgsolve: parámetros no válidos con los que se le pide
  # resolver el modelo, y sus avisos y errores.
  population_log_mrgsolve_params(params, ids)
  # mrgsim_q devuelve una fila por cada registro de los datos, en su orden.
  out <- withCallingHandlers(
    mrgsolve::mrgsim_q(structure$mod, data = ev, output = "df"),
    warning = function(w) {
      population_log_mrgsolve(paste(
        "aviso en la llamada", getOption("pklab.mrgsolve_calls"), "-",
        conditionMessage(w)
      ))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      population_log_mrgsolve(paste(
        "error en la llamada", getOption("pklab.mrgsolve_calls"), "-",
        conditionMessage(e)
      ))
    }
  )
  out <- out[ev$evid == 0L, , drop = FALSE]
  out[[observed]][match(paste(id, times), paste(out$ID, out$time))]
}

# Indica, para cada paciente, si una columna tiene el mismo valor en todas
# sus filas.
# Recibe: valores de la columna y paciente de cada fila.
# Devuelve: vector lógico con un elemento por paciente.
population_constant_by <- function(x, unit) {
  vapply(split(x, unit), function(z) length(unique(z)) == 1L, logical(1))
}

# Obtiene la dosis de cada individuo (en el orden de su ID): la de la
# columna de dosis, que debe ser constante dentro de cada paciente, o 0 si
# no hay (modelos sin compartimentos, que no usan dosis).
# Recibe: datos, identificador de cada fila y columna de dosis (o NULL).
# Devuelve: vector con la dosis de cada individuo.
population_dose_amounts <- function(fdata, id, dose_column = NULL) {
  k <- length(unique(id))
  if (is.null(dose_column)) {
    return(rep(0, k))
  }
  d <- fdata[[dose_column]]
  if (any(!is.finite(d) | d <= 0)) {
    stop("La columna de dosis debe ser positiva y sin valores ausentes.")
  }
  if (!all(population_constant_by(d, id))) {
    stop("La dosis debe ser constante dentro de cada paciente.")
  }
  as.numeric(d[match(seq_len(k), id)])
}

# Crea la función de esperanza f(phi_i, t_ij) de las fórmulas de nlme y
# nlsList. La función creada recibe los efectos de cada fila (t<param>,
# beta.<param>.<cov>, covariables) y las columnas ID y TIME, y toma los de
# la primera fila de cada individuo (nlsList pasa escalares, que se
# repiten).
# Recibe: estructura del modelo, parámetros estimados, términos de
# covariables, columnas de covariables, dosis de cada individuo,
# compartimento de la dosis y salida medida.
# Devuelve: función que calcula la predicción de cada fila.
population_expectation <- function(structure, estimated, terms, covcols,
                                   amounts, dose_cmt, observed) {
  theta <- paste0("t", tolower(estimated))
  betas <- vapply(terms, function(t) t$beta, character(1))
  function(...) {
    a <- list(...)
    n <- length(a$ID)
    first <- match(sort(unique(a$ID)), a$ID)
    coefs <- lapply(a[c(theta, betas)], function(v) rep_len(v, n)[first])
    cov <- lapply(a[covcols], function(v) v[first])
    p <- population_individual_params(structure, estimated, terms, coefs, cov)
    population_predict(structure, p, a$ID, a$TIME, amounts, dose_cmt,
                       observed)
  }
}

# Deja la función de esperanza como .pk en el entorno global mientras dura
# el ajuste, porque nlme y nlsList no la buscan en el entorno de la fórmula
# (quien la pone la quita con on.exit).
# Recibe: función de esperanza.
# Devuelve: entorno en el que se ha guardado.
population_set_expectation <- function(f) {
  fenv <- globalenv()
  assign(".pk", f, envir = fenv)
  fenv
}

# Construye la fórmula DV ~ .pk(a = a, ..., ID = ID, TIME = TIME), con
# "| ID" para nlsList.
# Recibe: nombres de los argumentos, entorno de la fórmula y si se agrupa
# por individuo.
# Devuelve: fórmula para nlme o nlsList.
population_expectation_formula <- function(args, fenv, by_id = FALSE) {
  args <- c(args, "ID", "TIME")
  stats::as.formula(
    sprintf(
      "DV ~ .pk(%s)%s",
      paste(sprintf("%s = %s", args, args), collapse = ", "),
      if (by_id) " | ID" else ""
    ),
    env = fenv
  )
}

# Pasa un valor de la escala natural a la del ajuste (log o natural).
# Recibe: estructura del modelo, nombre del parámetro y valor.
# Devuelve: valor en la escala del ajuste.
population_to_scale <- function(structure, p, v) {
  if (p %in% structure$log_params) log(v) else v
}

# Pasa un valor de la escala del ajuste a la natural.
# Recibe: estructura del modelo, nombre del parámetro y valor.
# Devuelve: valor en escala natural.
population_from_scale <- function(structure, p, v) {
  if (p %in% structure$log_params) exp(v) else v
}

# Calcula el valor de un término de covariable (en la escala del ajuste del
# parámetro) sin su beta.
# Recibe: término de covariable y valores de la covariable.
# Devuelve: valores transformados.
population_term_value <- function(term, x) {
  if (identical(term$kind, "categorical")) {
    return(x)
  }
  if (identical(term$form, "power")) {
    return(log(x / term$ref))
  }
  x - term$ref
}

# Calcula los parámetros individuales en escala natural a partir de los
# efectos de cada individuo.
# Recibe: estructura del modelo, parámetros estimados, términos de
# covariables, lista con theta (t<param>) y beta por individuo y columnas
# de covariables por individuo.
# Devuelve: data.frame con una fila por individuo y una columna por
# parámetro.
population_individual_params <- function(structure, estimated, terms, coefs,
                                         cov) {
  k <- length(coefs[[1]])
  out <- lapply(structure$params, function(p) {
    if (!p %in% estimated) {
      return(rep(structure$values[[p]], k))
    }
    lp <- coefs[[paste0("t", tolower(p))]]
    for (t in Filter(function(t) identical(t$param, p), terms)) {
      lp <- lp + coefs[[t$beta]] * population_term_value(t, cov[[t$internal]])
    }
    population_from_scale(structure, p, lp)
  })
  stats::setNames(as.data.frame(out), structure$params)
}

# Comprobaciones comunes a los ajustes individuales y al modelo mixto: el
# modelo se puede ajustar, la salida medida y el compartimento de la dosis
# son del modelo, queda algún parámetro por estimar y los valores iniciales
# son positivos.
# Recibe: estructura del modelo, salida medida (o NULL para la de por
# defecto), compartimento de la dosis y parámetros de valor fijado.
# Devuelve: lista con observed, estimated y values (valores iniciales).
population_fit_checks <- function(structure, observed, dose_cmt, fixed) {
  if (!isTRUE(structure$ok)) {
    stop("Este modelo no se puede ajustar todavía.")
  }
  if (is.null(observed) || !nzchar(observed)) {
    observed <- structure$observed
  }
  if (!observed %in% structure$outputs) {
    stop("La salida medida no pertenece al modelo.")
  }
  population_check_dose_cmt(structure, dose_cmt)
  estimated <- setdiff(structure$params, fixed)
  if (!length(estimated)) {
    stop("Debe quedar al menos un parámetro por estimar.")
  }
  values <- structure$values[structure$params]
  if (any(!is.finite(values)) || any(values <= 0)) {
    stop("Los valores iniciales de los parámetros deben ser positivos.")
  }
  list(observed = observed, estimated = estimated, values = values)
}

# Ajusta el modelo mixto no lineal por el algoritmo de Lindstrom y Bates
# (nlme::nlme, máxima verosimilitud). Los efectos fijos se llaman t<param>
# (valor típico en la escala del ajuste) y beta.<param>.<covariable>; los
# aleatorios van sobre los t<param>.
# Recibe: datos, estructura del modelo (population_structure()), columnas
# de concentración, paciente y tiempo, compartimento de administración,
# parámetros con efecto aleatorio, covariables
# ("COLUMNA|PARAMETRO"), error residual, forma de las covariables
# continuas, columna de dosis por paciente, salida medida, parámetros de
# valor conocido y estructura de Psi.
# Devuelve: lista con el ajuste de nlme y la información necesaria para
# construir las tablas de resultados.
population_fit_nlme <- function(data, structure, response, group, time,
                                dose_cmt, random, effects = character(),
                                error_model = "add", cov_form = "power",
                                dose_column = NULL, observed = NULL,
                                fixed = character(), omega = "diag") {

  checks <- population_fit_checks(structure, observed, dose_cmt, fixed)
  observed <- checks$observed
  estimated <- checks$estimated
  values <- checks$values
  if (!length(random)) {
    stop("Selecciona al menos un parámetro con efecto aleatorio.")
  }
  if (any(!random %in% estimated)) {
    stop("Un parámetro fijo no puede tener efecto aleatorio.")
  }
  if (!error_model %in% c("add", "prop", "comb")) {
    stop("Modelo de error residual no admitido.")
  }
  if (!omega %in% population_omega_structures) {
    stop("Estructura de la matriz de efectos aleatorios no admitida.")
  }

  fdata <- population_observations(data, response, group, time, dose_column)
  # Con error proporcional o combinado, la varianza depende de la
  # predicción, que vale 0 en t = 0 tras una dosis oral (peso infinito):
  # esas observaciones se excluyen.
  if (!identical(error_model, "add")) {
    fdata <- fdata[fdata[[time]] > 0, , drop = FALSE]
  }
  id <- population_individual_ids(fdata, group)
  amounts <- population_dose_amounts(fdata, id, dose_column)
  split <- population_split_inert(
    structure, estimated, random, fdata[[time]],
    stats::median(amounts), dose_cmt, observed
  )
  estimated <- split$estimated
  random <- population_check_random(split, random, observed)
  design <- population_covariate_design(
    fdata, id, effects, estimated, cov_form
  )

  theta <- paste0("t", tolower(estimated))
  betas <- vapply(design$terms, function(t) t$beta, character(1))
  covcols <- if (is.null(design$columns)) {
    character()
  } else {
    names(design$columns)
  }
  nd <- data.frame(
    ID = id,
    TIME = as.numeric(fdata[[time]]),
    DV = as.numeric(fdata[[response]])
  )
  if (length(covcols)) {
    nd <- cbind(nd, design$columns)
  }

  fenv <- population_set_expectation(
    population_expectation(
      structure, estimated, design$terms, covcols, amounts, dose_cmt,
      observed
    )
  )
  on.exit(rm(".pk", envir = fenv), add = TRUE)
  model_f <- population_expectation_formula(c(theta, betas, covcols), fenv)
  fixed_f <- stats::as.formula(
    sprintf("%s ~ 1", paste(c(theta, betas), collapse = " + "))
  )
  random_f <- stats::as.formula(
    sprintf("%s ~ 1", paste(paste0("t", tolower(random)), collapse = " + "))
  )
  pd <- if (identical(omega, "full") && length(random) > 1L) {
    nlme::pdSymm(random_f)
  } else {
    nlme::pdDiag(random_f)
  }
  # Error residual (Lambda_i): constante, proporcional a la predicción o
  # ambos.
  weights <- switch(
    error_model,
    add = NULL,
    prop = nlme::varPower(form = ~ fitted(.), fixed = 1),
    comb = nlme::varConstPower(form = ~ fitted(.), fixed = list(power = 1))
  )
  start <- c(
    stats::setNames(
      vapply(estimated, function(p) {
        population_to_scale(structure, p, values[[p]])
      }, numeric(1)),
      theta
    ),
    stats::setNames(rep(0, length(betas)), betas)
  )

  population_log_header(
    "Ajuste del modelo mixto (Lindstrom y Bates, nlme)", structure,
    c(
      sprintf(
        "Pacientes: %d · observaciones: %d", length(unique(id)), nrow(nd)
      ),
      sprintf("Salida medida: %s · dosis en: %s", observed, dose_cmt),
      sprintf("Parámetros estimados: %s", paste(estimated, collapse = ", ")),
      sprintf(
        "Efectos aleatorios: %s · Ψ %s", paste(random, collapse = ", "),
        if (identical(omega, "full")) "completa" else "diagonal"
      ),
      sprintf(
        "Error de medida: %s",
        names(population_error_models)[population_error_models == error_model]
      ),
      sprintf(
        "Covariables: %s",
        if (length(effects)) {
          paste(sub("|", " sobre ", effects, fixed = TRUE), collapse = ", ")
        } else {
          "ninguna"
        }
      )
    ),
    values[estimated]
  )
  cat("== Iteraciones de nlme ==\n")
  fit <- nlme::nlme(
    model_f,
    data = nd, fixed = fixed_f, random = pd,
    groups = ~ID, start = start, weights = weights, method = "ML",
    control = nlme::nlmeControl(msMaxIter = 200, returnObject = TRUE),
    # Escribe cada iteración (pasos PNLS y LME) en el registro del ajuste.
    verbose = TRUE
  )
  cat(
    "\n== El ajuste ha terminado ==\n",
    "mrgsolve ha resuelto el modelo ", getOption("pklab.mrgsolve_calls"),
    " veces.\n",
    sep = ""
  )

  first <- match(seq_along(unique(id)), id)
  list(
    fit = fit,
    structure = structure,
    estimated = estimated,
    terms = design$terms,
    random = random,
    omega = omega,
    error_model = error_model,
    observed = observed,
    n_units = length(unique(id)),
    labels = levels(factor(fdata[[group]])),
    covariates = if (length(covcols)) {
      design$columns[first, , drop = FALSE]
    } else {
      NULL
    },
    response = response,
    group = group,
    time = time
  )
}

# Comprueba los efectos aleatorios que quedan tras quitar los parámetros que
# no influyen en la salida; lanza un error si no queda ninguno.
# Recibe: resultado de population_split_inert(), efectos aleatorios pedidos
# y salida medida.
# Devuelve: vector con los efectos aleatorios que se pueden estimar.
population_check_random <- function(split, random, observed) {
  if (!length(split$random)) {
    stop(sprintf(
      paste(
        "Los parámetros con efecto aleatorio (%s) no influyen en %s con",
        "esta dosis: elige otros."
      ),
      paste(random, collapse = ", "), observed
    ))
  }
  split$random
}

# Ajusta el modelo y se queda solo con lo que muestra la aplicación (el
# objeto del ajuste es grande y no sale del proceso que lo calcula).
# Recibe: los mismos argumentos que population_fit_nlme().
# Devuelve: lista con las tablas de efectos fijos, variabilidad y
# estimaciones individuales, los criterios del ajuste y el texto del
# modelo.
population_fit_tables <- function(...) {
  res <- population_fit_nlme(...)
  list(
    fixed = population_fixed_table(res),
    variability = population_variability_table(res),
    individual = population_individual_table(res),
    summary = population_summary_text(res),
    n_units = res$n_units
  )
}

# Ajusta el modelo en un proceso de R aparte (population_fit_tables()). La
# resolución del modelo con mrgsolve y el propio ajuste quedan fuera de la
# sesión de Shiny: si fallan de forma grave, solo cae el proceso hijo, y el
# objeto del ajuste no ocupa memoria en la aplicación.
# Recibe: los mismos argumentos que population_fit_nlme().
# Devuelve: el resultado de population_fit_tables().
population_fit_isolated <- function(...) {
  population_run_isolated("population_fit_tables", list(...))
}

# Explica en español los errores más habituales de nlme: qué ha pasado y
# qué se puede probar. Los errores de la aplicación, ya en español, se
# dejan como están.
# Recibe: mensaje del error.
# Devuelve: lista con cause, hints (vector, puede estar vacío) y r_message
# (el mensaje original de R, o NULL si es de la aplicación).
population_explain_error <- function(msg) {
  starts <- paste(
    "Ajusta cada paciente por separado y pulsa «Usar la mediana como",
    "valores iniciales»."
  )
  simpler <- "Prueba con Ψ diagonal o con menos efectos aleatorios."
  known <- list(
    list(
      pattern = "singular",
      cause = paste(
        "El algoritmo se ha detenido porque una de sus matrices es singular",
        "(no se puede invertir). Suele pasar con valores iniciales alejados",
        "de los datos o con más efectos aleatorios de los que los datos",
        "permiten estimar."
      ),
      hints = c(starts, simpler)
    ),
    list(
      pattern = "step halving",
      cause = paste(
        "El paso PNLS no ha conseguido mejorar el ajuste: el algoritmo ha",
        "reducido el paso al mínimo sin avanzar."
      ),
      hints = c(
        starts, "Prueba con otro modelo de error o con menos covariables."
      )
    ),
    list(
      pattern = "iterations|converge",
      cause = paste(
        "El algoritmo no ha convergido en el número máximo de",
        "iteraciones."
      ),
      hints = c(starts, simpler)
    ),
    list(
      pattern = "missing|NA/NaN|non-finite|Inf",
      cause = paste(
        "El modelo ha dado valores no válidos con los parámetros que probaba",
        "el algoritmo."
      ),
      hints = c(
        starts,
        "Revisa en el registro qué parámetros recibió mrgsolve."
      )
    )
  )
  for (k in known) {
    if (grepl(k$pattern, msg, ignore.case = TRUE)) {
      return(list(cause = k$cause, hints = k$hints, r_message = msg))
    }
  }
  list(cause = msg, hints = character(), r_message = NULL)
}

# Ejecuta una función de este archivo en un proceso de R aparte. Si falla,
# lanza un error (de clase population_error) con un mensaje para el
# usuario y, en su campo log, el registro del proceso.
# Recibe: nombre de la función y lista con sus argumentos.
# Devuelve: el resultado de la función, con el registro del proceso en el
# atributo "log".
population_run_isolated <- function(fun, args) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "Falta instalar callr. En R ejecuta install.packages('callr') ",
      "y reinicia Shiny."
    )
  }
  # El proceso hijo no tiene las funciones de la aplicación: se le pasan
  # todas las de este archivo (population_*).
  env <- environment(population_fit_nlme)
  helpers <- mget(ls(env, pattern = "^population_"), envir = env)
  # Todo lo que escribe el proceso (iteraciones, mensajes y avisos) se
  # guarda como registro, que el usuario puede leer si el ajuste falla.
  log_file <- tempfile("pklab_ajuste_", fileext = ".txt")
  on.exit(unlink(log_file), add = TRUE)
  read_log <- function(extra = character()) {
    lines <- if (file.exists(log_file)) {
      readLines(log_file, warn = FALSE)
    } else {
      character()
    }
    c(
      sprintf("PKLaboratory · %s · %s", fun, format(Sys.time())),
      "", lines, extra
    )
  }
  # Error con la causa para el usuario, qué puede probar, el mensaje
  # original de R y el registro del proceso.
  fail <- function(msg) {
    x <- population_explain_error(msg)
    stop(structure(
      class = c("population_error", "error", "condition"),
      list(
        message = paste(c(x$cause, x$hints), collapse = " "), call = NULL,
        cause = x$cause, hints = x$hints, r_message = x$r_message,
        log = read_log(c("", "== Error ==", msg))
      )
    ))
  }
  result <- tryCatch(
    callr::r(
      function(helpers, fun, args) {
        options(warn = 1)
        list2env(helpers, globalenv())
        do.call(fun, args)
      },
      args = list(helpers = helpers, fun = fun, args = args),
      stdout = log_file, stderr = "2>&1"
    ),
    error = function(e) {
      cause <- e$parent
      if (inherits(cause, "condition")) {
        fail(conditionMessage(cause))
      }
      fail(paste(
        "El proceso de estimación se ha interrumpido de forma inesperada.",
        "Prueba otra vez o con un modelo de error distinto."
      ))
    }
  )
  attr(result, "log") <- read_log()
  result
}

# ------------------------------------------------------------
# Ajustes individuales (exploratorio, con nlsList() de nlme)
# ------------------------------------------------------------

# Ajusta el modelo por separado a cada paciente con nlme::nlsList(), por
# mínimos cuadrados no lineales y sin efectos aleatorios, como hacen
# Pinheiro y Bates antes del modelo mixto (fm1Theo.lis). Los parámetros se
# estiman en la escala de cada uno (logarítmica o natural) y, como
# intervals() de nlsList, sus errores estándar usan la desviación residual
# conjunta de todos los pacientes (pool), con su IC del 95 % (t de
# Student), el estadístico t y el p-valor del contraste H0: beta = 0
# (log(parámetro) = 0 en escala log). Si el modelo usa CL y un volumen
# central V en escala log, se añade KE = CL / V (log KE = log CL − log V,
# con su error estándar).
# nlsList necesita un único valor inicial para todos: se toma el ajuste
# conjunto de todos los pacientes, que no depende de que los valores del
# modelo estén en la escala de los datos.
# Recibe: datos, estructura del modelo, columnas de concentración, paciente
# y tiempo, compartimento de administración y, opcionalmente, columna de
# dosis, salida medida y parámetros de valor conocido.
# Devuelve: data.frame con Paciente, Parametro, Estimacion, EE,
# IC95_inferior, IC95_superior, t y p (en la escala del ajuste); NA en los
# pacientes cuyo ajuste no converge. El atributo "inert" lista los
# parámetros que no influyen en la salida y no se han estimado, y
# "log_params" los que están en escala logarítmica.
population_individual_fits <- function(data, structure, response, group,
                                       time, dose_cmt, dose_column = NULL,
                                       observed = NULL, fixed = character()) {

  checks <- population_fit_checks(structure, observed, dose_cmt, fixed)
  observed <- checks$observed
  estimated <- checks$estimated
  values <- checks$values
  fdata <- population_observations(data, response, group, time, dose_column)
  id <- population_individual_ids(fdata, group)
  labels <- levels(factor(fdata[[group]]))
  amount <- population_dose_amounts(fdata, id, dose_column)
  split <- population_split_inert(
    structure, estimated, character(), fdata[[time]],
    stats::median(amount), dose_cmt, observed
  )
  estimated <- split$estimated
  theta <- paste0("t", tolower(estimated))
  nd <- data.frame(
    ID = id,
    TIME = as.numeric(fdata[[time]]),
    DV = as.numeric(fdata[[response]])
  )
  f <- population_expectation(
    structure, estimated, list(), character(), amount, dose_cmt, observed
  )

  # Valor inicial común: ajuste conjunto de todos los pacientes.
  pooled_sse <- function(lp) {
    pred <- do.call(
      f,
      c(as.list(stats::setNames(lp, theta)), list(ID = nd$ID, TIME = nd$TIME))
    )
    r <- nd$DV - pred
    if (any(!is.finite(r))) 1e100 else sum(r^2)
  }
  pooled <- vapply(estimated, function(p) {
    population_to_scale(structure, p, values[[p]])
  }, numeric(1))
  # Nelder-Mead se reinicia hasta 3 veces, mientras siga mejorando.
  best <- pooled_sse(pooled)
  for (i in 1:3) {
    opt <- tryCatch(
      stats::optim(pooled, pooled_sse, control = list(maxit = 2000)),
      error = function(e) NULL
    )
    if (is.null(opt) || opt$value >= best * (1 - 1e-6)) {
      break
    }
    pooled <- opt$par
    best <- opt$value
  }

  fenv <- population_set_expectation(f)
  on.exit(rm(".pk", envir = fenv), add = TRUE)
  population_log_header(
    "Ajustes individuales (nlsList)", structure,
    c(
      sprintf("Pacientes: %d · observaciones: %d", length(labels), nrow(nd)),
      sprintf("Salida medida: %s · dosis en: %s", observed, dose_cmt),
      sprintf("Parámetros estimados: %s", paste(estimated, collapse = ", "))
    ),
    values[estimated]
  )
  cat("== Ajustes de cada paciente ==\n")
  lis <- suppressWarnings(nlme::nlsList(
    population_expectation_formula(theta, fenv, by_id = TRUE),
    data = nd,
    start = stats::setNames(as.list(pooled), theta),
    control = stats::nls.control(maxiter = 200, warnOnly = TRUE)
  ))

  # Ajustes válidos: con errores estándar finitos y razonables (si no, el
  # parámetro no está identificado con los datos de ese paciente).
  fits <- lapply(seq_along(labels), function(k) {
    fit <- lis[[as.character(k)]]
    if (is.null(fit)) {
      return(NULL)
    }
    s <- tryCatch(summary(fit), error = function(e) NULL)
    if (is.null(s)) {
      return(NULL)
    }
    se <- s$coefficients[, "Std. Error"]
    if (any(!is.finite(se)) || any(se > 100)) {
      cat(
        "[nlsList] paciente ", labels[[k]],
        ": errores estándar no finitos o desmesurados; no se usa su ajuste\n",
        sep = ""
      )
      return(NULL)
    }
    list(
      est = stats::coef(fit),
      unscaled = s$cov.unscaled,
      rss = sum(stats::residuals(fit)^2),
      df = s$df[[2]]
    )
  })
  valid <- Filter(Negate(is.null), fits)
  df <- sum(vapply(valid, function(x) x$df, numeric(1)))
  sp2 <- if (df > 0) {
    sum(vapply(valid, function(x) x$rss, numeric(1))) / df
  } else {
    NA_real_
  }

  vc <- structure$central_volume
  derived <- !is.null(vc) &&
    all(c("CL", vc) %in% intersect(estimated, structure$log_params))
  shown <- c(estimated, if (derived) sprintf("KE = CL/%s", vc))
  rows <- lapply(seq_along(labels), function(k) {
    x <- fits[[k]]
    if (is.null(x) || !is.finite(sp2)) {
      return(data.frame(
        Paciente = labels[[k]], Parametro = shown,
        Estimacion = NA_real_, EE = NA_real_, IC95_inferior = NA_real_,
        IC95_superior = NA_real_, t = NA_real_, p = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    est <- x$est[theta]
    v <- x$unscaled[theta, theta, drop = FALSE] * sp2
    se <- sqrt(diag(v))
    if (derived) {
      # log KE = log CL − log V; su varianza sale de la matriz de
      # covarianzas.
      g <- stats::setNames(rep(0, length(theta)), theta)
      g[paste0("t", tolower(c("CL", vc)))] <- c(1, -1)
      est <- c(est, sum(g * est))
      se <- c(se, sqrt(drop(t(g) %*% v %*% g)))
    }
    half <- stats::qt(0.975, df = df) * se
    tval <- est / se
    data.frame(
      Paciente = labels[[k]], Parametro = shown, Estimacion = unname(est),
      EE = unname(se), IC95_inferior = unname(est - half),
      IC95_superior = unname(est + half), t = unname(tval),
      p = unname(2 * stats::pt(-abs(tval), df = df)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "inert") <- split$inert
  attr(out, "log_params") <- c(
    intersect(estimated, structure$log_params),
    if (derived) sprintf("KE = CL/%s", vc)
  )
  out
}

# Comprueba que el compartimento de la dosis pertenece al modelo (los
# modelos sin compartimentos, como los $PRED, se ajustan sin dosis).
# Recibe: estructura del modelo y compartimento de la dosis.
population_check_dose_cmt <- function(structure, dose_cmt) {
  if (!length(structure$cmts)) {
    return(invisible())
  }
  if (length(dose_cmt) != 1L || !dose_cmt %in% structure$cmts) {
    stop("Elige un compartimento de administración que pertenezca al modelo.")
  }
}

# Separa los parámetros que no influyen en la salida medida con la dosis y
# los tiempos de los datos (por ejemplo, KA2 si no se dosifica EV2, o KM si
# VMAX = 0): estimarlos haría el modelo no identificable, así que se dejan
# en su valor. Se resuelve el modelo con los valores iniciales y con cada
# parámetro un 10 % mayor.
# Recibe: estructura del modelo, parámetros estimados, parámetros con
# efecto aleatorio, tiempos de los datos, dosis, compartimento de la dosis
# y salida medida.
# Devuelve: lista con estimated y random sin esos parámetros, e inert.
population_split_inert <- function(structure, estimated, random, times, dose,
                                   dose_cmt, observed) {

  times <- sort(unique(times[times > 0]))
  if (!length(times)) {
    return(list(estimated = estimated, random = random, inert = character()))
  }
  values <- structure$values[structure$params]
  pred <- function(v) {
    population_predict(
      structure, as.data.frame(as.list(v)), rep(1L, length(times)), times,
      dose, dose_cmt, observed
    )
  }
  base <- pred(values)
  scale <- suppressWarnings(max(abs(base), na.rm = TRUE))
  if (!is.finite(scale) || scale == 0) {
    stop(sprintf(
      paste(
        "Con la dosis en %s, la salida %s vale 0 en todos los tiempos:",
        "revisa el compartimento de la dosis y la salida medida."
      ),
      dose_cmt, observed
    ))
  }
  inert <- Filter(function(p) {
    v <- values
    v[[p]] <- v[[p]] * 1.1
    d <- pred(v)
    all(is.finite(d)) && max(abs(d - base)) / scale < 1e-6
  }, estimated)
  inert <- as.character(unlist(inert))
  estimated <- setdiff(estimated, inert)
  if (!length(estimated)) {
    stop(sprintf(
      "Ningún parámetro influye en %s con la dosis en %s.",
      observed, dose_cmt
    ))
  }
  list(estimated = estimated, random = setdiff(random, inert), inert = inert)
}

# Calcula los valores iniciales para el modelo mixto a partir de los
# ajustes individuales: la mediana de las estimaciones de cada parámetro
# (Pinheiro y Bates parten de nlsList() para ajustar nlme()).
# Recibe: resultado de population_individual_fits().
# Devuelve: vector con la mediana de cada parámetro en escala natural.
population_individual_starts <- function(fits) {
  ok <- fits[is.finite(fits$Estimacion), , drop = FALSE]
  if (!nrow(ok)) {
    return(numeric())
  }
  med <- tapply(ok$Estimacion, ok$Parametro, stats::median)
  logs <- names(med) %in% attr(fits, "log_params")
  med[logs] <- exp(med[logs])
  med
}

# ------------------------------------------------------------
# Resultados: solo intervalos de confianza y desviaciones
# ------------------------------------------------------------

# Obtiene las estimaciones de los efectos fijos (escala del modelo) con su
# error estándar, leídas de la tabla del ajuste de nlme.
# Recibe: ajuste de nlme.
# Devuelve: data.frame con nombre, estimación y error estándar.
population_fit_thetas <- function(fit) {
  tt <- summary(fit)$tTable
  data.frame(
    name = rownames(tt), est = tt[, "Value"], se = tt[, "Std.Error"],
    stringsAsFactors = FALSE
  )
}

# Calcula el IC del 95 % (Wald) en la escala del modelo y, si procede, lo
# pasa a la escala natural.
# Recibe: estimación, error estándar y si está en escala logarítmica.
# Devuelve: vector con la estimación y los límites inferior y superior.
population_wald <- function(est, se, log_scale) {
  z <- stats::qnorm(0.975)
  lo <- est - z * se
  hi <- est + z * se
  if (log_scale) c(exp(est), exp(lo), exp(hi)) else c(est, lo, hi)
}

# Construye la tabla de parámetros poblacionales con IC del 95 %. Los
# valores típicos se estiman en la escala de cada parámetro (beta:
# logarítmica, como lKe, lKa o lCl en Pinheiro y Bates, o natural, como A1
# y A2 en Indometh) y se devuelven también en su escala natural (CL, V,
# KA...). Los coeficientes de las covariables solo tienen una escala.
# Recibe: resultado de population_fit_nlme().
# Devuelve: data.frame con Parametro, Beta, Estimacion, IC95_inferior e
# IC95_superior.
population_fixed_table <- function(res) {
  th <- population_fit_thetas(res$fit)
  rows <- list()
  add_row <- function(name, label, log_scale) {
    r <- th[th$name == name, , drop = FALSE]
    if (!nrow(r)) {
      return()
    }
    v <- population_wald(r$est[[1]], r$se[[1]], log_scale)
    rows[[length(rows) + 1L]] <<- data.frame(
      Parametro = label, Beta = r$est[[1]], Estimacion = v[[1]],
      IC95_inferior = v[[2]], IC95_superior = v[[3]],
      stringsAsFactors = FALSE
    )
  }
  # Etiqueta de un parámetro con su unidad, si se reconoce.
  with_unit <- function(p) {
    u <- population_param_unit(p)
    if (nzchar(u)) sprintf("%s (%s)", p, u) else p
  }
  for (p in res$estimated) {
    add_row(
      paste0("t", tolower(p)), paste("Valor típico de", with_unit(p)),
      p %in% res$structure$log_params
    )
  }
  for (t in res$terms) {
    add_row(t$beta, population_term_label(t), FALSE)
  }

  # Con CL y volumen central, Ke típica = CL / V: log KE = β_CL − β_V, con
  # su error estándar a partir de la matriz de covarianzas de los efectos
  # fijos.
  vc <- res$structure$central_volume
  log_estimated <- intersect(res$estimated, res$structure$log_params)
  if (!is.null(vc) && all(c("CL", vc) %in% log_estimated)) {
    a <- "tcl"
    b <- paste0("t", tolower(vc))
    cv <- res$fit$varFix
    if (all(c(a, b) %in% th$name)) {
      est <- th$est[th$name == a] - th$est[th$name == b]
      se <- if (!is.null(cv) && all(c(a, b) %in% rownames(cv))) {
        sqrt(cv[a, a] + cv[b, b] - 2 * cv[a, b])
      } else {
        NA_real_
      }
      th <- rbind(th, data.frame(name = ".ke", est = est, se = se))
      add_row(
        ".ke", sprintf("Valor típico de KE (1/h) = CL / %s (derivado)", vc),
        TRUE
      )
    }
  }
  if (!length(rows)) {
    stop("No se han podido extraer los parámetros del ajuste.")
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Construye la tabla de desviaciones típicas de la variabilidad aleatoria y
# del error residual. La variabilidad es la desviación típica del efecto
# aleatorio en la escala del parámetro; en escala logarítmica, CV es el
# coeficiente de variación lognormal asociado. Si Psi es completa se añaden
# las correlaciones entre efectos aleatorios. Los intervalos de confianza
# son los aproximados de intervals() de nlme.
# Recibe: resultado de population_fit_nlme().
# Devuelve: data.frame con Fuente, Desviacion, CV_pct, IC95_inferior e
# IC95_superior.
population_variability_table <- function(res) {
  rows <- list()
  ci <- population_nlme_intervals(res$fit)

  # Si una desviación es prácticamente 0 (como lKe en Theoph), nlme da un
  # intervalo de muchos órdenes de magnitud que no informa de nada: se
  # omite.
  ci_of <- function(name) {
    if (is.null(ci$re) || !name %in% rownames(ci$re)) {
      return(c(NA_real_, NA_real_))
    }
    v <- c(ci$re[name, "lower"], ci$re[name, "upper"])
    if (startsWith(name, "sd(") &&
          (any(!is.finite(v)) || v[[2]] / v[[1]] > 1e3)) {
      return(c(NA_real_, NA_real_))
    }
    v
  }

  # Psi en la escala de los parámetros: la matriz relativa (D) de nlme por
  # sigma^2, como sigma^2 D en Lindstrom y Bates.
  om <- tryCatch(
    nlme::pdMatrix(res$fit$modelStruct$reStruct)[[1]] * res$fit$sigma^2,
    error = function(e) NULL
  )
  if (!is.null(om) && is.numeric(om) && nrow(om) == ncol(om) &&
        length(om)) {
    vars <- diag(om)
    labs <- rownames(om)
    names_of <- population_random_names(labs, res$structure$params)
    for (i in seq_along(vars)) {
      v <- ci_of(sprintf("sd(%s)", labs[[i]]))
      is_log <- names_of[[i]] %in% res$structure$log_params
      rows[[length(rows) + 1L]] <- data.frame(
        Fuente = paste("Variabilidad entre pacientes en", names_of[[i]]),
        Desviacion = sqrt(max(vars[[i]], 0)),
        CV_pct = if (vars[[i]] >= 0 && is_log) {
          100 * sqrt(expm1(vars[[i]]))
        } else {
          NA_real_
        },
        IC95_inferior = v[[1]], IC95_superior = v[[2]],
        stringsAsFactors = FALSE
      )
    }
    if (identical(res$omega, "full") && length(vars) > 1L) {
      for (i in seq_len(length(vars) - 1L)) {
        for (j in (i + 1L):length(vars)) {
          denom <- sqrt(vars[[i]] * vars[[j]])
          v <- ci_of(sprintf("cor(%s,%s)", labs[[i]], labs[[j]]))
          rows[[length(rows) + 1L]] <- data.frame(
            Fuente = sprintf(
              "Correlación entre los efectos de %s y %s",
              names_of[[i]], names_of[[j]]
            ),
            Desviacion = if (is.finite(denom) && denom > 0) {
              om[i, j] / denom
            } else {
              NA_real_
            },
            CV_pct = NA_real_, IC95_inferior = v[[1]], IC95_superior = v[[2]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  resid <- c(
    add.sd = "Error residual aditivo (unidades de la salida medida)",
    prop.sd = "Error residual proporcional (fracción; 0,1 = 10 %)"
  )
  add_resid <- function(nm, v) {
    rows[[length(rows) + 1L]] <<- data.frame(
      Fuente = resid[[nm]], Desviacion = v[[1]], CV_pct = NA_real_,
      IC95_inferior = max(v[[2]], 0), IC95_superior = v[[3]],
      stringsAsFactors = FALSE
    )
  }
  # sigma es la DE residual (aditivo) o el coeficiente proporcional; con
  # error combinado, la parte aditiva es sigma * const.
  s <- res$fit$sigma
  s_ci <- if (is.null(ci$sigma)) {
    c(NA_real_, NA_real_)
  } else {
    ci$sigma[c("lower", "upper")]
  }
  switch(
    res$error_model,
    add = add_resid("add.sd", c(s, s_ci)),
    prop = add_resid("prop.sd", c(s, s_ci)),
    comb = {
      const <- tryCatch(
        stats::coef(
          res$fit$modelStruct$varStruct, unconstrained = FALSE
        )[["const"]],
        error = function(e) NA_real_
      )
      add_resid("add.sd", c(s * const, NA_real_, NA_real_))
      add_resid("prop.sd", c(s, NA_real_, NA_real_))
    }
  )
  if (!length(rows)) {
    stop("No se han podido extraer las desviaciones del ajuste.")
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Calcula los intervalos aproximados de nlme
# (intervals(..., which = "var-cov")).
# Recibe: ajuste de nlme.
# Devuelve: lista con la matriz re (filas "sd(tx)" y "cor(tx,ty)" y
# columnas lower/est./upper) y sigma (error residual), o NULL si nlme no ha
# podido calcularlos (por ejemplo, si una desviación es prácticamente 0).
population_nlme_intervals <- function(fit) {
  iv <- tryCatch(
    nlme::intervals(fit, which = "var-cov"),
    error = function(e) NULL
  )
  if (is.null(iv)) {
    return(NULL)
  }
  list(re = as.matrix(iv$reStruct[[1]]), sigma = iv$sigma)
}

# Obtiene el parámetro del modelo al que corresponde cada efecto aleatorio
# ("tcl" es el de CL).
# Recibe: nombres de los efectos aleatorios y parámetros del modelo.
# Devuelve: vector con el nombre del parámetro de cada efecto.
population_random_names <- function(labs, params) {
  vapply(labs, function(l) {
    p <- params[match(sub("^t", "", l), tolower(params))]
    if (is.na(p)) l else p
  }, character(1), USE.NAMES = FALSE)
}

# Construye la tabla de efectos aleatorios estimados de cada paciente (b_i,
# como ranef() en nlme) y sus parámetros individuales en escala natural,
# con el identificador original del paciente.
# Recibe: resultado de population_fit_nlme().
# Devuelve: data.frame con una fila por paciente.
population_individual_table <- function(res) {
  re <- as.data.frame(nlme::ranef(res$fit))
  co <- as.data.frame(stats::coef(res$fit))
  ids <- as.integer(rownames(co))
  cov <- if (is.null(res$covariates)) {
    list()
  } else {
    as.list(res$covariates[ids, , drop = FALSE])
  }
  ind <- population_individual_params(
    res$structure, res$estimated, res$terms, as.list(co), cov
  )
  re <- re[as.character(ids), , drop = FALSE]
  names(re) <- paste0(
    "b_", population_random_names(names(re), res$structure$params)
  )
  out <- data.frame(
    Paciente = res$labels[ids], re, ind,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- out[order(ids), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Nombres legibles de los coeficientes del ajuste: los efectos fijos de
# los parámetros (lCL si está en escala log, como lCl en Pinheiro y Bates,
# o CL) y los de las covariables (CL.Wt, KA.SEX=M...), en lugar de los
# nombres internos (tcl, beta.cl.cov1...).
# Recibe: resultado de population_fit_nlme().
# Devuelve: vector con el nombre legible de cada nombre interno.
population_coef_names <- function(res) {
  pars <- res$estimated
  readable <- ifelse(
    pars %in% res$structure$log_params, paste0("l", pars), pars
  )
  terms <- vapply(res$terms, function(t) {
    if (identical(t$kind, "categorical")) {
      sprintf("%s.%s=%s", t$param, t$col, t$level)
    } else {
      sprintf("%s.%s", t$param, t$col)
    }
  }, character(1))
  c(
    stats::setNames(readable, paste0("t", tolower(pars))),
    stats::setNames(terms, vapply(res$terms, `[[`, character(1), "beta"))
  )
}

# Escribe el resumen del ajuste como summary() de nlme, con los nombres
# legibles de los coeficientes, el de la columna del paciente y el de la
# salida medida.
# Recibe: resultado de population_fit_nlme().
# Devuelve: vector de texto con una línea por línea del resumen.
population_summary_text <- function(res) {
  s <- summary(res$fit)
  names_map <- population_coef_names(res)
  rename <- function(x) {
    hit <- x %in% names(names_map)
    x[hit] <- names_map[x[hit]]
    x
  }
  rownames(s$tTable) <- rename(rownames(s$tTable))
  if (!is.null(s$corFixed)) {
    dimnames(s$corFixed) <- lapply(dimnames(s$corFixed), rename)
  }
  out <- utils::capture.output(print(s))
  # Los nombres internos que quedan (fórmulas de los efectos aleatorios y
  # sus desviaciones) se sustituyen en el texto.
  for (k in names(names_map)) {
    out <- gsub(
      paste0("\\b\\Q", k, "\\E\\b"), names_map[[k]], out, perl = TRUE
    )
  }
  out <- sub(
    "^(\\s*Model: ).*$",
    sprintf(
      "\\1%s ~ %s(%s, %s)", res$response, res$observed, res$time,
      paste(names_map[paste0("t", tolower(res$estimated))], collapse = ", ")
    ),
    out
  )
  out <- sub("^(\\s*Data: ).*$", "\\1CSV subido", out)
  out <- sub("^(Fixed effects:).*$", "\\1", out)
  out <- sub("^(\\s*Level: )ID$", paste0("\\1", res$group), out)
  out
}

# Pone los nombres de columna en español a la tabla de efectos fijos.
# Recibe: tabla de population_fixed_table().
# Devuelve: la misma tabla con los nombres para mostrar.
population_display_fixed <- function(tab) {
  names(tab) <- c(
    "Parámetro", "β (escala del ajuste)", "Estimación",
    "IC 95 % inferior", "IC 95 % superior"
  )
  tab
}

# Pone los nombres de columna en español a la tabla de variabilidad.
# Recibe: tabla de population_variability_table().
# Devuelve: la misma tabla con los nombres para mostrar.
population_display_variability <- function(tab) {
  names(tab) <- c(
    "Fuente de variabilidad", "Desviación típica o correlación", "CV (%)",
    "IC 95 % inferior", "IC 95 % superior"
  )
  tab
}

# Une las tablas de efectos fijos y de variabilidad para exportarlas en un
# único CSV.
# Recibe: tabla de efectos fijos y tabla de variabilidad.
# Devuelve: data.frame con las filas de las dos tablas.
population_results_export <- function(fixed, variability) {
  a <- data.frame(
    Tipo = "Parámetro poblacional",
    Nombre = fixed$Parametro,
    Estimacion_o_desviacion = fixed$Estimacion,
    CV_pct = NA_real_,
    IC95_inferior = fixed$IC95_inferior,
    IC95_superior = fixed$IC95_superior,
    stringsAsFactors = FALSE
  )
  b <- data.frame(
    Tipo = ifelse(
      startsWith(variability$Fuente, "Correlación"),
      "Correlación", "Desviación típica"
    ),
    Nombre = variability$Fuente,
    Estimacion_o_desviacion = variability$Desviacion,
    CV_pct = variability$CV_pct,
    IC95_inferior = variability$IC95_inferior,
    IC95_superior = variability$IC95_superior,
    stringsAsFactors = FALSE
  )
  rbind(a, b)
}
