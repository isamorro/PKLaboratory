# ============================================================
# Archivo: population.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para leer los datos poblacionales de un CSV y
# detectar las columnas que pueden servir como covariables (sin
# dependencias de Shiny). El ajuste de modelos mixtos no
# lineales está en population_nlme.R.
# ============================================================

# ------------------------------------------------------------
# Datos de un CSV
# ------------------------------------------------------------

# Lee el CSV con los datos de los pacientes. El separador se detecta en la
# primera línea: ";" (con decimales con coma, como el CSV de Excel en
# español) o ",". Se quita la columna con el número de fila que añaden
# write.csv() de R o pandas (rownames, X, ...1, Unnamed: 0).
# Recibe: ruta del archivo.
# Devuelve: data.frame con los datos.
population_read_csv <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE)
  semicolon <- length(first) &&
    lengths(regmatches(first, gregexpr(";", first))) >
      lengths(regmatches(first, gregexpr(",", first)))
  dat <- if (semicolon) utils::read.csv2(path) else utils::read.csv(path)
  if (nrow(dat) < 10L || nrow(dat) > 200000L) {
    stop("El CSV debe contener entre 10 y 200000 filas.")
  }
  index_names <- c("ROWNAMES", "X", "...1", "UNNAMED..0", "")
  index <- toupper(names(dat)) %in% index_names &
    vapply(dat, population_is_row_index, logical(1))
  dat[, !index, drop = FALSE]
}

# Indica si una columna solo numera las filas (1, 2, 3...).
# Recibe: columna.
# Devuelve: TRUE o FALSE.
population_is_row_index <- function(x) {
  is.numeric(x) && isTRUE(all(x == seq_along(x)))
}

# Unidades de los datos y su factor de conversión a las unidades con las
# que se trabaja: horas, mg y mg/L. Así, si el modelo calcula la
# concentración como cantidad / volumen (CP = CENT / V), CL sale en L/h,
# los volúmenes en L y las constantes de velocidad en 1/h.
population_time_units <- c("h" = 1, "min" = 1 / 60, "días" = 24)
population_conc_units <- c(
  "mg/L" = 1, "µg/mL" = 1, "ng/mL" = 1e-3, "µg/L" = 1e-3, "mg/mL" = 1e3,
  "g/L" = 1e3
)
# mg/kg se multiplica además por el peso (en kg) de cada paciente.
population_dose_units <- c("mg" = 1, "µg" = 1e-3, "g" = 1e3, "mg/kg" = 1)

# Columna interna con la dosis de cada paciente en mg.
population_dose_mg <- ".DOSIS_MG"

# Pasa el tiempo, la concentración y la dosis a horas, mg/L y mg, y guarda
# la dosis de cada fila en la columna population_dose_mg (si hay columna de
# dosis).
# Recibe: datos, columnas de tiempo, concentración y dosis (o NULL),
# unidades de tiempo, concentración y dosis, y columna del peso en kg (solo
# con mg/kg).
# Devuelve: datos con esas columnas convertidas.
population_convert_units <- function(dat, time, response, dose_column = NULL,
                                     time_unit = "h", conc_unit = "mg/L",
                                     dose_unit = "mg", weight = NULL) {
  if (!time_unit %in% names(population_time_units) ||
        !conc_unit %in% names(population_conc_units) ||
        !dose_unit %in% names(population_dose_units)) {
    stop("Unidades no admitidas.")
  }
  if (!is.numeric(dat[[time]]) || !is.numeric(dat[[response]])) {
    stop("Selecciona columnas numéricas para el tiempo y la concentración.")
  }
  dat[[time]] <- dat[[time]] * population_time_units[[time_unit]]
  dat[[response]] <- dat[[response]] * population_conc_units[[conc_unit]]
  if (is.null(dose_column)) {
    return(dat)
  }
  if (!is.numeric(dat[[dose_column]])) {
    stop("La columna de dosis debe ser numérica.")
  }
  dose <- dat[[dose_column]]
  if (identical(dose_unit, "mg/kg")) {
    if (is.null(weight) || !weight %in% names(dat) ||
          !is.numeric(dat[[weight]]) ||
          any(!is.finite(dat[[weight]]) | dat[[weight]] <= 0)) {
      stop(
        "Con la dosis en mg/kg, elige una columna de peso (kg) positiva y ",
        "sin valores ausentes."
      )
    }
    dose <- dose * dat[[weight]]
  } else {
    dose <- dose * population_dose_units[[dose_unit]]
  }
  dat[[population_dose_mg]] <- dose
  dat
}

# Unidad de un parámetro según su nombre, con los datos en h, mg y mg/L:
# aclaramientos (CL, Q...) en L/h, volúmenes (V, V2, VC...) en L y
# constantes de velocidad (KA, KE, K10...) en 1/h.
# Recibe: nombre del parámetro.
# Devuelve: la unidad, o "" si no se reconoce.
population_param_unit <- function(p) {
  p <- toupper(p)
  if (grepl("^(CL|Q)[0-9A-Z]*$", p)) {
    "L/h"
  } else if (grepl("^V[0-9A-Z]*$", p)) {
    "L"
  } else if (grepl("^K[0-9A-Z]*$", p)) {
    "1/h"
  } else {
    ""
  }
}

# Nombres habituales (en mayúsculas) de las columnas del paciente, el
# tiempo y la concentración.
population_column_names <- list(
  group = c("ID", "SUBJECT", "SUBJ", "USUBJID", "PACIENTE", "PATIENT",
            "SUJETO"),
  time = c("TIME", "TIEMPO", "T", "HOURS", "HORAS", "TAD"),
  response = c("DV", "CONC", "CP", "CONCENTRACION", "CONCENTRACIÓN",
               "CONCENTRATION", "Y")
)

# Busca en el CSV, por su nombre, las columnas del paciente, el tiempo, la
# concentración y la dosis. El tiempo, la concentración y la dosis deben
# ser numéricos.
# Recibe: datos.
# Devuelve: lista con group, time, response y dose (NULL si no se
# encuentra).
population_guess_columns <- function(dat) {
  cols <- names(dat)
  nums <- cols[vapply(dat, is.numeric, logical(1))]
  first <- function(x) if (length(x)) x[[1]] else NULL
  name_in <- function(choices, role) {
    first(choices[toupper(choices) %in% population_column_names[[role]]])
  }
  list(
    group = name_in(cols, "group"),
    time = name_in(nums, "time"),
    response = name_in(nums, "response"),
    dose = first(nums[population_is_dose_name(nums)])
  )
}

# Indica qué columnas necesarias no se han encontrado en el CSV.
# Recibe: resultado de population_guess_columns() y si el modelo necesita
# la dosis.
# Devuelve: vector con lo que falta.
population_missing_columns <- function(guess, needs_dose) {
  c(
    if (is.null(guess$group)) {
      "Una columna con el paciente (por ejemplo, ID o Subject)."
    },
    if (is.null(guess$time)) {
      "Una columna numérica con el tiempo (por ejemplo, TIME o Tiempo)."
    },
    if (is.null(guess$response)) {
      paste(
        "Una columna numérica con la concentración (por ejemplo, DV, CONC",
        "o CP)."
      )
    },
    if (needs_dose && is.null(guess$dose)) {
      paste(
        "Una columna numérica con la dosis de cada paciente (por ejemplo,",
        "DOSE o Dosis)."
      )
    }
  )
}

# Nombres habituales de las covariables (en mayúsculas) y lo que indican.
population_known_covariates <- c(
  WT = "Peso", WEIGHT = "Peso", PESO = "Peso", BW = "Peso", WGT = "Peso",
  SEX = "Sexo", SEXO = "Sexo", GENDER = "Sexo",
  AGE = "Edad", EDAD = "Edad",
  HT = "Altura", HEIGHT = "Altura", ALTURA = "Altura", TALLA = "Altura",
  BMI = "Índice de masa corporal", IMC = "Índice de masa corporal",
  RACE = "Raza", RAZA = "Raza", ETHNIC = "Raza",
  SMOK = "Fumador", SMOKER = "Fumador", FUMADOR = "Fumador"
)

# Covariables conocidas que son categorías aunque vengan codificadas con
# números (por ejemplo, sexo 0/1 o raza 1/2/3).
population_categorical_labels <- c("Sexo", "Raza", "Fumador")

# Columnas de dosificación o de medida (en mayúsculas), que no son
# características del paciente.
population_event_columns <- c(
  "AMT", "EVID", "MDV", "CMT", "RATE", "ADDL", "II", "SS", "TAD", "DV",
  "CP", "CONC", "BLQ", "LLOQ", "OCC"
)

# Indica qué nombres de columna parecen la dosis (DOSE, Dosis, Dose_mg...).
# Recibe: nombres de columna.
# Devuelve: vector lógico.
population_is_dose_name <- function(x) {
  grepl("^(DOSE|DOSIS)", toupper(x))
}

# Revisa las columnas del CSV que no son el paciente, el tiempo, la
# concentración ni la dosis, y decide cuáles pueden usarse como covariables:
# deben ser constantes en cada paciente, sin valores ausentes, variar entre
# pacientes y, si son categorías, tener como mucho 6. Las numéricas con más
# de dos valores son continuas; el resto, categóricas (como en
# population_covariate_design()).
# Recibe: datos, columna del paciente y columnas ya usadas.
# Devuelve: data.frame con una fila por columna revisada (columna, qué
# indica, tipo, valores, si se puede usar y motivo si no).
population_detect_covariates <- function(dat, group, reserved = character()) {
  cols <- setdiff(names(dat), c(group, reserved))
  cols <- cols[!toupper(cols) %in% population_event_columns]
  cols <- cols[!vapply(dat[cols], population_is_row_index, logical(1))]
  unit <- dat[[group]]
  rows <- lapply(cols, function(col) {
    x <- dat[[col]]
    label <- unname(population_known_covariates[toupper(col)])
    if (is.na(label)) label <- "—"
    out <- function(type, values, usable, reason = "") {
      data.frame(
        Columna = col, Indica = label, Tipo = type, Valores = values,
        Usable = usable, Motivo = reason, stringsAsFactors = FALSE
      )
    }
    if (population_is_dose_name(col)) {
      return(out("—", "—", FALSE, "Parece la dosis: indícala como dosis."))
    }
    if (is.list(x) || anyNA(x) || (is.character(x) && any(!nzchar(x)))) {
      return(out("—", "—", FALSE, "Tiene valores ausentes."))
    }
    if (!all(population_constant_by(x, unit))) {
      return(out("—", "—", FALSE, "Cambia dentro de un mismo paciente."))
    }
    # Un valor por paciente.
    first <- x[!duplicated(unit)]
    if (length(unique(first)) < 2L) {
      return(out("—", "—", FALSE, "Es igual en todos los pacientes."))
    }
    if (is.numeric(x) && length(unique(first)) > 2L &&
          !label %in% population_categorical_labels) {
      if (any(!is.finite(x))) {
        return(out("Continua", "—", FALSE, "Tiene valores infinitos."))
      }
      return(out("Continua", sprintf(
        "%s – %s (mediana %s)",
        signif(min(first), 4), signif(max(first), 4),
        signif(stats::median(first), 4)
      ), TRUE))
    }
    counts <- table(as.character(first))
    values <- paste(
      sprintf("%s (%d)", names(counts), as.integer(counts)),
      collapse = ", "
    )
    if (length(counts) > 6L) {
      return(out(
        "Categórica", sprintf("%d categorías", length(counts)), FALSE,
        "Tiene más de 6 categorías."
      ))
    }
    out("Categórica", values, TRUE)
  })
  if (!length(rows)) {
    return(data.frame(
      Columna = character(), Indica = character(), Tipo = character(),
      Valores = character(), Usable = logical(), Motivo = character()
    ))
  }
  res <- do.call(rbind, rows)
  # Primero las que se pueden usar y, entre ellas, las reconocidas.
  res[order(!res$Usable, res$Indica == "—"), , drop = FALSE]
}

# Prepara los datos para el ajuste: las covariables categóricas que vienen
# codificadas con números (sexo 0/1, raza 1/2/3...) se pasan a texto para
# que se traten como categorías.
# Recibe: datos y resultado de population_detect_covariates().
# Devuelve: datos con esas columnas como texto.
population_prepare_covariates <- function(dat, detected) {
  cat_cols <- detected$Columna[detected$Usable &
                                 detected$Tipo == "Categórica"]
  for (col in cat_cols) {
    if (is.numeric(dat[[col]])) dat[[col]] <- as.character(dat[[col]])
  }
  dat
}

# ------------------------------------------------------------
# Datos frente al modelo elegido
# ------------------------------------------------------------

# Revisa si los datos tienen lo que necesita el modelo elegido antes de
# ajustarlo: pacientes y medidas suficientes, la dosis y que la forma de
# las curvas encaje con el compartimento donde entra la dosis (si la
# concentración baja desde la primera medida no hay fase de absorción, y
# al revés).
# Recibe: estructura del modelo (population_structure()), datos, columnas
# del paciente, tiempo y concentración y compartimento de la dosis.
# Devuelve: lista con missing (datos que faltan: el modelo no se puede
# ajustar) y warnings (avisos: se puede ajustar, pero conviene revisarlo).
population_check_data <- function(st, dat, group, time, response, dose_cmt) {
  missing <- character()
  warnings <- character()
  y <- dat[[response]]
  t <- dat[[time]]
  unit <- dat[[group]]

  # Pacientes y medidas.
  n_obs <- tapply(is.finite(y), unit, sum)
  if (length(n_obs) < 5L || any(n_obs < 3L)) {
    missing <- c(missing, sprintf(
      paste(
        "Al menos 5 pacientes con 3 medidas cada uno (hay %d pacientes y",
        "el que menos tiene %d medidas)."
      ),
      length(n_obs), min(n_obs)
    ))
  }
  if (any(y < 0, na.rm = TRUE)) {
    missing <- c(missing, "Concentraciones sin valores negativos.")
  }
  if (stats::median(n_obs) <= length(st$params)) {
    warnings <- c(warnings, sprintf(
      paste(
        "El modelo tiene %d parámetros y la mitad de los pacientes tiene %d",
        "medidas o menos: los ajustes de cada paciente por separado pueden",
        "no tener solución."
      ),
      length(st$params), stats::median(n_obs)
    ))
  }

  # Dosis: solo la necesitan los modelos con compartimentos.
  if (!length(st$cmts)) {
    return(list(missing = missing, warnings = warnings))
  }

  # Forma de las curvas frente al compartimento de la dosis: se mira en
  # cada paciente si la concentración máxima está en la primera medida
  # después de t = 0.
  central <- st$central_cmt
  if (is.null(central) || !dose_cmt %in% st$cmts) {
    return(list(missing = missing, warnings = warnings))
  }
  ok <- is.finite(y) & is.finite(t) & t > 0
  peak_first <- vapply(split(seq_along(y)[ok], unit[ok]), function(i) {
    if (length(i) < 2L) {
      return(NA)
    }
    i <- i[order(t[i])]
    which.max(y[i]) == 1L
  }, logical(1))
  peak_first <- peak_first[!is.na(peak_first)]
  n_first <- sum(peak_first)
  n <- length(peak_first)
  absorption <- st$params[grepl("^KA", toupper(st$params))]
  if (n && !identical(dose_cmt, central) && n_first > n / 2) {
    warnings <- c(warnings, sprintf(
      paste(
        "En %d de %d pacientes la concentración ya baja desde la primera",
        "medida: no hay fase de absorción, como tras una dosis",
        "intravenosa. Con la dosis en %s, %s no se puede estimar con estos",
        "datos. Si la dosis fue intravenosa, elige %s como compartimento",
        "de la dosis."
      ),
      n_first, n, dose_cmt,
      if (length(absorption)) {
        paste(absorption, collapse = ", ")
      } else {
        "la absorción"
      },
      central
    ))
  } else if (n && identical(dose_cmt, central) && n - n_first > n / 2) {
    warnings <- c(warnings, sprintf(
      paste(
        "En %d de %d pacientes la concentración sube al principio, como",
        "tras una dosis oral o extravascular, pero la dosis entra",
        "directamente en %s. Si hubo absorción, elige el compartimento de",
        "absorción (%s)."
      ),
      n - n_first, n, central,
      paste(setdiff(st$cmts, central), collapse = ", ")
    ))
  }
  list(missing = missing, warnings = warnings)
}
