# ============================================================
# Archivo: test-unit-population-data.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de unidad de la lectura y preparación de los datos del análisis
# poblacional: separador del CSV (RF-3.2), límite de filas (RNF-11),
# detección de columnas y covariables (RF-3.3 y RF-3.7) y conversión de
# unidades (RF-3.4). Se usa el conjunto de datos Theoph de R.
# ============================================================

theoph <- function() {
  dat <- as.data.frame(datasets::Theoph)
  dat$Subject <- as.character(dat$Subject)
  dat
}

# Escribe un data.frame en un CSV temporal.
write_tmp <- function(dat, excel = FALSE) {
  path <- tempfile(fileext = ".csv")
  if (excel) {
    utils::write.csv2(dat, path, row.names = FALSE)
  } else {
    utils::write.csv(dat, path)
  }
  path
}

test_that("se leen CSV con coma y con punto y coma (Excel en español)", {
  a <- population_read_csv(write_tmp(theoph()))
  b <- population_read_csv(write_tmp(theoph(), excel = TRUE))
  expect_equal(nrow(a), 132)
  expect_equal(a$conc, b$conc)
})

test_that("se quita la columna con el número de fila", {
  dat <- population_read_csv(write_tmp(theoph()))
  expect_false("X" %in% names(dat))
})

test_that("el CSV debe tener entre 10 y 200 000 filas", {
  expect_error(population_read_csv(write_tmp(theoph()[1:9, ])),
               "entre 10 y 200000")
  expect_silent(population_read_csv(write_tmp(theoph()[1:10, ])))
})

test_that("se detectan las columnas de paciente, tiempo, concentración y dosis", {
  cols <- population_guess_columns(theoph())
  expect_equal(cols$group, "Subject")
  expect_equal(cols$time, "Time")
  expect_equal(cols$response, "conc")
  expect_equal(cols$dose, "Dose")
})

test_that("el peso de Theoph es una covariable continua utilizable", {
  cov <- population_detect_covariates(theoph(), "Subject",
                                      c("Time", "conc", "Dose"))
  wt <- cov[cov$Columna == "Wt", ]
  expect_equal(wt$Tipo, "Continua")
  expect_true(wt$Usable)
})

test_that("una columna que cambia dentro del paciente no es covariable", {
  dat <- theoph()
  dat$Ruido <- seq_len(nrow(dat)) %% 7
  cov <- population_detect_covariates(dat, "Subject",
                                      c("Time", "conc", "Dose"))
  expect_false(cov$Usable[cov$Columna == "Ruido"])
})

test_that("las unidades se convierten a h, mg/L y mg", {
  dat <- data.frame(t = 60, c = 1000, d = 2, w = 70)
  out <- population_convert_units(dat, "t", "c", "d",
                                  time_unit = "min", conc_unit = "µg/L",
                                  dose_unit = "mg/kg", weight = "w")
  expect_equal(out$t, 1)
  expect_equal(out$c, 1)
  expect_equal(out[[population_dose_mg]], 140)
})

test_that("con mg/kg hace falta una columna de peso válida", {
  dat <- data.frame(t = 1, c = 1, d = 2, w = 0)
  expect_error(
    population_convert_units(dat, "t", "c", "d", dose_unit = "mg/kg",
                             weight = "w"),
    "columna de peso"
  )
})

test_that("se exigen al menos 5 pacientes con 3 medidas", {
  st <- population_ke_structure(population_structure(load_model("pk1")))
  few <- theoph()[theoph()$Subject %in% c("1", "2", "3", "4"), ]
  out <- population_check_data(st, few, "Subject", "Time", "conc", "EV")
  expect_match(out$missing, "Al menos 5 pacientes", all = FALSE)
  out <- population_check_data(st, theoph(), "Subject", "Time", "conc", "EV")
  expect_length(out$missing, 0)
})
