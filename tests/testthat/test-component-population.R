# ============================================================
# Archivo: test-component-population.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas del componente de análisis poblacional (mrgsolve + nlme). Se
# reproduce el ejemplo de la teofilina de Pinheiro y Bates y se comparan
# los resultados de la aplicación con los de nlme ejecutado directamente
# con SSfol (RNF-7). También se prueba que el ajuste en un proceso aparte
# no bloquea la sesión (RNF-10).
# ============================================================

theoph <- function() {
  dat <- as.data.frame(datasets::Theoph)
  dat$Subject <- as.character(dat$Subject)
  dat
}

# Estructura de pk1 con KE, como la usa la aplicación (V = CL / KE).
pk1_structure <- function() {
  population_ke_structure(population_structure(load_model("pk1")))
}

# Ajuste de referencia: fm3Theo.nlme de Pinheiro y Bates (sección 6.2).
reference_fit <- function() {
  nlme::nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data = datasets::Theoph,
    fixed = lKe + lKa + lCl ~ 1,
    random = nlme::pdDiag(lKa + lCl ~ 1),
    start = c(lKe = -2.4, lKa = 0.45, lCl = -3.2)
  )
}

test_that("pk1 tiene la estructura que espera el análisis", {
  st <- pk1_structure()
  expect_true(st$ok)
  expect_setequal(st$params, c("CL", "KE", "KA"))
  expect_equal(st$ke, "V")
  expect_equal(st$observed, "CP")
})

test_that("el modelo mixto coincide con nlme + SSfol (Theoph)", {
  res <- NULL
  invisible(capture.output(
    res <- population_fit_nlme(
      theoph(), pk1_structure(), "conc", "Subject", "Time", "EV",
      random = c("CL", "KA"), dose_column = "Dose"
    )
  ))
  ref <- reference_fit()

  app_fixed <- nlme::fixef(res$fit)
  ref_fixed <- nlme::fixef(ref)
  expect_equal(unname(app_fixed[c("tcl", "tke", "tka")]),
               unname(ref_fixed[c("lCl", "lKe", "lKa")]),
               tolerance = 1e-4)
  expect_equal(as.numeric(logLik(res$fit)), as.numeric(logLik(ref)),
               tolerance = 1e-4)
  # Desviaciones típicas de los efectos aleatorios (KA y CL).
  sd_of <- function(fit) {
    vc <- nlme::VarCorr(fit)
    stats::setNames(as.numeric(vc[, "StdDev"]), rownames(vc))
  }
  expect_equal(unname(sd_of(res$fit)[c("tka", "tcl")]),
               unname(sd_of(ref)[c("lKa", "lCl")]),
               tolerance = 1e-3)
  expect_equal(res$fit$sigma, ref$sigma, tolerance = 1e-4)
})

test_that("los ajustes individuales coinciden con nlsList + SSfol", {
  fits <- NULL
  invisible(capture.output(
    fits <- population_individual_fits(
      theoph(), pk1_structure(), "conc", "Subject", "Time", "EV",
      dose_column = "Dose"
    )
  ))
  ref <- coef(nlme::nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
                            data = datasets::Theoph))
  for (pair in list(c("CL", "lCl"), c("KA", "lKa"), c("KE", "lKe"))) {
    app <- fits[fits$Parametro == pair[1], ]
    expect_equal(app$Estimacion[order(as.numeric(app$Paciente))],
                 ref[order(as.numeric(rownames(ref))), pair[2]],
                 tolerance = 1e-3, label = pair[1])
  }
})

test_that("un parámetro fijo no puede tener efecto aleatorio", {
  expect_error(
    population_fit_nlme(theoph(), pk1_structure(), "conc", "Subject",
                        "Time", "EV", random = "CL", fixed = "CL",
                        dose_column = "Dose"),
    "fijo"
  )
})

test_that("los errores de nlme se explican en español", {
  x <- population_explain_error("step halving factor reduced below minimum")
  expect_false(identical(x$cause,
                         "step halving factor reduced below minimum"))
  expect_gt(length(x$hints), 0)
})

test_that("el ajuste en otro proceso no bloquea la sesión", {
  started <- Sys.time()
  promise <- population_fit_isolated(
    theoph(), pk1_structure(), "conc", "Subject", "Time", "EV",
    random = c("CL", "KA"), dose_column = "Dose"
  )
  # La promesa se devuelve enseguida: el ajuste sigue en el proceso hijo.
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 1)

  result <- NULL
  promises::then(promise, function(value) result <<- value)
  deadline <- Sys.time() + 120
  while (is.null(result) && Sys.time() < deadline) later::run_now(0.2)

  expect_false(is.null(result))
  expect_equal(result$n_units, 12)
  expect_true(all(c("fixed", "variability", "individual") %in%
                    names(result)))
})
