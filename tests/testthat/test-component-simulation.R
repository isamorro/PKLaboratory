# ============================================================
# Archivo: test-component-simulation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas del componente de simulación (validación + mrgsolve), de caja
# negra: se comparan los resultados con la ejecución directa de mrgsolve
# (RNF-5), con la solución analítica del modelo monocompartimental, entre
# ejecuciones repetidas (RNF-6), y se mide el tiempo de respuesta con los
# modelos de la biblioteca (RNF-9).
# ============================================================

# Tiempo máximo de una simulación ya compilada (RNF-9), en segundos.
max_simulation_seconds <- 1

test_that("los resultados coinciden con mrgsolve ejecutado directamente", {
  for (id in c("pk1", "pk2", "pk2cmt", "irm1", "tmdd")) {
    mod <- load_model(id)
    params <- get_model_params(mod)
    cmt <- get_model_cmt(mod)[1]
    app <- simulate_model(mod, params, dose = 100, dose_cmt = cmt,
                          interval = 12, n_doses = 5, end_time = 120)

    direct <- mod |>
      mrgsolve::zero_re() |>
      mrgsolve::param(params) |>
      mrgsolve::ev(amt = 100, cmt = cmt, ii = 12, addl = 4) |>
      mrgsolve::mrgsim(end = 120,
                       delta = choose_time_step(120, 12, 5)) |>
      as.data.frame()

    expect_equal(app, direct, tolerance = 1e-10, label = id)
  }
})

test_that("pk1 coincide con la solución analítica (bolo IV y absorción)", {
  mod <- load_model("pk1")
  p <- list(CL = 1, V = 20, KA = 1)
  k <- p$CL / p$V

  iv <- simulate_model(mod, p, dose = 100, dose_cmt = "CENT",
                       interval = 0, n_doses = 1, end_time = 24)
  # mrgsolve registra la observación de t = 0 antes de la dosis (como
  # NONMEM), así que la comparación empieza después.
  expect_equal(iv$CP[1], 0)
  after <- iv$time > 0
  expect_equal(iv$CP[after], 100 / p$V * exp(-k * iv$time[after]),
               tolerance = 1e-6)

  oral <- simulate_model(mod, p, dose = 100, dose_cmt = "EV",
                         interval = 0, n_doses = 1, end_time = 24)
  bateman <- 100 * p$KA / (p$V * (p$KA - k)) *
    (exp(-k * oral$time) - exp(-p$KA * oral$time))
  expect_equal(oral$CP, bateman, tolerance = 1e-6)
})

test_that("una infusión coincide con su solución analítica", {
  mod <- load_model("pk1")
  p <- list(CL = 1, V = 20, KA = 1)
  k <- p$CL / p$V
  tinf <- 2
  out <- simulate_model(mod, p, dose = 100, dose_cmt = "CENT",
                        interval = 0, n_doses = 1, end_time = 24,
                        infusion = tinf)
  rate <- 100 / tinf
  t <- out$time
  expected <- ifelse(
    t <= tinf,
    rate / p$CL * (1 - exp(-k * t)),
    rate / p$CL * (1 - exp(-k * tinf)) * exp(-k * (t - tinf))
  )
  expect_equal(out$CP, expected, tolerance = 1e-6)
})

test_that("la misma simulación da exactamente los mismos resultados", {
  mod <- load_model("pk2cmt")
  run <- function() {
    simulate_model(mod, get_model_params(mod), 100, "EV", 24, 10, 480)
  }
  expect_identical(run(), run())
})

test_that("todos los modelos de la biblioteca simulan con sus valores", {
  for (id in list_models()) {
    mod <- load_model(id)
    params <- get_model_params(mod)
    cmts <- get_model_cmt(mod)
    expect_length(
      validate_simulation(params, 100, cmts[1], cmts, 10, 24, 480), 0
    )
    out <- simulate_model(mod, params, 100, cmts[1], 24, 10, 480)
    expect_gt(nrow(out), 1000, label = id)

    # viral1 y viral2 calculan log10 de la carga viral: cuando cae a ~0, el
    # integrador deja valores de -1e-10 y su logaritmo es NaN. Pasa igual
    # con mrgsolve directo; el resto de columnas deben ser finitas.
    cols <- if (id %in% c("viral1", "viral2")) {
      setdiff(names(out), grep("^log", names(out), value = TRUE))
    } else {
      names(out)
    }
    expect_true(all(is.finite(as.matrix(out[cols]))), label = id)
  }
})

test_that("cada simulación ya compilada responde en menos de 1 s", {
  times <- vapply(list_models(), function(id) {
    mod <- load_model(id)
    cmts <- get_model_cmt(mod)
    system.time(
      simulate_model(mod, get_model_params(mod), 100, cmts[1], 24, 10, 480)
    )[["elapsed"]]
  }, numeric(1))
  expect_true(all(times < max_simulation_seconds),
              label = paste(names(times)[times >= max_simulation_seconds],
                            collapse = ", "))
})

test_that("el peor caso (20 000 puntos) también responde a tiempo", {
  mod <- load_model("pk2cmt")
  elapsed <- system.time(
    out <- simulate_model(mod, get_model_params(mod), 100, "EV",
                          interval = 1, n_doses = 1000, end_time = 20000)
  )[["elapsed"]]
  expect_lte(nrow(out), max_time_points + 2)
  expect_lt(elapsed, max_simulation_seconds)
})

test_that("un error de mrgsolve llega traducido al usuario", {
  mod <- load_model("pk1")
  expect_error(
    suppressMessages(simulate_model(mod, list(CL = 1, V = 20, KA = 1), 100,
                                    "NO_EXISTE", 0, 1, 24)),
    class = "error"
  )
  err <- tryCatch(
    suppressMessages(simulate_model(mod, list(CL = 1, V = 20, KA = 1), 100,
                                    "NO_EXISTE", 0, 1, 24)),
    error = conditionMessage
  )
  expect_no_match(err, "mrgsim|Rcpp|cmt", ignore.case = FALSE)
})
