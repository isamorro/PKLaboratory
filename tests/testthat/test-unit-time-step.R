# ============================================================
# Archivo: test-unit-time-step.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de unidad de choose_time_step() y translate_simulation_error().
# Son pruebas de caja blanca: cada caso recorre una rama concreta del
# código (dosis única, varias dosis, infusión, mínimo de puntos y
# alineación con las dosis; cada patrón de error conocido y el genérico).
# ============================================================

test_that("con una dosis el paso divide el tiempo en 1000 intervalos", {
  expect_equal(choose_time_step(100, 0, 1), 0.1)
})

test_that("con varias dosis hay al menos 20 puntos por intervalo", {
  step <- choose_time_step(1000, 10, 5)
  expect_lte(step, 10 / 20)
})

test_that("con infusión hay al menos 10 puntos durante la infusión", {
  step <- choose_time_step(100, 0, 1, infusion = 0.5)
  expect_lte(step, 0.5 / 10)
})

test_that("nunca se superan 20 000 puntos", {
  step <- choose_time_step(1e5, 0, 1, infusion = 0.001)
  expect_gte(step, 1e5 / max_time_points)
})

test_that("cada dosis coincide con un punto de la rejilla", {
  for (case in list(c(480, 24, 10), c(100, 7, 12), c(50, 0.3, 100))) {
    step <- choose_time_step(case[1], case[2], case[3])
    ratio <- case[2] / step
    expect_equal(ratio, round(ratio), tolerance = 1e-9)
  }
})

test_that("si alinear supera el máximo de puntos, se alinea con un paso mayor", {
  # Intervalo corto y tiempo largo: el paso mínimo manda.
  step <- choose_time_step(20000, 1.5, 1000)
  expect_gte(step, 20000 / max_time_points)
  expect_equal(1.5 / step, round(1.5 / step), tolerance = 1e-9)
})

test_that("se traducen los errores conocidos de mrgsolve", {
  e <- simpleError("pred_CL has a 0 or negative value")
  expect_equal(
    suppressMessages(translate_simulation_error(e)),
    "El parámetro CL debe ser mayor que 0 en este modelo."
  )
  e <- simpleError("missing value in cmt")
  expect_equal(suppressMessages(translate_simulation_error(e)),
               no_compartments_error)
})

test_that("un error desconocido da un mensaje genérico, no el técnico", {
  e <- simpleError("segfault in lsoda at 0x7ff")
  out <- suppressMessages(translate_simulation_error(e))
  expect_equal(out, generic_simulation_error)
  expect_no_match(out, "lsoda")
})
