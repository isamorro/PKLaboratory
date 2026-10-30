# ============================================================
# Archivo: test-unit-validation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de unidad de la validación de las simulaciones
# (validate_simulation() y check_param_rules()). Son pruebas de caja negra:
# se eligen clases de equivalencia (valores válidos y no válidos) y los
# valores límite de cada dato, según RF-1.11 y RNF-13.
# ============================================================

# Configuración válida de partida; cada prueba cambia un solo dato.
valid_args <- function(...) {
  args <- list(
    params = list(CL = 1, V = 20, KA = 1),
    dose = 100, dose_cmt = "EV", model_cmts = c("EV", "CENT"),
    n_doses = 1, interval = 24, end_time = 48, infusion = 0
  )
  utils::modifyList(args, list(...))
}
errors_for <- function(...) do.call(validate_simulation, valid_args(...))

test_that("una configuración válida no produce errores", {
  expect_length(errors_for(), 0)
  expect_length(errors_for(n_doses = 10, interval = 24, end_time = 240), 0)
  expect_length(errors_for(infusion = 2), 0)
})

test_that("la dosis debe ser positiva (límite en 0)", {
  expect_match(errors_for(dose = 0), "dosis debe ser un número mayor que 0")
  expect_match(errors_for(dose = -5), "dosis")
  expect_match(errors_for(dose = NA_real_), "dosis")
  expect_length(errors_for(dose = 1e-6), 0)
})

test_that("sin compartimentos se admite dosis 0", {
  expect_length(errors_for(dose = 0, model_cmts = character(),
                           dose_cmt = ""), 0)
  expect_match(errors_for(dose = -1, model_cmts = character(),
                          dose_cmt = ""), "mayor o igual que 0")
})

test_that("el tiempo final debe ser positivo", {
  expect_match(errors_for(end_time = 0), "tiempo final")
  expect_match(errors_for(end_time = Inf), "tiempo final")
})

test_that("el número de dosis es un entero entre 1 y 1000", {
  expect_match(errors_for(n_doses = 0), "entero mayor o igual que 1")
  expect_match(errors_for(n_doses = 2.5), "entero")
  expect_length(errors_for(n_doses = 1000, interval = 1, end_time = 1000), 0)
  expect_match(errors_for(n_doses = 1001, interval = 1, end_time = 2000),
               "no puede superar 1000")
})

test_that("con varias dosis, el intervalo es positivo y cabe en el tiempo", {
  expect_match(errors_for(n_doses = 3, interval = 0), "intervalo")
  # Última dosis en t = 48: justo en el límite es válida, después no.
  expect_length(errors_for(n_doses = 3, interval = 24, end_time = 48), 0)
  expect_match(errors_for(n_doses = 3, interval = 24, end_time = 47),
               "t = 48")
})

test_that("la infusión es 0 (bolo) o positiva y no supera el intervalo", {
  expect_match(errors_for(infusion = -1), "infusión")
  expect_length(errors_for(n_doses = 2, interval = 12, infusion = 12), 0)
  expect_match(errors_for(n_doses = 2, interval = 12, infusion = 13),
               "no puede durar más que el intervalo")
})

test_that("el compartimento de administración debe existir en el modelo", {
  expect_match(errors_for(dose_cmt = "GUT"), "compartimento")
})

test_that("los parámetros vacíos o no numéricos se señalan por nombre", {
  out <- errors_for(params = list(CL = NA, V = "a", KA = 1))
  expect_match(out, "CL, V", all = FALSE)
})

test_that("se devuelven todos los errores a la vez", {
  out <- errors_for(dose = 0, end_time = 0, n_doses = 0)
  expect_gte(length(out), 3)
})

test_that("las reglas de los parámetros distinguen > 0 y >= 0", {
  expect_length(check_param_rules(list(CL = 1, V = 1, Q = 0, VMAX = 0)), 0)
  expect_match(check_param_rules(list(CL = 0)), "CL .* mayor que 0")
  expect_match(check_param_rules(list(Q = -1)), "Q .* no puede ser negativo")
  # Un parámetro sin regla no se comprueba.
  expect_length(check_param_rules(list(OTRO = -3)), 0)
})

test_that("las reglas no se aplican a los modelos del usuario", {
  out <- errors_for(params = list(CL = -1), apply_param_rules = FALSE)
  expect_length(out, 0)
})
