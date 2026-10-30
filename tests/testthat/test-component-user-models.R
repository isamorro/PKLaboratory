# ============================================================
# Archivo: test-component-user-models.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas del componente de modelos del usuario: validación, compilación
# con mrgsolve y simulación de prueba (RF-2.7 y RF-2.8), serialización
# para guardarlos (RNF-22) y limpieza de los archivos compilados.
# ============================================================

user_spec <- function(code) {
  list(name = "Prueba", description = "", code = code)
}

one_cmt_code <- paste(
  "$PARAM CL = 5, V = 50, KA = 1",
  "$CMT DEPOT CENT",
  "$ODE",
  "dxdt_DEPOT = -KA * DEPOT;",
  "dxdt_CENT = KA * DEPOT - CL / V * CENT;",
  "$TABLE",
  "capture CP = CENT / V;",
  sep = "\n"
)

# Prefijo propio de esta ejecución, como el de cada sesión de Shiny.
prefix <- paste0("test_", as.integer(Sys.time()), "_")
withr::defer(remove_compiled_files(prefix), teardown_env())

test_that("un modelo válido se compila y se simula como uno de la biblioteca", {
  mod <- compile_user_model(user_spec(one_cmt_code), paste0(prefix, "ok"))
  expect_true(attr(mod, "pk_user_model"))
  expect_equal(names(get_model_params(mod)), c("CL", "V", "KA"))
  expect_equal(get_model_cmt(mod), c("DEPOT", "CENT"))

  # Mismo modelo que pk1 escrito a mano: mismas concentraciones.
  p <- list(CL = 1, V = 20, KA = 1)
  own <- simulate_model(mod, p, 100, "DEPOT", 24, 3, 72)
  lib <- simulate_model(load_model("pk1"), p, 100, "EV", 24, 3, 72)
  expect_equal(own$CP, lib$CP, tolerance = 1e-6)
})

test_that("un error de C++ muestra los mensajes del compilador", {
  bad <- sub("CL / V", "CL / VV", one_cmt_code, fixed = TRUE)
  expect_length(validate_model_code(bad), 0)
  err <- tryCatch(
    suppressMessages(compile_user_model(user_spec(bad),
                                        paste0(prefix, "bad"))),
    error = conditionMessage
  )
  expect_match(err, compile_code_error, fixed = TRUE)
  expect_match(err, "VV")
})

test_that("un modelo que produce valores no finitos se rechaza", {
  not_finite <- paste(
    "$PARAM K = 1",
    "$CMT A",
    "$ODE",
    "dxdt_A = -K * A;",
    "$TABLE",
    "capture Y = log(A - 1000.0);",
    sep = "\n"
  )
  expect_error(
    suppressMessages(compile_user_model(user_spec(not_finite),
                                        paste0(prefix, "nan"))),
    unstable_model_error, fixed = TRUE
  )
})

test_that("un modelo que hace fallar al integrador se rechaza", {
  explosive <- paste(
    "$PARAM K = 1000",
    "$CMT A",
    "$ODE",
    "dxdt_A = K * A * A;",
    sep = "\n"
  )
  err <- tryCatch(
    suppressMessages(suppressWarnings(capture.output(
      compile_user_model(user_spec(explosive), paste0(prefix, "explosive")),
      type = "message"
    ))),
    error = conditionMessage
  )
  expect_equal(err, generic_simulation_error)
})

test_that("la especificación se guarda en JSON y se recupera igual", {
  spec <- user_spec(one_cmt_code)
  json <- to_json(serialize_model_spec(spec))
  back <- normalize_saved_spec(
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  )
  expect_equal(back$code, spec$code)
})

test_that("una especificación guardada con código no admitido se rechaza", {
  saved <- list(version = 1L, code = paste(one_cmt_code, "$ENV x <- 1"))
  expect_error(normalize_saved_spec(saved))
  expect_error(normalize_saved_spec(list(version = 2L, code = "")),
               "Versión")
})

test_that("al cerrar la sesión se borran sus archivos compilados", {
  name <- paste0(prefix, "clean")
  compile_user_model(user_spec(one_cmt_code), name)
  pattern <- paste0("^", name)
  expect_gt(length(list.files(tempdir(), pattern, recursive = TRUE,
                              include.dirs = TRUE)), 0)
  remove_compiled_files(name)
  expect_length(list.files(tempdir(), pattern, recursive = TRUE,
                           include.dirs = TRUE), 0)
})
