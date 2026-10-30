# ============================================================
# Archivo: test-unit-login.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de unidad de la autenticación que no necesitan base de datos:
# el bloqueo tras intentos fallidos (RF-5.3), el hash de las contraseñas
# (RNF-15) y de los tokens de sesión (RNF-16). El instante actual se pasa
# como argumento para probar la ventana de 15 minutos y el bloqueo de 5
# sin esperar.
# ============================================================

# Empieza cada prueba sin fallos anotados.
reset_failures <- function() {
  rm(list = ls(.login_failures, all.names = TRUE), envir = .login_failures)
}

test_that("con menos de cinco fallos no hay bloqueo", {
  reset_failures()
  now <- as.POSIXct("2026-10-28 10:00:00", tz = "UTC")
  for (i in 1:4) register_login_failure("ana", now)
  expect_equal(login_wait_seconds("ana", now), 0)
})

test_that("el quinto fallo bloquea cinco minutos", {
  reset_failures()
  now <- as.POSIXct("2026-10-28 10:00:00", tz = "UTC")
  for (i in 1:5) register_login_failure("ana", now)
  expect_equal(login_wait_seconds("ana", now), 300)
  expect_equal(login_wait_seconds("ana", now + 299), 1)
  expect_equal(login_wait_seconds("ana", now + 300), 0)
})

test_that("los fallos de hace más de quince minutos no cuentan", {
  reset_failures()
  start <- as.POSIXct("2026-10-28 10:00:00", tz = "UTC")
  for (i in 1:4) register_login_failure("ana", start)
  later <- start + 16 * 60
  register_login_failure("ana", later)
  expect_equal(login_wait_seconds("ana", later), 0)
})

test_that("el usuario se normaliza y el acierto borra los fallos", {
  reset_failures()
  now <- Sys.time()
  for (i in 1:5) register_login_failure(" Ana ", now)
  expect_gt(login_wait_seconds("ana", now), 0)
  clear_login_failures("ANA")
  expect_equal(login_wait_seconds("ana", now), 0)
})

test_that("el tiempo de espera se describe en minutos o segundos", {
  expect_equal(describe_wait(300), "5 minutos")
  expect_equal(describe_wait(60), "1 minuto")
  expect_equal(describe_wait(1), "1 segundo")
})

test_that("las contraseñas se guardan con un hash salado y se verifican", {
  stored <- hash_password("TFG_2026")
  expect_false(grepl("TFG_2026", stored, fixed = TRUE))
  expect_false(identical(stored, hash_password("TFG_2026")))
  expect_true(check_password("TFG_2026", stored))
  expect_false(check_password("tfg_2026", stored))
})

test_that("el hash del token es SHA-256 y determinista", {
  token <- strrep("ab", 32)
  expect_match(hash_session_token(token), "^[0-9a-f]{64}$")
  expect_equal(hash_session_token(token), hash_session_token(token))
  expect_false(identical(hash_session_token(token), token))
})

test_that("un token con formato incorrecto se rechaza sin consultar la base", {
  # db = NULL: si llegara a consultarla, fallaría.
  expect_null(find_session_user(NULL, "no-es-un-token"))
  expect_null(find_session_user(NULL, strrep("z", 64)))
  expect_null(find_session_user(NULL, "' OR 1=1 --"))
})
