# ============================================================
# Archivo: test-integration-database.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de integración con PostgreSQL: guardar, leer, compartir, editar
# y borrar modelos (operaciones CRUD), permisos del propietario (RNF-17),
# coherencia del catálogo compartido (RNF-20), límite de modelos
# (RNF-11), tokens de sesión (RNF-16) y borrado en cascada.
#
# Usan la base de datos configurada en .Renviron, así que solo se ejecutan
# si se pide expresamente con PKLAB_TEST_DB=true. Crean dos usuarios
# temporales (pktest_...) y los borran al terminar; al borrarlos se borran
# también sus modelos y sesiones.
# ============================================================

skip_if_not(identical(Sys.getenv("PKLAB_TEST_DB"), "true"),
            "Pruebas de base de datos desactivadas (PKLAB_TEST_DB=true)")

db <- create_database_pool()
withr::defer(pool::poolClose(db), teardown_env())

suffix <- sodium::bin2hex(sodium::random(4))
owner <- paste0("pktest_a_", suffix)
other <- paste0("pktest_b_", suffix)

# Crea un usuario temporal con contraseña conocida.
add_user <- function(username, active = TRUE) {
  DBI::dbExecute(
    db,
    "INSERT INTO app_users (username, password_hash, full_name, is_active)
     VALUES ($1, $2, 'Usuario de prueba', $3)",
    params = list(username, hash_password("clave_prueba"), active)
  )
}
add_user(owner)
add_user(other)
withr::defer(
  DBI::dbExecute(db, "DELETE FROM app_users WHERE username IN ($1, $2)",
                 params = list(owner, other)),
  teardown_env()
)

code <- paste(
  "$PARAM CL = 1, V = 20, KA = 1",
  "$CMT DEPOT CENT",
  "$ODE",
  "dxdt_DEPOT = -KA * DEPOT;",
  "dxdt_CENT = KA * DEPOT - CL / V * CENT;",
  "$TABLE",
  "capture CP = CENT / V;",
  sep = "\n"
)
prefix <- paste0("dbtest_", suffix, "_")
withr::defer(remove_compiled_files(prefix), teardown_env())
mod <- compile_user_model(list(name = "Oral", code = code),
                          paste0(prefix, "mod"))
spec <- function(name) list(name = name, description = "Prueba", code = code)

test_that("las credenciales se comprueban contra la base", {
  expect_equal(verify_user_credentials(db, owner, "clave_prueba")$username,
               owner)
  expect_null(verify_user_credentials(db, owner, "otra"))
  expect_null(verify_user_credentials(db, "no_existe_xyz", "clave_prueba"))
})

test_that("un modelo guardado es privado y solo lo ve su propietario", {
  id <- save_user_model(db, spec("Privado"), mod, owner)
  mine <- list_user_models(db, owner)
  expect_true(id %in% mine$id)
  expect_false(mine$is_shared[mine$id == id])
  expect_false(id %in% list_user_models(db, other)$id)
  expect_error(get_user_model(db, id, other), "no está disponible")

  saved <- saved_model_entry(mine[mine$id == id, ])
  expect_equal(saved$spec$code, code)
})

test_that("al compartirlo lo ven los demás y cambia la huella del catálogo", {
  id <- save_user_model(db, spec("Compartido"), mod, owner)
  before <- shared_catalog_version(db)
  set_model_shared(db, id, owner, TRUE)
  expect_false(identical(shared_catalog_version(db), before))
  expect_true(id %in% list_user_models(db, other)$id)
  expect_equal(get_user_model(db, id, other)$definition, code)

  # Editarlo también cambia la huella (RF-2.17).
  shared_now <- shared_catalog_version(db)
  edited <- spec("Compartido")
  edited$code <- sub("CL = 1", "CL = 2", code)
  update_user_model(db, id, edited, mod, owner)
  expect_false(identical(shared_catalog_version(db), shared_now))

  # Retirarlo lo oculta a los demás, pero el propietario lo conserva.
  set_model_shared(db, id, owner, FALSE)
  expect_false(id %in% list_user_models(db, other)$id)
  expect_true(id %in% list_user_models(db, owner)$id)
})

test_that("solo el propietario puede compartir, editar o borrar", {
  id <- save_user_model(db, spec("Ajeno"), mod, owner)
  expect_error(set_model_shared(db, id, other, TRUE), "no está disponible")
  expect_error(update_user_model(db, id, spec("Ajeno"), mod, other),
               "ya no está disponible")
  expect_equal(delete_user_model(db, id, other), 0)
  expect_equal(delete_user_model(db, id, owner), 1)
})

test_that("no se repite el nombre en una cuenta (sin distinguir mayúsculas)", {
  save_user_model(db, spec("Repetido"), mod, owner)
  expect_error(save_user_model(db, spec("REPETIDO"), mod, owner))
  # Otro usuario sí puede usar ese nombre.
  expect_no_error(save_user_model(db, spec("Repetido"), mod, other))
})

test_that("cada cuenta guarda como máximo 10 modelos", {
  DBI::dbExecute(db, "DELETE FROM user_models WHERE owner_username = $1",
                 params = list(other))
  for (i in seq_len(max_user_models)) {
    save_user_model(db, spec(paste0("M", i)), mod, other)
  }
  expect_error(save_user_model(db, spec("Uno_mas"), mod, other),
               "máximo de 10")
})

test_that("los tokens de sesión se guardan con hash y caducan", {
  token <- create_session_token(db, owner)
  expect_match(token, "^[0-9a-f]{64}$")
  stored <- DBI::dbGetQuery(
    db, "SELECT token_hash FROM app_sessions WHERE username = $1",
    params = list(owner)
  )$token_hash
  expect_false(token %in% stored)
  expect_true(hash_session_token(token) %in% stored)
  expect_equal(find_session_user(db, token)$username, owner)

  # Caducado.
  DBI::dbExecute(
    db,
    "UPDATE app_sessions SET expires_at = now() - interval '1 minute'
     WHERE token_hash = $1",
    params = list(hash_session_token(token))
  )
  expect_null(find_session_user(db, token))

  # Cerrar sesión lo invalida.
  token <- create_session_token(db, owner)
  delete_session_token(db, token)
  expect_null(find_session_user(db, token))
})

test_that("una cuenta desactivada no puede recuperar la sesión", {
  token <- create_session_token(db, other)
  DBI::dbExecute(db, "UPDATE app_users SET is_active = FALSE
                      WHERE username = $1", params = list(other))
  withr::defer(DBI::dbExecute(db, "UPDATE app_users SET is_active = TRUE
                                   WHERE username = $1",
                              params = list(other)))
  expect_null(find_session_user(db, token))
  expect_null(verify_user_credentials(db, other, "clave_prueba"))
})

test_that("al borrar una cuenta se borran sus modelos y sesiones", {
  temp <- paste0("pktest_c_", suffix)
  add_user(temp)
  save_user_model(db, spec("Cascada"), mod, temp)
  create_session_token(db, temp)
  DBI::dbExecute(db, "DELETE FROM app_users WHERE username = $1",
                 params = list(temp))

  count <- function(table) {
    DBI::dbGetQuery(
      db,
      sprintf("SELECT COUNT(*)::int AS n FROM %s WHERE %s = $1", table,
              if (table == "user_models") "owner_username" else "username"),
      params = list(temp)
    )$n
  }
  expect_equal(count("user_models"), 0)
  expect_equal(count("app_sessions"), 0)
})
