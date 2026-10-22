# ============================================================
# Archivo: database.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la configuración de conexión a la base de datos del
# catálogo de modelos. La aplicación usa PostgreSQL en producción
# para guardar los modelos de cada usuario en su cuenta (y los que
# decida compartir) y volver a cargarlos en cualquier sesión.
# ============================================================

# Crea el pool de conexiones a PostgreSQL para el catálogo compartido.
# Devuelve: pool de conexiones de dbPool, listo para consultar y guardar.
create_database_pool <- function() {
  required <- c(
    "PGHOST", "PGDATABASE", "PGUSER", "PGPASSWORD"
  )

  missing <- required[!nzchar(Sys.getenv(required))]

  if (length(missing) > 0) {
    stop(
      "Faltan variables de conexión: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  pool::dbPool(
    drv = RPostgres::Postgres(),
    sslmode = Sys.getenv("PGSSLMODE", "require"),
    connect_timeout = 15,
    minSize = 0,
    maxSize = 3,
    idleTimeout = 60
  )
}

# Inicializa la tabla de usuarios para la autenticación en PostgreSQL.
# No modifica una tabla que ya exista.
# Recibe: conexión a la base de datos de usuarios.
# Devuelve: invisible(NULL).
ensure_auth_users_table <- function(db) {
  if (is.null(db)) {
    stop("La base de datos de autenticación no está disponible.", call. = FALSE)
  }

  sql <- "CREATE TABLE IF NOT EXISTS app_users (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    full_name TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  )"
  DBI::dbExecute(db, sql)
  invisible(NULL)
}

# Prepara la tabla user_models para que cada modelo pertenezca a una cuenta.
# Es idempotente: se puede ejecutar en cada arranque.
#  - owner_username: usuario propietario (NULL en los modelos anteriores).
#  - is_shared: si el modelo es visible para el resto de usuarios. Las filas
#    que ya existían eran públicas, así que quedan como compartidas; las
#    nuevas son privadas por defecto.
# Recibe: conexión a la base de datos PostgreSQL del catálogo.
# Devuelve: invisible(NULL).
ensure_user_models_schema <- function(db) {
  if (is.null(db)) {
    stop("El catálogo no está configurado.", call. = FALSE)
  }

  DBI::dbExecute(
    db,
    "ALTER TABLE user_models
       ADD COLUMN IF NOT EXISTS owner_username TEXT"
  )
  DBI::dbExecute(
    db,
    "ALTER TABLE user_models
       ADD COLUMN IF NOT EXISTS is_shared BOOLEAN NOT NULL DEFAULT TRUE"
  )
  DBI::dbExecute(
    db,
    "ALTER TABLE user_models ALTER COLUMN is_shared SET DEFAULT FALSE"
  )
  # Un usuario no puede tener dos modelos con el mismo nombre.
  DBI::dbExecute(
    db,
    "CREATE UNIQUE INDEX IF NOT EXISTS user_models_owner_name_uidx
       ON user_models (owner_username, lower(name))
       WHERE owner_username IS NOT NULL"
  )
  invisible(NULL)
}

# Calcula el hash seguro de una contraseña (scrypt con sal aleatoria, de
# sodium). Cada llamada produce un resultado distinto para la misma contraseña.
# Recibe: contraseña en texto plano.
# Devuelve: cadena con el hash, lista para guardar en app_users.
hash_password <- function(password) {
  sodium::password_store(password)
}

# Comprueba una contraseña frente al hash guardado en el formato moderno de
# sodium (por ejemplo: $7$C6.../....lJNI9u1GC4FPtFSTM...).
# Recibe: contraseña en texto plano y hash guardado.
# Devuelve: TRUE si coincide.
check_password <- function(password, stored) {
  isTRUE(tryCatch(
    sodium::password_verify(stored, password),
    error = function(e) FALSE
  ))
}

# Verifica usuario y contraseña frente a la BD usando el formato moderno de
# sodium. Un error de base de datos se propaga (no equivale a "contraseña
# incorrecta").
# Recibe: pool de autenticación, usuario y contraseña.
# Devuelve: lista con el usuario si la combinación es válida, o NULL.
verify_user_credentials <- function(db, username, password) {
  if (is.null(db)) {
    stop("La autenticación no está configurada.", call. = FALSE)
  }

  row <- DBI::dbGetQuery(
    db,
    "SELECT username, full_name, password_hash
     FROM app_users
     WHERE username = $1 AND is_active = TRUE",
    params = list(username)
  )

  if (nrow(row) != 1L) {
    # Se hace el mismo trabajo que con un usuario real para no revelar,
    # por el tiempo de respuesta, qué usuarios existen.
    check_password(password, dummy_password_hash)
    return(NULL)
  }

  stored <- row$password_hash[[1]]
  if (!check_password(password, stored)) {
    return(NULL)
  }

  list(username = row$username[[1]], full_name = row$full_name[[1]])
}

# Hash de relleno para comparar cuando el usuario no existe.
dummy_password_hash <- hash_password("contraseña-de-relleno")
