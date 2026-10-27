# ============================================================
# Archivo: login.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Limita los intentos fallidos de inicio de sesión para dificultar los
# ataques de fuerza bruta. Los fallos se cuentan por usuario (no por sesión,
# para que abrir otra pestaña no reinicie el contador) y se guardan en memoria
# del proceso de R: se pierden al reiniciar la aplicación.
# También gestiona los tokens de sesión, que permiten recargar la página sin
# volver a escribir la contraseña. El navegador guarda el token solo en la
# pestaña (sessionStorage) y la base de datos solo guarda su hash, con una
# caducidad fija; al cerrar la sesión se borra.
# ============================================================

# Fallos permitidos dentro de la ventana antes de bloquear el usuario.
login_max_failures <- 5

# Ventana en la que se cuentan los fallos y duración del bloqueo (segundos).
login_window_secs <- 15 * 60
login_lock_secs <- 5 * 60

# Número máximo de usuarios distintos que se recuerdan (evita que alguien
# llene la memoria probando nombres al azar).
login_max_tracked <- 1000

# Instantes de los fallos recientes: usuario -> vector de tiempos.
.login_failures <- new.env(parent = emptyenv())

# Normaliza el usuario para que "Ana" y "ana " cuenten como el mismo.
# Recibe: usuario.
# Devuelve: usuario en minúsculas y sin espacios sobrantes.
login_key <- function(username) tolower(trimws(username))

# Obtiene los fallos recientes (dentro de la ventana) de un usuario.
# Recibe: usuario e instante actual.
# Devuelve: vector con los instantes de esos fallos.
recent_login_failures <- function(username, now) {
  times <- .login_failures[[login_key(username)]]
  if (is.null(times)) {
    return(numeric(0))
  }
  times[as.numeric(now) - times <= login_window_secs]
}

# Segundos que debe esperar un usuario antes de poder intentarlo de nuevo.
# Recibe: usuario e instante actual.
# Devuelve: 0 si puede intentarlo; si no, los segundos de espera.
login_wait_seconds <- function(username, now = Sys.time()) {
  failures <- recent_login_failures(username, now)
  if (length(failures) < login_max_failures) {
    return(0)
  }
  max(0, ceiling(max(failures) + login_lock_secs - as.numeric(now)))
}

# Anota un intento fallido.
# Recibe: usuario e instante actual.
register_login_failure <- function(username, now = Sys.time()) {
  keys <- ls(.login_failures, all.names = TRUE)
  if (length(keys) >= login_max_tracked) {
    for (key in keys) {
      if (length(recent_login_failures(key, now)) == 0L) {
        rm(list = key, envir = .login_failures)
      }
    }
  }
  if (length(ls(.login_failures, all.names = TRUE)) >= login_max_tracked) {
    return(invisible(NULL))
  }

  key <- login_key(username)
  .login_failures[[key]] <- c(recent_login_failures(username, now),
                              as.numeric(now))
  invisible(NULL)
}

# Borra los fallos de un usuario tras un inicio de sesión correcto.
# Recibe: usuario.
clear_login_failures <- function(username) {
  key <- login_key(username)
  if (exists(key, envir = .login_failures, inherits = FALSE)) {
    rm(list = key, envir = .login_failures)
  }
  invisible(NULL)
}

# Texto de espera para mostrar al usuario ("2 minutos", "30 segundos").
# Recibe: segundos de espera.
# Devuelve: texto con la espera en minutos o segundos.
describe_wait <- function(seconds) {
  if (seconds >= 60) {
    minutes <- ceiling(seconds / 60)
    paste(minutes, if (minutes == 1) "minuto" else "minutos")
  } else {
    paste(seconds, if (seconds == 1) "segundo" else "segundos")
  }
}

# Horas que dura un token de sesión desde que se inicia sesión.
session_token_hours <- 12

# Calcula el hash con el que se guarda un token en la base de datos.
# Recibe: token en texto.
# Devuelve: hash SHA-256 en hexadecimal.
hash_session_token <- function(token) {
  sodium::bin2hex(sodium::sha256(charToRaw(token)))
}

# Crea un token de sesión aleatorio para un usuario y guarda su hash. De paso
# borra los tokens caducados.
# Recibe: conexión a la base de datos y usuario.
# Devuelve: token en texto, para enviarlo al navegador.
create_session_token <- function(db, username) {
  token <- sodium::bin2hex(sodium::random(32))
  DBI::dbExecute(db, "DELETE FROM app_sessions WHERE expires_at <= now()")
  DBI::dbExecute(
    db,
    "INSERT INTO app_sessions (token_hash, username, expires_at)
     VALUES ($1, $2, now() + make_interval(hours => $3))",
    params = list(hash_session_token(token), username,
                  as.integer(session_token_hours))
  )
  token
}

# Busca el usuario de un token de sesión que no haya caducado y cuya cuenta
# siga activa.
# Recibe: conexión a la base de datos y token en texto.
# Devuelve: lista con el usuario (como verify_user_credentials) o NULL.
find_session_user <- function(db, token) {
  if (!grepl("^[0-9a-f]{64}$", token)) return(NULL)
  row <- DBI::dbGetQuery(
    db,
    "SELECT u.username, u.full_name
     FROM app_sessions s JOIN app_users u ON u.username = s.username
     WHERE s.token_hash = $1 AND s.expires_at > now() AND u.is_active = TRUE",
    params = list(hash_session_token(token))
  )
  if (nrow(row) != 1L) return(NULL)
  list(username = row$username[[1]], full_name = row$full_name[[1]])
}

# Borra un token de sesión (al cerrar la sesión).
# Recibe: conexión a la base de datos y token en texto.
delete_session_token <- function(db, token) {
  DBI::dbExecute(
    db,
    "DELETE FROM app_sessions WHERE token_hash = $1",
    params = list(hash_session_token(token))
  )
  invisible(NULL)
}
