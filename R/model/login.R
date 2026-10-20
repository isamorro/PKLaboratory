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
login_key <- function(username) tolower(trimws(username))

# Fallos recientes (dentro de la ventana) de un usuario.
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
describe_wait <- function(seconds) {
  if (seconds >= 60) {
    minutes <- ceiling(seconds / 60)
    paste(minutes, if (minutes == 1) "minuto" else "minutos")
  } else {
    paste(seconds, if (seconds == 1) "segundo" else "segundos")
  }
}
