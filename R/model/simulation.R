# ============================================================
# Archivo: simulation.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para ejecutar simulaciones farmacocinéticas.
# ============================================================

# Número máximo de puntos temporales por simulación.
max_time_points <- 20000

# Calcula el paso temporal para la simulación, de forma que se generen
# un número razonable de puntos y se representen correctamente las dosis.
# Recibe: tiempo final, intervalo entre dosis, número de dosis y duración
# de la infusión (0 si es un bolo).
# Devuelve: paso temporal a utilizar en la simulación.
choose_time_step <- function(end_time, interval, n_doses, infusion = 0) {

  step <- end_time / 1000

  # Con varias dosis, al menos 20 puntos por intervalo entre dosis.
  if (n_doses > 1) {
    step <- min(step, interval / 20)
  }

  # Con infusión, al menos 10 puntos mientras dura.
  if (infusion > 0) {
    step <- min(step, infusion / 10)
  }

  min_step <- end_time / max_time_points
  step <- max(step, min_step)

  # Alinea la rejilla con las dosis: cada dosis cae exactamente en un punto,
  # de modo que el pico no queda recortado. Si así se superase el máximo de
  # puntos, se alinea con un paso algo mayor o, si el intervalo es más corto
  # que el paso, no se alinea.
  if (n_doses > 1) {
    aligned <- interval / ceiling(interval / step)
    if (aligned < min_step && interval >= step) {
      aligned <- interval / floor(interval / step)
    }
    if (aligned >= min_step) step <- aligned
  }

  step
}

# Ejecuta una simulación farmacocinética sin efectos aleatorios. Si mrgsolve
# falla, lanza un error con el mensaje ya traducido al español.
# Recibe: modelo de mrgsolve ya compilado, parámetros, dosis, compartimento
# de administración, intervalo entre dosis, número de dosis, tiempo final y
# duración de la infusión (0 si cada dosis es un bolo).
# Devuelve: data.frame con los resultados de la simulación.
simulate_model <- function(mod, params, dose, dose_cmt,
                           interval, n_doses, end_time, infusion = 0) {

  tryCatch(
    {
      # Sin efectos aleatorios: la simulación es determinista.
      mod <- mrgsolve::zero_re(mod)

      if (length(params) > 0) {
        mod <- mrgsolve::param(mod, params)
      }

      has_cmt <- length(mrgsolve::init(mod)) > 0L

      result <- if (has_cmt) {
        # Con infusión, la dosis entra a velocidad constante (cantidad por
        # unidad de tiempo) durante el tiempo indicado.
        dose_event <- mrgsolve::ev(
          amt = dose,
          cmt = dose_cmt,
          ii = if (n_doses > 1) interval else 0,
          addl = n_doses - 1,
          rate = if (infusion > 0) dose / infusion else 0
        )

        mod |>
          mrgsolve::ev(dose_event) |>
          mrgsolve::mrgsim(
            end = end_time,
            delta = choose_time_step(end_time, interval, n_doses, infusion)
          )
      } else {
        mod |>
          mrgsolve::mrgsim(
            end = end_time,
            delta = choose_time_step(end_time, interval, n_doses)
          )
      }

      as.data.frame(result)
    },
    error = function(e) stop(translate_simulation_error(e), call. = FALSE)
  )
}
