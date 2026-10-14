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
# Recibe: tiempo final, intervalo entre dosis y número de dosis.
# Devuelve: paso temporal a utilizar en la simulación.
choose_time_step <- function(end_time, interval, n_doses) {

  step <- end_time / 1000

  # Con varias dosis, al menos 20 puntos por intervalo entre dosis.
  if (n_doses > 1) {
    step <- min(step, interval / 20)
  }

  step <- max(step, end_time / max_time_points)

  # Alinea la rejilla con las dosis: cada dosis cae exactamente en un punto,
  # de modo que el pico no queda recortado.
  if (n_doses > 1) {
    step <- interval / ceiling(interval / step)
  }

  step
}

# Ejecuta una simulación farmacocinética sin efectos aleatorios.
# Recibe: identificador del modelo, parámetros, dosis, compartimento
# de administración y tiempo final.
# Devuelve: data.frame con los resultados de la simulación.
simulate_model <- function(model_id, params, dose, dose_cmt,
                           interval, n_doses, end_time) {

  mod <- load_model(model_id)

  tryCatch(
    {
      # Sin efectos aleatorios: la simulación es determinista.
      mod <- mrgsolve::zero_re(mod)

      if (length(params) > 0) {
        mod <- mrgsolve::param(mod, params)
      }

      dose_event <- mrgsolve::ev(
        amt = dose,
        cmt = dose_cmt,
        ii = if (n_doses > 1) interval else 0,
        addl = n_doses - 1
      )

      result <- mod |>
        mrgsolve::ev(dose_event) |>
        mrgsolve::mrgsim(
          end = end_time,
          delta = choose_time_step(end_time, interval, n_doses)
        )

      as.data.frame(result)
    },
    error = function(e) stop(translate_simulation_error(e), call. = FALSE)
  )
}