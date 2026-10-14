# ============================================================
# Archivo: plots.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para generar los gráficos de los
# resultados de las simulaciones farmacocinéticas.
# ============================================================

# Número de columnas y altura del panel de gráficos.
plot_columns <- 2
plot_panel_height <- 250

# Representa una variable de la simulación frente al tiempo.
# Recibe: data.frame con los resultados de la simulación,
# vector con los nombres de las variables a representar y
# identificador del modelo.
# Devuelve: objeto ggplot con el gráfico.
create_pk_plot <- function(data, vars, model_id) {

  long <- do.call(
    rbind,
    lapply(vars, function(var) {
      data.frame(
        time = data$time,
        variable = factor(var, levels = vars),
        value = data[[var]]
      )
    })
  )

  ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = .data$value)) +
    ggplot2::geom_line(color = "#2C5AA0", linewidth = 0.8) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$variable),
      ncol = plot_columns,
      scales = "free_y"
    ) +
    ggplot2::labs(x = "Tiempo", y = "Valor", title =
                    paste("Modelo", model_id)) +
    ggplot2::theme_bw()
}

# Calcula la altura del panel de gráficos en función del número de variables.
# Recibe: número de variables a representar.
# Devuelve: altura del panel de gráficos.
plot_height <- function(n_vars) {
  max(400, plot_panel_height * ceiling(n_vars / plot_columns))
}