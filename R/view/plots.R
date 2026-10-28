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

# Paleta de colores: cada variable se dibuja con uno distinto.
plot_palette <- c(
  "#187A5B", "#4F5BD5", "#F59E0B", "#EF6A4C",
  "#8B5CF6", "#0EA5C6", "#D946A8", "#65A30D"
)

# Número de columnas y altura del panel de gráficos.
plot_columns <- 2
plot_panel_height <- 250

# Anchura (px) por debajo de la cual un gráfico se dibuja en formato móvil:
# una sola columna, letra más pequeña y menos marcas en los ejes.
plot_narrow_width <- 600
plot_narrow_panel_height <- 210

# Indica si un gráfico se muestra en una pantalla estrecha (móvil).
# Recibe: sesión de Shiny e id de la salida del gráfico.
# Devuelve: TRUE si su anchura es menor que plot_narrow_width.
plot_is_narrow <- function(session, output_id) {
  width <- session$clientData[[paste0("output_", output_id, "_width")]]
  !is.null(width) && width < plot_narrow_width
}

# Representa las variables seleccionadas frente al tiempo, cada una en su
# propio panel y con su propia escala vertical.
# Recibe: data.frame con los resultados de la simulación,
# vector con los nombres de las variables a representar y
# nombre del modelo y si se dibuja en formato móvil.
# Devuelve: objeto ggplot con el gráfico.
create_pk_plot <- function(data, vars, model_name, narrow = FALSE) {

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

  colors <- stats::setNames(rep_len(plot_palette, length(vars)), vars)
  title <- paste("Modelo", model_name)
  if (narrow) title <- paste(strwrap(title, 32), collapse = "\n")

  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$time, y = .data$value,
      color = .data$variable, fill = .data$variable
    )
  ) +
    ggplot2::geom_area(alpha = 0.12, color = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_x_continuous(n.breaks = if (narrow) 4 else 6) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$variable),
      ncol = if (narrow) 1 else plot_columns,
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Tiempo",
      y = "Valor",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = if (narrow) 11 else 13) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        face = "bold", color = "#243D33", margin = ggplot2::margin(b = 10)
      ),
      strip.text = ggplot2::element_text(
        face = "bold", hjust = 0, color = "#243D33"
      ),
      axis.title = ggplot2::element_text(color = "#64746D"),
      axis.text = ggplot2::element_text(color = "#64746D"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E3ECE7"),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      plot.background = ggplot2::element_rect(fill = NA, color = NA)
    )
}

# Calcula la altura del panel de gráficos en función del número de variables.
# Recibe: número de variables a representar y si se dibuja en formato móvil.
# Devuelve: altura del panel de gráficos.
plot_height <- function(n_vars, narrow = FALSE) {
  if (narrow) return(max(280, 60 + plot_narrow_panel_height * n_vars))
  max(400, plot_panel_height * ceiling(n_vars / plot_columns))
}
