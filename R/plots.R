create_pk_plot <- function(data) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = time, y = CP)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Tiempo",
      y = "Concentración plasmática",
      title = "Curva concentración-tiempo"
    ) +
    ggplot2::theme_minimal()
}