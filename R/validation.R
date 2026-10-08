validate_parameters <- function(dose, cl, v, ka, end_time) {

  shiny::validate(
    shiny::need(dose > 0, "La dosis debe ser positiva."),
    shiny::need(cl > 0, "El aclaramiento debe ser positivo."),
    shiny::need(v > 0, "El volumen debe ser positivo."),
    shiny::need(ka > 0, "La constante de absorción debe ser positiva."),
    shiny::need(end_time > 0, "El tiempo final debe ser positivo.")
  )
}