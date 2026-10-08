simulate_pk1 <- function(dose, cl, v, ka, end_time = 24) {

  mod <- modlib("pk1")

  mod <- param(
    mod,
    CL = cl,
    V = v,
    KA = ka
  )

  dose_event <- ev(
    amt = dose,
    cmt = 1
  )

  result <- mod |>
    ev(dose_event) |>
    mrgsim(
      end = end_time,
      delta = 0.1
    )

  as.data.frame(result)
}