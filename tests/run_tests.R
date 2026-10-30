# ============================================================
# Archivo: run_tests.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Ejecuta todas las pruebas de tests/testthat y mide la cobertura del
# código de la aplicación con covr (porcentaje de líneas que recorren las
# pruebas), por capa y por archivo.
#
# Uso, desde la carpeta del proyecto:
#   Rscript tests/run_tests.R
#   PKLAB_TEST_DB=true Rscript tests/run_tests.R   (incluye la base de datos)
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(mrgsolve)
})

source_dirs <- c("R/model", "R/view", "R/controller")
files <- c(
  unlist(lapply(source_dirs, function(d) {
    sort(list.files(d, pattern = "\\.[rR]$", full.names = TRUE))
  })),
  "server.R"
)
# keep.source: covr necesita saber de qué línea viene cada instrucción.
for (file in files) source(file, local = FALSE, keep.source = TRUE)
options(pklab.app_loaded = TRUE)

# covr instrumenta las funciones cargadas y después ejecuta las pruebas.
runner <- tempfile(fileext = ".R")
writeLines(
  'results <<- testthat::test_dir("tests/testthat", reporter = "summary",
                                  stop_on_failure = FALSE)',
  runner
)
coverage <- covr::environment_coverage(globalenv(), test_files = runner)

# Resumen de las pruebas.
summary <- as.data.frame(results)
cat("\n== Pruebas ==\n")
cat(sprintf(
  "Pruebas: %d · comprobaciones: %d · fallos: %d · errores: %d · omitidas: %d\n",
  nrow(summary), sum(summary$nb), sum(summary$failed),
  sum(summary$error), sum(summary$skipped)
))
by_file <- stats::aggregate(
  cbind(pruebas = 1, comprobaciones = nb, fallos = failed) ~ file,
  data = transform(summary, nb = nb), FUN = sum
)
print(by_file, row.names = FALSE)

# Cobertura por capa y por archivo.
lines <- covr::tally_coverage(coverage, by = "line")
lines$layer <- ifelse(grepl("^R/", lines$filename),
                      sub("^R/([^/]+)/.*$", "\\1", lines$filename), "server")
percent <- function(x) round(100 * mean(x$value > 0), 1)

cat("\n== Cobertura por capa (% de líneas ejecutadas) ==\n")
for (layer in c("model", "controller", "view", "server")) {
  cat(sprintf("%-11s %5.1f %%\n", layer,
              percent(lines[lines$layer == layer, ])))
}
cat(sprintf("%-11s %5.1f %%\n", "total", percent(lines)))

cat("\n== Cobertura por archivo ==\n")
per_file <- vapply(split(lines, lines$filename), percent, numeric(1))
print(data.frame(archivo = names(per_file), cobertura = per_file),
      row.names = FALSE)

if (sum(summary$failed) + sum(summary$error) > 0) quit(status = 1)
