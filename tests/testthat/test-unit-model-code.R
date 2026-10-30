# ============================================================
# Archivo: test-unit-model-code.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de unidad de la validación de los modelos del usuario: el código
# de mrgsolve (validate_model_code(), RNF-12), el nombre del modelo
# (check_model_name(), RF-2.6) y la traducción a C++ del asistente
# (math_cpp() y generate_component_code(), RF-2.3). Se comprueba que el
# código admitido pasa y que se rechazan las construcciones que permitirían
# ejecutar algo distinto de un modelo farmacocinético.
# ============================================================

valid_code <- paste(
  "$PARAM CL = 5, V = 50, KA = 1",
  "$CMT DEPOT CENT",
  "$ODE",
  "dxdt_DEPOT = -KA * DEPOT;",
  "dxdt_CENT = KA * DEPOT - CL / V * CENT;",
  "$TABLE",
  "capture CP = CENT / V;",
  sep = "\n"
)

# Sustituye una línea del código válido.
with_code <- function(pattern, replacement) {
  sub(pattern, replacement, valid_code, fixed = TRUE)
}

test_that("un modelo correcto no tiene errores", {
  expect_length(validate_model_code(valid_code), 0)
})

test_that("el código vacío o demasiado largo se rechaza", {
  expect_match(validate_model_code(""), "Escribe el código")
  expect_match(validate_model_code(NA_character_), "Escribe el código")
  long <- paste0(valid_code, "\n// ", strrep("x", max_code_length))
  expect_match(validate_model_code(long), "30000 caracteres")
})

test_that("sin bloques no hay modelo", {
  expect_match(validate_model_code("dxdt_A = -A;"), "ningún bloque")
})

test_that("se rechazan los bloques que leen archivos o ejecutan R", {
  for (block in c("$ENV x <- 1", "$INCLUDE otro.h", "$NMXML run.xml")) {
    code <- paste(valid_code, block, sep = "\n")
    expect_gt(length(validate_model_code(code)), 0, label = block)
  }
})

test_that("$PARAM solo admite constantes (no código R)", {
  code <- with_code("CL = 5", "CL = system('ls')")
  expect_gt(length(validate_model_code(code)), 0)
  code <- with_code("CL = 5", "CL = 5e-1")
  expect_length(validate_model_code(code), 0)
})

test_that("C++ no admite include, cadenas, bucles ni funciones", {
  attacks <- c(
    "#include <cstdlib>",
    "system(\"rm -rf /\");",
    "for (int i = 0; i < 10; i++) {}",
    "while (true) {}",
    "double *p = 0;",
    "std::system(\"ls\");"
  )
  for (line in attacks) {
    code <- with_code("$TABLE", paste("$TABLE", line, sep = "\n"))
    expect_gt(length(validate_model_code(code)), 0, label = line)
  }
})

test_that("C++ admite funciones matemáticas y de mrgsolve", {
  code <- with_code(
    "capture CP = CENT / V;",
    "capture CP = exp(log(CENT / V)) + pow(KA, 2.0) * 0.0;"
  )
  expect_length(validate_model_code(code), 0)
})

test_that("solo se admiten los plugins que no dan acceso a R", {
  ok <- paste("$PLUGIN autodec", valid_code, sep = "\n")
  expect_length(validate_model_code(ok), 0)
  bad <- paste("$PLUGIN mrgx", valid_code, sep = "\n")
  expect_gt(length(validate_model_code(bad)), 0)
})

test_that("el nombre del modelo es obligatorio, válido y único", {
  expect_match(check_model_name("", "", character()), "nombre")
  expect_match(check_model_name("1modelo", "", character()), "empezar por")
  expect_match(check_model_name("Modelo ñ", "", character()), "sin tilde")
  expect_match(check_model_name(strrep("a", 31), "", character()),
               "30 caracteres")
  expect_length(check_model_name(strrep("a", 30), "", character()), 0)
  # Se compara sin distinguir mayúsculas, como un identificador.
  expect_match(check_model_name("PK1", "", "pk1"), "Ya existe")
  expect_match(check_model_name("Ok", strrep("d", 201), character()),
               "200 caracteres")
})

test_that("math_cpp traduce expresiones y escribe decimales", {
  expect_equal(math_cpp("KA * DEPOT", c("KA", "DEPOT")), "(KA * DEPOT)")
  expect_match(math_cpp("CL / 2", "CL"), "2.0")
  expect_match(math_cpp("A^2", "A"), "pow")
})

test_that("math_cpp no evalúa código ni admite símbolos desconocidos", {
  expect_error(math_cpp("system('ls')", character()), "no admitido|desconocida")
  expect_error(math_cpp("X + 1", "A"), "Variable desconocida: X")
  expect_error(math_cpp("A <- 1", "A"), "no admitido")
  expect_error(math_cpp("1 +", "A"), "no válida")
})

test_that("el asistente genera un código que pasa la validación", {
  spec <- list(
    params = list(KA = 1, CL = 5, V = 50),
    initials = list(DEPOT = 0, CENT = 0),
    auxiliary = list(KE = "CL / V"),
    equations = list(DEPOT = "-KA * DEPOT", CENT = "KA * DEPOT - KE * CENT"),
    outputs = c("CENT", "KE")
  )
  code <- generate_component_code(spec)
  expect_match(code, "dxdt_CENT")
  expect_length(validate_model_code(code), 0)
})

test_that("el asistente exige una derivada por compartimento", {
  spec <- list(
    params = list(K = 1), initials = list(A = 0, B = 0),
    auxiliary = list(), equations = list(A = "-K * A"), outputs = "A"
  )
  expect_match(validate_components(spec), "una derivada")
})
