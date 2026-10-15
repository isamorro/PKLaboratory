# ============================================================
# Archivo: catalog.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para consultar y cargar los modelos
# disponibles en mrgsolve, obtener sus parámetros y compartimentos
# y almacenar los modelos compilados para reutilizarlos durante
# la sesión.
# ============================================================

# Caché de modelos ya compilados durante la sesión de R.
# Compilar un modelo tarda unos segundos, así que solo se hace una vez.
.model_cache <- new.env(parent = emptyenv())

# Obtiene los identificadores de los modelos disponibles en mrgsolve.
models_ids <- list_models()

# Obtiene los identificadores de los modelos disponibles en mrgsolve.
# Devuelve: vector de nombres ordenados alfabéticamente.
list_models <- function() {
  models_dir <- system.file("models", package = "mrgsolve")
  files <- list.files(models_dir, pattern = "\\.cpp$")
  sort(sub("\\.cpp$", "", files))
}

# Carga el modelo indicado y lo almacena para reutilizarlo durante la sesión.
# Recibe: identificador del modelo.
# Devuelve: modelo de mrgsolve compilado.
load_model <- function(model_id) {
  if (!model_id %in% models_ids) {
    stop("Modelo no disponible: ", model_id, call. = FALSE)
  }

  if (is.null(.model_cache[[model_id]])) {
    .model_cache[[model_id]] <- mrgsolve::modlib(model_id)
  }

  .model_cache[[model_id]]
}

# Obtiene los parámetros numéricos del modelo.
# Recibe: modelo de mrgsolve.
# Devuelve: lista con los nombres y valores de los parámetros.
get_model_params <- function(mod) {
  params <- as.list(mrgsolve::param(mod))
  params[vapply(params, is.numeric, logical(1))]
}

# Obtiene los nombres de los compartimentos del modelo.
# Recibe: modelo de mrgsolve.
# Devuelve: vector con los nombres de los compartimentos.
get_model_cmt <- function(mod) {
  names(mrgsolve::init(mod))
}
