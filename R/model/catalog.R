# ============================================================
# Archivo: catalog.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para consultar y cargar los modelos
# disponibles: los de la biblioteca de mrgsolve, los guardados en la cuenta
# del usuario y los compartidos por otros. También obtiene sus parámetros y
# compartimentos y almacena los modelos de la biblioteca ya
# compilados para reutilizarlos.
# ============================================================

# Caché de modelos de la biblioteca ya compilados durante la sesión de R.
# Compilar un modelo tarda unos segundos, así que solo se hace una vez.
# Los modelos del usuario no se guardan aquí: se compilan por sesión y se
# almacenan en la lista de modelos del usuario o en la caché de la sesión.
.model_cache <- new.env(parent = emptyenv())

# Modelo seleccionado al abrir la aplicación.
default_model_id <- "pk1"

# Número máximo de modelos propios por cuenta.
max_user_models <- 10

# Obtiene los identificadores de los modelos disponibles en mrgsolve.
# Devuelve: vector de nombres ordenados alfabéticamente.
list_models <- function() {
  models_dir <- system.file("models", package = "mrgsolve")
  files <- list.files(models_dir, pattern = "\\.cpp$")
  sort(sub("\\.cpp$", "", files))
}

# Carga el modelo de la biblioteca indicado y lo almacena para reutilizarlo
# durante la sesión.
# Recibe: identificador del modelo.
# Devuelve: modelo de mrgsolve compilado.
load_model <- function(model_id) {
  if (!model_id %in% list_models()) {
    stop("Modelo no disponible: ", model_id, call. = FALSE)
  }

  if (is.null(.model_cache[[model_id]])) {
    .model_cache[[model_id]] <- mrgsolve::modlib(model_id)
  }

  .model_cache[[model_id]]
}

# Obtiene un modelo de la biblioteca, propio o compartido. Los modelos
# guardados en la base de datos se compilan la primera vez que se usan en la
# sesión y se reutilizan después.
# Recibe: identificador del modelo, lista de modelos propios, conexión a la
# base, caché de modelos compilados de la sesión, prefijo de compilación y
# nombre del usuario (para comprobar que puede ver el modelo).
# Devuelve: modelo de mrgsolve compilado.
resolve_model <- function(model_id, user_models, db = NULL,
                          shared_cache = NULL, compile_prefix = "shared_",
                          username = NULL) {
  own <- user_models[[model_id]]

  # Modelo propio ya compilado (recién creado en esta sesión).
  if (!is.null(own) && !is.null(own$mod)) {
    return(own$mod)
  }

  if (grepl("^db_[0-9]+$", model_id)) {
    if (is.null(shared_cache)) stop("Caché compartida no inicializada.")
    if (is.null(shared_cache[[model_id]])) {
      if (!is.null(own)) {
        spec <- own$spec
      } else {
        row <- get_user_model(db, sub("^db_", "", model_id), username)
        spec <- normalize_saved_spec(
          jsonlite::fromJSON(row$specification[[1]], simplifyVector = FALSE)
        )
        spec$name <- row$name[[1]]
        spec$description <- row$description[[1]]
      }
      mod <- compile_user_model(spec, paste0(compile_prefix, model_id))
      shared_cache[[model_id]] <- new_user_model(model_id, spec, mod)
    }
    return(shared_cache[[model_id]]$mod)
  }

  load_model(model_id)
}

# Construye las opciones del selector de modelos.
# Recibe: lista de modelos propios y data.frame del catálogo compartido.
# Devuelve: lista con la biblioteca de mrgsolve y, si los hay, los grupos
# "Mis modelos" y "Catálogo compartido" (sin repetir los que son del usuario).
model_choices <- function(user_models, shared_models = NULL) {
  choices <- list("Biblioteca mrgsolve" = list_models())
  if (length(user_models)) {
    choices[["Mis modelos"]] <- stats::setNames(
      names(user_models),
      vapply(user_models, function(m) {
        if (isTRUE(m$saved)) m$label else paste0(m$label, " (solo sesión)")
      }, character(1))
    )
  }
  if (!is.null(shared_models) && nrow(shared_models)) {
    shared_ids <- paste0("db_", shared_models$id)
    others <- !shared_ids %in% names(user_models)
    if (any(others)) {
      choices[["Catálogo compartido"]] <- stats::setNames(
        shared_ids[others],
        shared_models$name[others]
      )
    }
  }
  choices
}

# Obtiene el nombre con el que mostrar un modelo (en el gráfico y en los
# archivos descargados), en lugar de su identificador interno.
# Recibe: identificador del modelo, lista de modelos propios y data.frame del
# catálogo compartido.
# Devuelve: nombre del modelo; el propio identificador si es de la biblioteca.
model_label <- function(model_id, user_models, shared_models = NULL) {
  if (length(model_id) != 1L || is.na(model_id)) {
    return("")
  }
  own <- user_models[[model_id]]
  if (!is.null(own)) {
    return(own$label)
  }
  if (!is.null(shared_models) && nrow(shared_models)) {
    row <- shared_models[paste0("db_", shared_models$id) == model_id, ,
                         drop = FALSE]
    if (nrow(row)) {
      return(row$name[[1]])
    }
  }
  model_id
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
