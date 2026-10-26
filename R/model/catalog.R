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

# Descripción de los modelos de la biblioteca de mrgsolve que describe su
# documentación (?modlib_pk, ?modlib_pkpd, ?modlib_tmdd y ?modlib_viral,
# https://cran.r-project.org/web/packages/mrgsolve/refman/mrgsolve.html).
# Las descripciones se limitan a lo que dice esa documentación; para los
# demás modelos de la biblioteca se indica que no tienen descripción oficial.
library_descriptions <- c(
  "effect" = "Modelo con compartimento del efecto.",
  "emax" = paste(
    "Modelo Emax sigmoide, con un modelo PK bicompartimental con absorción",
    "desde dos compartimentos extravasculares y aclaramiento lineal y no",
    "lineal."
  ),
  "irm1" = paste(
    "Modelo de respuesta indirecta: inhibición de la producción de la",
    "respuesta. Incluye un modelo PK bicompartimental con absorción desde dos",
    "compartimentos extravasculares y aclaramiento lineal y no lineal."
  ),
  "irm2" = paste(
    "Modelo de respuesta indirecta: inhibición de la eliminación de la",
    "respuesta. Incluye un modelo PK bicompartimental con absorción desde dos",
    "compartimentos extravasculares y aclaramiento lineal y no lineal."
  ),
  "irm3" = paste(
    "Modelo de respuesta indirecta: estimulación de la producción de la",
    "respuesta. Incluye un modelo PK bicompartimental con absorción desde dos",
    "compartimentos extravasculares y aclaramiento lineal y no lineal."
  ),
  "irm4" = paste(
    "Modelo de respuesta indirecta: estimulación de la eliminación de la",
    "respuesta. Incluye un modelo PK bicompartimental con absorción desde dos",
    "compartimentos extravasculares y aclaramiento lineal y no lineal."
  ),
  "pk1" = "Modelo PK monocompartimental en forma cerrada.",
  "pk1cmt" = paste(
    "Modelo PK monocompartimental con ecuaciones diferenciales, con dos",
    "compartimentos de administración extravascular y aclaramiento lineal y",
    "no lineal."
  ),
  "pk2" = "Modelo PK bicompartimental en forma cerrada.",
  "pk2cmt" = paste(
    "Modelo PK bicompartimental con ecuaciones diferenciales, con dos",
    "compartimentos de administración extravascular y aclaramiento lineal y",
    "no lineal."
  ),
  "pk3" = "Modelo PK tricompartimental en forma cerrada.",
  "pk3cmt" = paste(
    "Modelo PK tricompartimental con ecuaciones diferenciales, con dos",
    "compartimentos de administración extravascular y aclaramiento lineal y",
    "no lineal."
  ),
  "popex" = "Modelo PK poblacional sencillo.",
  "tmdd" = "Modelo de disposición mediada por la diana.",
  "viral1" = paste(
    "Modelo de dinámica viral de la hepatitis C con una sola especie de",
    "virus."
  ),
  "viral2" = paste(
    "Modelo de dinámica viral de la hepatitis C con virus salvaje y",
    "mutante."
  )
)

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

# Busca un modelo en el catálogo compartido.
# Recibe: identificador del modelo y data.frame del catálogo compartido.
# Devuelve: fila del modelo; NULL si no está en el catálogo.
shared_model_row <- function(model_id, shared_models) {
  if (is.null(shared_models) || !nrow(shared_models)) {
    return(NULL)
  }
  row <- shared_models[paste0("db_", shared_models$id) == model_id, ,
                       drop = FALSE]
  if (nrow(row)) row else NULL
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
  row <- shared_model_row(model_id, shared_models)
  if (!is.null(row)) {
    return(row$name[[1]])
  }
  model_id
}

# Obtiene la descripción de un modelo para mostrarla junto al selector.
# Recibe: identificador del modelo, lista de modelos propios y data.frame del
# catálogo compartido.
# Devuelve: descripción del modelo; "" si no tiene.
model_description <- function(model_id, user_models, shared_models = NULL) {
  if (length(model_id) != 1L || is.na(model_id)) {
    return("")
  }
  text <- NULL
  own <- user_models[[model_id]]
  row <- shared_model_row(model_id, shared_models)
  if (!is.null(own)) {
    text <- own$description
  } else if (!is.null(row)) {
    text <- row$description[[1]]
  }
  if (is.null(text) && model_id %in% list_models()) {
    text <- if (model_id %in% names(library_descriptions)) {
      library_descriptions[[model_id]]
    } else {
      "Sin descripción oficial en la documentación de mrgsolve."
    }
  }
  if (length(text) != 1L || is.na(text)) "" else text
}

# Obtiene el código mrgsolve de un modelo para que el usuario lo consulte.
# Los modelos compartidos se leen de la caché, donde quedan al cargarlos.
# Recibe: identificador del modelo, lista de modelos propios y caché de
# modelos compartidos.
# Devuelve: código del modelo; NULL si no se ha podido obtener.
model_source_code <- function(model_id, user_models, shared_cache = NULL) {
  if (length(model_id) != 1L || is.na(model_id)) {
    return(NULL)
  }
  own <- user_models[[model_id]]
  if (!is.null(own)) {
    return(own$spec$code)
  }
  if (!is.null(shared_cache) && !is.null(shared_cache[[model_id]])) {
    return(shared_cache[[model_id]]$spec$code)
  }
  if (model_id %in% list_models()) {
    file <- system.file("models", paste0(model_id, ".cpp"),
                        package = "mrgsolve")
    return(paste(readLines(file, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n"))
  }
  NULL
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
