# ============================================================
# Archivo: user_models.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la capa de persistencia de los modelos de usuario. Cada modelo
# pertenece a una cuenta (owner_username) y puede marcarse como compartido
# (is_shared) para que lo vean los demás. Se guardan como especificación y se
# re-compilan en la sesión actual cuando se seleccionan, en lugar de almacenar
# objetos de mrgsolve en la base de datos.
# ============================================================

# Comprueba que la base de datos del catálogo está disponible.
# Recibe: conexión activa a la base de datos.
# Devuelve: error si no existe la conexión, o nada si es válida.
require_database <- function(db) {
  if (is.null(db)) stop("El catálogo no está configurado.",
                        call. = FALSE)
}

# Catálogo compartido vacío (mismas columnas que el que devuelve la base).
# Devuelve: data.frame sin filas.
empty_shared_models <- function() {
  data.frame(
    id = character(),
    name = character(),
    description = character(),
    date_of_joining = character(),
    stringsAsFactors = FALSE
  )
}

# Obtiene los modelos que puede ver un usuario: los suyos (privados o no) y
# los que otros usuarios han compartido.
# Recibe: conexión a la base de datos y nombre del usuario.
# Devuelve: data.frame con metadatos, propietario, si está compartido y la
# especificación en JSON.
list_user_models <- function(db, username) {
  require_database(db)
  DBI::dbGetQuery(db, "
    SELECT id::text AS id, name, description, date_of_joining,
           owner_username, is_shared, specification::text AS specification
    FROM user_models
    WHERE validation_status = 'validado' AND specification IS NOT NULL
      AND (is_shared OR owner_username = $1)
    ORDER BY name, id", params = list(as.character(username)))
}

# Recupera un modelo concreto, solo si es del usuario o está compartido.
# Recibe: conexión activa, identificador del modelo y nombre del usuario.
# Devuelve: fila con la especificación y metadatos del modelo solicitado.
get_user_model <- function(db, model_id, username) {
  require_database(db)
  result <- DBI::dbGetQuery(
    db,
    "
    SELECT id::text AS id, name, description, definition,
           specification::text AS specification
    FROM user_models
    WHERE id = $1 AND validation_status = 'validado'
      AND specification IS NOT NULL
      AND (is_shared OR owner_username = $2)",
    params = list(as.character(model_id), as.character(username))
  )
  if (nrow(result) != 1L) stop("El modelo no está disponible.", call. = FALSE)
  result
}

# Prepara la especificación del modelo para guardarla en la base de datos.
# Recibe: especificación del modelo.
# Devuelve: lista serializable con la versión y el contenido del modelo.
serialize_model_spec <- function(spec) {
  list(
    version = 2L,
    params = spec$params,
    initials = spec$initials,
    auxiliary = spec$auxiliary,
    equations = spec$equations,
    outputs = spec$outputs
  )
}

# Normaliza la especificación guardada y la valida para evitar cargas
# corruptas.
# Recibe: especificación guardada en la base de datos.
# Devuelve: especificación válida lista para recompilar con mrgsolve.
normalize_saved_spec <- function(spec) {
  if (!is.list(spec) || !identical(spec$version, 2L)) {
    stop("Versión de especificación no admitida.", call. = FALSE)
  }
  spec$version <- NULL
  problems <- validate_model_spec(spec)
  if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)
  spec
}

# Convierte una fila de la base de datos en la entrada de un modelo propio.
# El modelo aún no está compilado (mod = NULL): se compila al seleccionarlo.
# Recibe: fila de list_user_models().
# Devuelve: lista con los datos del modelo (ver new_user_model()).
saved_model_entry <- function(row) {
  spec <- normalize_saved_spec(
    jsonlite::fromJSON(row$specification[[1]], simplifyVector = FALSE)
  )
  spec$name <- row$name[[1]]
  spec$description <- if (is.na(row$description[[1]])) {
    ""
  } else {
    row$description[[1]]
  }

  new_user_model(
    paste0("db_", row$id[[1]]), spec, mod = NULL,
    saved = TRUE, is_shared = isTRUE(row$is_shared[[1]])
  )
}

# Construye la lista de modelos propios a partir de las filas de la base.
# Un modelo corrupto se ignora (queda registrado en consola) para que no
# impida cargar los demás.
# Recibe: data.frame con las filas de los modelos del usuario.
# Devuelve: lista con nombre (identificador) de entradas de modelo.
own_model_entries <- function(rows) {
  entries <- list()
  for (i in seq_len(nrow(rows))) {
    entry <- tryCatch(
      saved_model_entry(rows[i, , drop = FALSE]),
      error = function(e) {
        message("[modelos] Modelo #", rows$id[[i]], " ignorado: ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(entry)) entries[[entry$id]] <- entry
  }
  entries
}

# Actualiza los modelos propios con lo que hay en la base de datos sin perder
# el trabajo ya hecho: se conservan los modelos ya compilados y los que no se
# pudieron guardar (tmp_).
# Recibe: lista actual de modelos propios y filas del usuario en la base.
# Devuelve: nueva lista de modelos propios.
merge_own_models <- function(existing, rows) {
  fresh <- own_model_entries(rows)
  for (id in names(fresh)) {
    mod <- existing[[id]]$mod
    if (!is.null(mod)) fresh[[id]]$mod <- mod
  }
  c(fresh, existing[grepl("^tmp_", names(existing))])
}

# Elimina un modelo de la base de datos. Solo lo puede borrar su propietario.
# Recibe: conexión activa, identificador del modelo y nombre del usuario.
# Devuelve: número de filas borradas.
delete_user_model <- function(db, model_id, username) {
  require_database(db)
  DBI::dbExecute(
    db,
    "DELETE FROM user_models WHERE id = $1 AND owner_username = $2",
    params = list(as.integer(model_id), as.character(username))
  )
}

# Comparte un modelo con el resto de usuarios o deja de compartirlo. Solo lo
# puede cambiar su propietario.
# Recibe: conexión activa, identificador del modelo, nombre del usuario y
# TRUE para compartir o FALSE para dejar de compartir.
# Devuelve: invisible(NULL); lanza un error si el modelo no es del usuario.
set_model_shared <- function(db, model_id, username, shared) {
  require_database(db)
  changed <- DBI::dbExecute(
    db,
    "UPDATE user_models SET is_shared = $1
     WHERE id = $2 AND owner_username = $3",
    params = list(isTRUE(shared), as.integer(model_id), as.character(username))
  )
  if (changed != 1L) stop("El modelo no está disponible.", call. = FALSE)
  invisible(NULL)
}

# Guarda un modelo previamente validado y compilado en la cuenta del usuario.
# Por defecto es privado; se comparte después con set_model_shared().
# Recibe: conexión, especificación del modelo, objeto mrgsolve y usuario.
# Devuelve: identificador del modelo recién guardado.
save_user_model <- function(db, spec, mod, username) {

  require_database(db)
  if (!is.character(username) || length(username) != 1L || !nzchar(username)) {
    stop("No hay ningún usuario con la sesión iniciada.", call. = FALSE)
  }
  problems <- validate_model_spec(spec, existing_ids = character(0))

  if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)

  owned <- DBI::dbGetQuery(
    db,
    "SELECT COUNT(*)::int AS n FROM user_models WHERE owner_username = $1",
    params = list(username)
  )$n[[1]]
  if (owned >= max_user_models) {
    stop(sprintf("Has alcanzado el máximo de %d modelos. Elimina alguno.",
                 max_user_models), call. = FALSE)
  }

  saved <- serialize_model_spec(spec)
  normalize_saved_spec(saved)

  json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))
  }

  DBI::dbGetQuery(
    db,
    "
      INSERT INTO user_models
        (name, description, definition, parameters, compartments,
        output_variables, specification, validation_status,
        owner_username, is_shared)
      VALUES ($1, $2, $3, $4::jsonb, $5::jsonb, $6::jsonb, $7::jsonb,
      'validado', $8, FALSE)
      RETURNING id::text AS id",
    params = list(
      trimws(spec$name),
      spec$description,
      generate_model_code(spec),
      json(spec$params),
      json(as.list(get_model_cmt(mod))),
      json(as.list(spec$outputs)),
      json(saved),
      username
    )
  )$id[[1]]
}

# Actualiza el mismo registro y mantiene propietario y estado compartido.
# Recibe: conexión, identificador del modelo, especificación del modelo,
# objeto mrgsolve y usuario.
update_user_model <- function(db, model_id, spec, mod, username) {
  require_database(db)
  problems <- validate_model_spec(spec, existing_ids = character(0))
  if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)

  saved <- serialize_model_spec(spec)
  normalize_saved_spec(saved)

  json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))
  }

  changed <- DBI::dbExecute(
    db,
    "
    UPDATE user_models SET name = $1, description = $2, definition = $3,
      parameters = $4::jsonb, compartments = $5::jsonb,
      output_variables = $6::jsonb, specification = $7::jsonb,
      validation_status = 'validado'
    WHERE id = $8 AND owner_username = $9",
    params = list(
      trimws(spec$name),
      spec$description,
      generate_model_code(spec),
      json(spec$params),
      json(as.list(get_model_cmt(mod))),
      json(as.list(spec$outputs)),
      json(saved),
      as.character(model_id),
      as.character(username)
    )
  )

  if (changed != 1L) stop("El modelo ya no está disponible.", call. = FALSE)
  invisible(NULL)
}
