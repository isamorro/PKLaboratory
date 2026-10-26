# ============================================================
# Archivo: server.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la lógica del servidor y gestiona el funcionamiento
# reactivo de la aplicación PKLaboratory.
# ============================================================

# Gestiona la lógica y conecta las entradas del usuario
# con las simulaciones y la presentación de los resultados.
# Recibe: entradas, salidas y sesión de Shiny.
server <- function(input, output, session) {

  # Estado de la sesión.
  logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)

  # Modelos creados por el usuario: id -> datos del modelo. Los guardados en la
  # base de datos tienen id "db_<n>" y se cargan al iniciar sesión; los que no
  # se pudieron guardar tienen id "tmp_<n>" y solo duran lo que la sesión.
  user_models <- reactiveVal(list())
  selected_model_id <- reactiveVal(default_model_id)
  shared_models <- reactiveVal(empty_shared_models())
  shared_cache <- new.env(parent = emptyenv())
  compile_prefix <- paste0("shared_", gsub("[^A-Za-z0-9]", "",
                                           session$token), "_")
  session$onSessionEnded(function() remove_compiled_files(compile_prefix))

  navigation_controller(input, session)
  help_controller(output)

  refresh_catalog <- shared_catalog_controller(
    input,
    output,
    session,
    user_models,
    shared_models,
    database_pool,
    selected_model_id,
    current_user
  )
  model <- model_controller(
    input,
    output,
    session,
    user_models,
    shared_models,
    shared_cache,
    database_pool,
    compile_prefix,
    current_user
  )
  builder_controller(
    input,
    output,
    session,
    user_models,
    shared_models,
    selected_model_id,
    database_pool,
    current_user,
    shared_cache
  )
  simulation <- simulation_controller(input, output, model)
  population_controller(input, output, session, user_models, shared_models,
                        shared_cache, database_pool, compile_prefix,
                        current_user)
  # Nombre del modelo para mostrar (gráfico y descargas), no su id interno.
  current_model_label <- function(id) {
    model_label(
      id,
      shiny::isolate(user_models()),
      shiny::isolate(shared_models())
    )
  }
  results_controller(input, output, session, simulation, current_model_label)

  output$app_page <- renderUI({
    if (isTRUE(logged_in())) {
      main_app_ui(
        model_choices(
          shiny::isolate(user_models()),
          shiny::isolate(shared_models())
        ),
        shiny::isolate(selected_model_id())
      )
    } else {
      login_ui()
    }
  })

  observeEvent(input$login_submit, {
    if (is.null(database_pool)) {
      showNotification(
        "La base de datos de autenticación no está configurada.",
        type = "error",
        duration = NULL
      )
      return()
    }

    username <- trimws(input$username)
    password <- input$password

    if (!nzchar(username) || !nzchar(password)) {
      showNotification(
        "Introduce usuario y contraseña.",
        type = "error",
        duration = NULL
      )
      return()
    }

    wait <- login_wait_seconds(username)
    if (wait > 0) {
      showNotification(
        paste0("Demasiados intentos fallidos. Vuelve a intentarlo en ",
               describe_wait(wait), "."),
        type = "error",
        duration = NULL
      )
      return()
    }

    db_error <- FALSE
    user <- tryCatch(
      verify_user_credentials(database_pool, username, password),
      error = function(e) {
        message("[login] ", conditionMessage(e))
        db_error <<- TRUE
        NULL
      }
    )

    if (db_error) {
      showNotification(
        "No se pudo comprobar el acceso. Inténtalo de nuevo en unos minutos.",
        type = "error",
        duration = NULL
      )
      return()
    }

    if (is.null(user)) {
      register_login_failure(username)
      showNotification(
        "Usuario o contraseña incorrectos.",
        type = "error",
        duration = NULL
      )
      return()
    }

    clear_login_failures(username)

    # Se cargan los modelos guardados de la cuenta antes de mostrar la
    # aplicación, para que ya aparezcan en el selector.
    current_user(user)
    if (!is.null(database_pool) && !refresh_catalog()) {
      showNotification(
        "No se pudieron cargar tus modelos guardados.",
        type = "warning"
      )
    }
    logged_in(TRUE)
  })

  observeEvent(input$logout, {
    logged_in(FALSE)
    current_user(NULL)

    # Nada de lo de este usuario debe quedar a la vista del siguiente.
    user_models(list())
    shared_models(empty_shared_models())
    selected_model_id(default_model_id)
    rm(list = ls(shared_cache, all.names = TRUE), envir = shared_cache)
  })
}
