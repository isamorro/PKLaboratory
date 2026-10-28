# ============================================================
# Archivo: population.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene el controlador del análisis poblacional: lee los datos de un
# CSV, detecta las posibles covariables y ajusta un modelo mixto no lineal
# (Lindstrom y Bates, con nlme) para el modelo seleccionado.
# ============================================================

# Controla la página de análisis poblacional: datos del CSV, posibles
# covariables, ajustes individuales y estimación del modelo mixto para cada
# modelo disponible.
# Recibe: entradas, salidas y sesión de Shiny, reactive con los modelos del
# usuario, reactive con el catálogo compartido, caché de modelos
# compartidos, conexión a la base de datos, prefijo para compilar los
# modelos y reactive con el usuario activo.
population_controller <- function(input, output, session, user_models,
                                  shared_models = NULL, shared_cache = NULL,
                                  db = NULL, compile_prefix = "pop_",
                                  current_user = NULL) {

  # Identificador de los campos de la página.
  id <- function(s) paste0("pop_nlme_", s)

  # ---- Modelo ---------------------------------------------------------

  # Modelos que se pueden elegir.
  shared <- function() if (is.null(shared_models)) NULL else shared_models()
  catalogue <- shiny::reactive(model_choices(user_models(), shared()))

  # Selector de modelo; conserva el elegido si sigue en el catálogo.
  output[[id("catalog_selector")]] <- shiny::renderUI({
    options <- catalogue()
    all_ids <- unlist(options, use.names = FALSE)
    selected <- input[[id("catalog")]]
    if (length(selected) != 1L || is.na(selected) ||
          !(selected %in% all_ids)) {
      selected <- if ("pk1" %in% all_ids) {
        "pk1"
      } else if (length(all_ids)) {
        all_ids[[1]]
      } else {
        character()
      }
    }
    shiny::selectInput(
      id("catalog"),
      "Modelo",
      choices = options,
      selected = selected
    )
  })

  # Descripción del modelo seleccionado, junto al selector.
  output[[id("model_description")]] <- shiny::renderUI({
    text <- model_description(input[[id("catalog")]], user_models(), shared())
    if (nzchar(text)) shiny::p(text)
  })

  # Ventana con el código del modelo seleccionado. Antes se carga el
  # modelo, para que los compartidos estén en la caché.
  shiny::observeEvent(input[[id("model_code_view")]], {
    selected <- input[[id("catalog")]]
    shiny::req(length(selected) == 1L, !is.na(selected))
    tryCatch(model(), error = function(e) NULL)
    shiny::showModal(model_code_modal(
      model_label(selected, user_models(), shared()),
      model_description(selected, user_models(), shared()),
      model_source_code(selected, user_models(), shared_cache)
    ))
  })

  # Modelo seleccionado, con el mismo cargador que la página de
  # simulación (biblioteca, propios y catálogo compartido).
  model <- shiny::reactive({
    selected <- input[[id("catalog")]]
    shiny::req(length(selected) == 1L, !is.na(selected), nzchar(selected))
    user <- if (is.null(current_user)) NULL else current_user()
    mod <- resolve_model(
      as.character(selected), user_models(),
      db = db, shared_cache = shared_cache,
      compile_prefix = compile_prefix,
      username = if (is.null(user)) NULL else user$username
    )
    if (!methods::is(mod, "mrgmod")) {
      stop(sprintf(
        "El modelo '%s' no se ha resuelto como objeto mrgmod (clases: %s).",
        selected, paste(class(mod), collapse = ", ")
      ), call. = FALSE)
    }
    mod
  })

  # Error al cargar o compilar el modelo (NULL si se ha cargado), para
  # mostrarlo en la interfaz sin interrumpir renderUI.
  model_error <- shiny::reactive({
    tryCatch(
      {
        model()
        NULL
      },
      error = function(e) {
        # Mientras se crea el selector, req() detiene model() con un error
        # silencioso: no es un fallo, así que no se muestra ningún aviso.
        if (inherits(e, "shiny.silent.error")) stop(e)
        conditionMessage(e)
      }
    )
  })

  # Estructura del análisis: siempre con KE en lugar del volumen central
  # (V = CL / KE), como SSfol en Pinheiro y Bates, si el modelo tiene esa
  # forma; si no, la del modelo.
  analysis_structure <- shiny::reactive({
    st <- population_structure(model())
    ke <- population_ke_structure(st)
    if (is.null(ke)) st else ke
  })

  # Estructura para el análisis, o NULL si el modelo no se ha podido
  # cargar.
  structure_or_null <- function() {
    tryCatch(analysis_structure(), error = function(e) NULL)
  }

  # Indicador numérico del resumen de resultados.
  stat <- function(value, label) {
    shiny::div(
      class = "pk-stat",
      shiny::div(class = "pk-stat-value", value),
      shiny::div(class = "pk-stat-label", label)
    )
  }

  # Aviso cuando el modelo no se ha podido cargar.
  model_error_ui <- function(message) {
    shiny::div(
      class = "alert alert-danger",
      shiny::strong("No se ha podido cargar el modelo mrgsolve. "),
      shiny::tags$div(message),
      shiny::tags$small(
        "Comprueba la selección del catálogo y el error indicado arriba. ",
        "También puedes probar en la consola: mrgsolve::modlib('pk1')."
      )
    )
  }

  # Etiqueta junto al modelo: si se puede estimar o no (con el motivo).
  output[[id("model_details")]] <- shiny::renderUI({
    if (!is.null(model_error())) {
      return(model_error_ui(model_error()))
    }
    st <- structure_or_null()
    shiny::div(
      class = "pk-pop-badges",
      if (isTRUE(st$ok)) {
        shiny::span(
          class = "pk-pop-badge pk-pop-badge-ok",
          "Estimación (Lindstrom-Bates)"
        )
      } else {
        shiny::span(
          class = "pk-pop-badge pk-pop-badge-no",
          title = st$reason,
          "Sin estimación"
        )
      }
    )
  })

  # ---- Datos ----------------------------------------------------------

  # Las tablas largas muestran sus primeras filas en un recuadro con
  # desplazamiento (unas 10 a la vista).
  max_rows <- 150L

  # Datos del CSV subido.
  csv_data <- shiny::reactive({
    file <- input[[id("file")]]
    shiny::req(file)
    population_read_csv(file$datapath)
  })

  # Datos del CSV, o NULL si todavía no hay o no son válidos.
  data_or_null <- function() {
    tryCatch(csv_data(), error = function(e) NULL)
  }

  # Aviso si los datos no se pueden usar.
  output[[id("dataset_message")]] <- shiny::renderUI({
    tryCatch(
      {
        csv_data()
        NULL
      },
      error = function(e) {
        # Antes de subir datos, req() detiene csv_data() con un error
        # silencioso y sin mensaje: no hay nada que avisar.
        if (inherits(e, "shiny.silent.error") ||
              !nzchar(conditionMessage(e))) {
          return(NULL)
        }
        shiny::div(
          class = "alert alert-warning pk-pop-alert",
          conditionMessage(e)
        )
      }
    )
  })

  # Indicador para los conditionalPanel de la vista.
  output[[id("has_data")]] <- shiny::reactive(!is.null(data_or_null()))
  shiny::outputOptions(output, id("has_data"), suspendWhenHidden = FALSE)

  # Columnas del paciente, el tiempo, la concentración y la dosis
  # encontradas en el CSV por su nombre.
  csv_columns <- shiny::reactive(population_guess_columns(csv_data()))

  # Selector de una unidad, al lado de su columna.
  unit_input <- function(name, units) {
    shiny::selectInput(id(name), "Unidad", choices = names(units))
  }

  # Columnas del paciente, el tiempo y la concentración: se proponen las
  # encontradas en el CSV.
  output[[id("variables")]] <- shiny::renderUI({
    dat <- data_or_null()
    if (is.null(dat)) {
      return(NULL)
    }
    g <- csv_columns()
    cols <- names(dat)
    nums <- cols[vapply(dat, is.numeric, logical(1))]
    shiny::tagList(
      shiny::selectInput(
        id("group"), "Identificador del paciente",
        choices = cols, selected = g$group
      ),
      shiny::div(
        class = "pk-pop-inline",
        shiny::selectInput(
          id("time"), "Tiempo", choices = nums, selected = g$time
        ),
        unit_input("time_unit", population_time_units)
      ),
      shiny::div(
        class = "pk-pop-inline",
        shiny::selectInput(
          id("response"), "Concentración observada",
          choices = nums, selected = g$response
        ),
        unit_input("conc_unit", population_conc_units)
      )
    )
  })

  # Si el modelo elegido necesita la dosis (tiene compartimentos).
  needs_dose <- function(st) isTRUE(st$ok) && length(st$cmts) > 0L

  # Compartimento de la dosis elegido, o el del modelo si el elegido no es
  # de este modelo.
  # Recibe: estructura del modelo y, opcionalmente, shiny::isolate para no
  # depender de la entrada.
  dose_cmt_for <- function(st, read = identity) {
    cmt <- read(input[[id("fit_dose_cmt")]])
    if (length(cmt) == 1L && cmt %in% st$cmts) cmt else st$dose_cmt
  }

  # Dosis: columna, unidad, peso (con mg/kg) y compartimento donde entra.
  # Solo si el modelo elegido la necesita.
  output[[id("dose_inputs")]] <- shiny::renderUI({
    st <- structure_or_null()
    dat <- data_or_null()
    if (is.null(dat) || !needs_dose(st)) {
      return(NULL)
    }
    nums <- names(dat)[vapply(dat, is.numeric, logical(1))]
    # Se conserva la columna elegida al cambiar de modelo.
    dose_column <- shiny::isolate(input[[id("dose_column")]])
    if (length(dose_column) != 1L || !dose_column %in% nums) {
      dose_column <- csv_columns()$dose
    }
    weight_guess <- nums[
      population_known_covariates[toupper(nums)] %in% "Peso"
    ]
    per_kg <- sprintf("input['%s'] == 'mg/kg'", id("dose_unit"))
    shiny::tagList(
      shiny::div(
        class = "pk-pop-inline",
        shiny::selectInput(
          id("dose_column"), "Dosis de cada paciente",
          choices = nums, selected = dose_column
        ),
        unit_input("dose_unit", population_dose_units)
      ),
      shiny::conditionalPanel(
        per_kg,
        shiny::selectInput(
          id("weight_column"), "Peso de cada paciente (kg)",
          choices = nums,
          selected = if (length(weight_guess)) weight_guess[[1]] else nums[[1]]
        )
      ),
      shiny::selectInput(
        id("fit_dose_cmt"), "Compartimento donde entra la dosis",
        choices = st$cmts, selected = dose_cmt_for(st, shiny::isolate)
      )
    )
  })

  # Columnas elegidas para el paciente, el tiempo, la concentración y la
  # dosis (solo si el modelo la necesita), comprobadas frente a los datos.
  # Van aparte de las unidades para que cambiar una unidad no vuelva a
  # revisar las covariables.
  data_columns <- shiny::reactive({
    dat <- csv_data()
    group <- input[[id("group")]]
    time <- input[[id("time")]]
    response <- input[[id("response")]]
    shiny::req(
      length(group) == 1L, length(time) == 1L, length(response) == 1L,
      all(c(group, time, response) %in% names(dat))
    )
    dose_column <- NULL
    if (needs_dose(structure_or_null())) {
      dose_column <- input[[id("dose_column")]]
      shiny::req(length(dose_column) == 1L, dose_column %in% names(dat))
    }
    list(
      group = group, time = time, response = response,
      dose_column = dose_column
    )
  })

  # Columnas elegidas y sus unidades.
  data_roles <- shiny::reactive({
    r <- data_columns()
    shiny::req(input[[id("time_unit")]], input[[id("conc_unit")]])
    dose_unit <- "mg"
    if (!is.null(r$dose_column)) {
      dose_unit <- input[[id("dose_unit")]]
      shiny::req(dose_unit)
    }
    c(r, list(
      time_unit = input[[id("time_unit")]],
      conc_unit = input[[id("conc_unit")]], dose_unit = dose_unit,
      weight = if (identical(dose_unit, "mg/kg")) {
        input[[id("weight_column")]]
      }
    ))
  })

  # Nombre del modelo elegido.
  selected_label <- function() {
    model_label(input[[id("catalog")]], user_models(), shared())
  }

  # Comprobación de los datos frente al modelo elegido: lo que falta para
  # poder ajustarlo y avisos.
  data_check <- shiny::reactive({
    st <- analysis_structure()
    shiny::req(isTRUE(st$ok))
    # Primero, que el CSV tenga las columnas que necesita el modelo.
    missing <- population_missing_columns(csv_columns(), needs_dose(st))
    if (length(missing)) {
      return(list(missing = missing, warnings = character()))
    }
    r <- data_roles()
    res <- population_check_data(
      st, csv_data(), r$group, r$time, r$response, dose_cmt_for(st)
    )
    # Los errores al pasar a h, mg y mg/L (por ejemplo, mg/kg sin peso)
    # dependen de las unidades elegidas: también impiden ajustar, pero se
    # corrigen cambiando la elección.
    res$conversion <- tryCatch(
      {
        data_for_fit()
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    res
  })

  # Comprobación, o NULL mientras no hay datos o columnas elegidas.
  check_or_null <- shiny::reactive({
    tryCatch(data_check(), error = function(e) NULL)
  })

  # Indicadores para los conditionalPanel: el CSV tiene lo que necesita el
  # modelo (csv_ok) y, además, las unidades elegidas son válidas (data_ok).
  output[[id("csv_ok")]] <- shiny::reactive({
    res <- check_or_null()
    !is.null(res) && !length(res$missing)
  })
  output[[id("data_ok")]] <- shiny::reactive({
    res <- check_or_null()
    !is.null(res) && !length(res$missing) && is.null(res$conversion)
  })
  # Las columnas se eligen aunque estén ocultas: sin ellas no se puede
  # comprobar el CSV.
  for (o in c("csv_ok", "data_ok", "variables", "dose_inputs")) {
    shiny::outputOptions(output, id(o), suspendWhenHidden = FALSE)
  }

  # Error al pasar los datos a h, mg y mg/L con las unidades elegidas.
  output[[id("conversion_message")]] <- shiny::renderUI({
    res <- check_or_null()
    if (is.null(res) || is.null(res$conversion)) {
      return(NULL)
    }
    shiny::div(class = "alert alert-warning pk-pop-alert", res$conversion)
  })

  # Aviso si al CSV le falta algo que necesita el modelo elegido o si hay
  # algo que conviene revisar.
  output[[id("data_check")]] <- shiny::renderUI({
    res <- check_or_null()
    if (is.null(res)) {
      return(NULL)
    }
    name <- selected_label()
    items <- function(x) shiny::tags$ul(lapply(x, shiny::tags$li))
    if (length(res$missing)) {
      return(shiny::div(
        class = "alert alert-danger pk-pop-alert",
        shiny::strong(sprintf(
          "Este CSV no contiene los datos necesarios para el modelo %s.",
          name
        )),
        shiny::p("Falta:"),
        items(res$missing)
      ))
    }
    if (length(res$warnings)) {
      return(shiny::div(
        class = "alert alert-warning pk-pop-alert",
        shiny::strong(sprintf(
          "El CSV tiene los datos necesarios para el modelo %s, pero:", name
        )),
        items(res$warnings)
      ))
    }
    # Si los datos sirven, no se muestra nada.
    NULL
  })

  # Columnas del CSV revisadas como posibles covariables.
  detected_covariates <- shiny::reactive({
    r <- data_columns()
    population_detect_covariates(
      csv_data(), r$group, c(r$time, r$response, r$dose_column)
    )
  })

  # Datos que se analizan: con las covariables categóricas como texto y el
  # tiempo, la concentración y la dosis en horas, mg/L y mg.
  data_for_fit <- shiny::reactive({
    r <- data_roles()
    dat <- population_prepare_covariates(csv_data(), detected_covariates())
    population_convert_units(
      dat, r$time, r$response, r$dose_column, r$time_unit, r$conc_unit,
      r$dose_unit, r$weight
    )
  })

  # Resumen en una sola línea encima de las tarjetas.
  output[[id("data_summary")]] <- shiny::renderUI({
    dat <- data_or_null()
    shiny::req(dat)
    group <- input[[id("group")]]
    n_ids <- if (length(group) == 1L && group %in% names(dat)) {
      length(unique(dat[[group]]))
    } else {
      "—"
    }
    usable <- tryCatch(
      sum(detected_covariates()$Usable),
      error = function(e) "—"
    )
    shiny::div(
      class = "pk-pop-databar",
      shiny::span(
        class = "pk-pop-databar-text",
        shiny::strong(n_ids), " pacientes · ",
        shiny::strong(nrow(dat)), " registros · ",
        shiny::strong(ncol(dat)), " columnas · ",
        shiny::strong(usable), " posibles covariables"
      )
    )
  })

  # Tabla de posibles covariables o, si no hay, un aviso.
  output[[id("covariates")]] <- shiny::renderUI({
    det <- detected_covariates()
    if (!nrow(det)) {
      return(shiny::p(
        class = "pk-component-note m-0",
        "No se han encontrado posibles covariables."
      ))
    }
    shiny::div(
      class = "pk-pop-table pk-pop-scroll",
      shiny::tableOutput(id("covariate_table"))
    )
  })

  # Tabla con las columnas revisadas como posibles covariables.
  output[[id("covariate_table")]] <- shiny::renderTable(
    {
      det <- detected_covariates()
      shiny::req(nrow(det) > 0)
      data.frame(
        Columna = det$Columna,
        `Qué parece` = det$Indica,
        Tipo = det$Tipo,
        `Valores (por paciente)` = det$Valores,
        `¿Se puede usar?` = ifelse(det$Usable, "Sí", paste("No.", det$Motivo)),
        check.names = FALSE
      )
    },
    striped = TRUE
  )

  # Concentraciones de cada paciente a lo largo del tiempo.
  output[[id("data_plot")]] <- shiny::renderPlot({
    r <- data_roles()
    dat <- csv_data()
    narrow <- plot_is_narrow(session, id("data_plot"))
    ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = .data[[r$time]], y = .data[[r$response]],
        group = .data[[r$group]]
      )
    ) +
      ggplot2::geom_line(alpha = .4, colour = "#187A5B") +
      ggplot2::geom_point(alpha = .5, colour = "#187A5B", size = 1) +
      ggplot2::scale_x_continuous(n.breaks = if (narrow) 4 else 6) +
      ggplot2::theme_minimal(base_size = if (narrow) 10 else 11) +
      ggplot2::labs(
        x = sprintf("%s (%s)", r$time, r$time_unit),
        y = sprintf("%s (%s)", r$response, r$conc_unit),
        title = "Concentraciones de cada paciente"
      )
  })

  # Primeras filas del CSV.
  output[[id("data_preview")]] <- shiny::renderTable(
    {
      dat <- data_or_null()
      shiny::req(dat)
      utils::head(dat, max_rows)
    },
    striped = TRUE
  )

  # Pasos: el selector de la barra superior muestra el paso; los botones de
  # cada paso lo cambian a través de él.
  go_to <- function(stage) {
    shiny::updateRadioButtons(session, id("stage"), selected = stage)
  }
  shiny::observeEvent(input[[id("stage")]], {
    bslib::nav_select(id("tabs"), input[[id("stage")]], session = session)
  })
  shiny::observeEvent(input[[id("go_analysis")]], go_to("analysis"))
  shiny::observeEvent(input[[id("back_data")]], go_to("data"))
  shiny::observeEvent(input[[id("back_analysis")]], go_to("analysis"))
  shiny::observeEvent(input[[id("back_analysis_empty")]], go_to("analysis"))

  # ---- Opciones del análisis (las decide el usuario) ------------------

  # Escala de cada parámetro en el ajuste: logarítmica (por defecto, como
  # lKe, lKa y lCl en Theoph) o natural (sin transformar).
  output[[id("scales")]] <- shiny::renderUI({
    st <- structure_or_null()
    if (is.null(st) || !isTRUE(st$ok)) {
      return(NULL)
    }
    page_section(
      "Escala de los parámetros",
      shiny::checkboxGroupInput(
        id("log_params"), "Se estiman en escala logarítmica",
        choices = st$params, selected = st$log_params, inline = TRUE
      ),
      if (!is.null(st$ke)) {
        shiny::p(
          class = "pk-component-note",
          "Se estima KE que se calcula como CL / V."
        )
      }
    )
  })

  # Parámetros con efecto aleatorio marcados por defecto.
  default_random_for <- function(params) {
    d <- intersect(c("CL", "KA"), params)
    if (length(d)) d else params[[1]]
  }

  # Opciones del modelo mixto: efectos aleatorios, covariables, error de
  # medida y opciones avanzadas.
  output[[id("fit_options")]] <- shiny::renderUI({
    if (!is.null(model_error())) {
      return(NULL)
    }
    st <- structure_or_null()
    if (is.null(st)) {
      return(NULL)
    }
    if (!isTRUE(st$ok)) {
      return(shiny::div(
        class = "alert alert-warning pk-pop-alert",
        shiny::strong("Este modelo no se puede estimar. "),
        paste0("Motivo: ", st$reason, ".")
      ))
    }
    default_random <- default_random_for(st$params)
    shiny::tagList(
      page_section(
        "Efectos aleatorios",
        shiny::checkboxGroupInput(
          id("estimate_random"), "Parámetros que cambian entre pacientes",
          choices = st$params, selected = default_random, inline = TRUE
        ),
        # La estructura de Psi solo importa con dos o más efectos
        # aleatorios.
        shiny::conditionalPanel(
          sprintf("(input['%s'] || []).length > 1", id("estimate_random")),
          shiny::radioButtons(
            id("omega"),
            "Matriz de covarianzas de los efectos aleatorios (Ψ)",
            choices = population_omega_structures
          )
        )
      ),
      page_section(
        "Covariables",
        shiny::uiOutput(id("covariate_options"))
      ),
      page_section(
        "Estimación",
        shiny::selectInput(
          id("error_model"), "Error de medida",
          choices = population_error_models
        )
      ),
      shiny::tags$details(
        class = "pk-pop-details",
        shiny::tags$summary("Opciones avanzadas"),
        shiny::div(
          class = "pk-pop-details-body",
          shiny::div(
            class = "pk-pop-inline",
            shiny::selectInput(
              id("observed"), "Salida medida",
              choices = st$outputs, selected = st$observed
            ),
            shiny::selectizeInput(
              id("fixed_params"),
              "Parámetros con valor fijado (no se estiman)",
              choices = st$params, multiple = TRUE,
              options = list(placeholder = "Ninguno")
            )
          ),
          shiny::p(
            class = "pk-component-note",
            paste(
              "Estos parámetros se dejan fijos en su valor inicial."
            )
          ),
          shiny::radioButtons(
            id("cov_form"), "Forma del efecto de las covariables continuas",
            choices = population_covariate_forms
          ),
          shiny::uiOutput(id("fit_starts"))
        )
      )
    )
  })

  # Valores iniciales propuestos a partir de los ajustes individuales; se
  # descartan al cambiar de modelo.
  suggested_starts <- shiny::reactiveVal(numeric())
  shiny::observeEvent(input[[id("catalog")]], suggested_starts(numeric()))

  # Valor inicial de un parámetro: el propuesto o, si no hay, el del
  # modelo.
  start_value <- function(st, p) {
    s <- suggested_starts()
    if (p %in% names(s) && is.finite(s[[p]])) {
      s[[p]]
    } else {
      unname(st$values[[p]])
    }
  }

  # Campos con los valores iniciales de los parámetros.
  output[[id("fit_starts")]] <- shiny::renderUI({
    st <- structure_or_null()
    if (is.null(st) || !isTRUE(st$ok)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::p(
        class = "pk-pop-subtitle",
        if (length(suggested_starts())) {
          "Valores iniciales (de los ajustes individuales)"
        } else {
          "Valores iniciales (por defecto, los del modelo)"
        }
      ),
      build_param_inputs(
        stats::setNames(
          lapply(st$params, function(p) signif(start_value(st, p), 4)),
          st$params
        ),
        prefix = id("start_"),
        labels = stats::setNames(
          vapply(st$params, function(p) {
            unit <- population_param_unit(p)
            if (nzchar(unit)) sprintf("%s (%s)", p, unit) else p
          }, character(1)),
          st$params
        )
      )
    )
  })

  # Covariables que se pueden incluir (las detectadas en el CSV) y sobre
  # qué parámetro actúan.
  output[[id("covariate_options")]] <- shiny::renderUI({
    st <- structure_or_null()
    det <- tryCatch(detected_covariates(), error = function(e) NULL)
    if (is.null(st) || !isTRUE(st$ok) || is.null(det)) {
      return(NULL)
    }
    det <- det[det$Usable, , drop = FALSE]
    if (!nrow(det)) {
      return(shiny::p(
        class = "pk-component-note",
        paste(
          "El CSV no tiene columnas que se puedan usar como covariables",
          "(revisa «Posibles covariables» en el paso 1)."
        )
      ))
    }
    labels <- ifelse(
      det$Indica == "—",
      det$Columna,
      sprintf("%s (%s)", det$Indica, det$Columna)
    )
    choices <- stats::setNames(lapply(det$Columna, function(col) {
      stats::setNames(
        paste0(col, "|", st$params),
        paste(col, "sobre", st$params)
      )
    }), labels)
    shiny::tagList(
      shiny::selectizeInput(
        id("cov_effects"),
        "Características del paciente que influyen en un parámetro (opcional)",
        choices = choices, multiple = TRUE,
        options = list(
          placeholder = sprintf("Ej.: %s sobre %s", det$Columna[[1]],
                                st$params[[1]])
        )
      )
    )
  })

  # ---- Ajuste ---------------------------------------------------------

  shiny::observeEvent(input[[id("fit")]], go_to("results"))

  # Reúne los datos, el modelo y las columnas comunes a los ajustes
  # individuales y al mixto.
  fit_inputs <- function() {
    st <- analysis_structure()
    if (!isTRUE(st$ok)) {
      stop("Este modelo no se puede estimar: ", st$reason, ".")
    }
    checks <- data_check()
    if (length(checks$missing)) {
      stop(
        "Este CSV no contiene los datos necesarios para el modelo ",
        selected_label(), ". Falta: ", paste(checks$missing, collapse = " ")
      )
    }
    if (!is.null(checks$conversion)) {
      stop(checks$conversion)
    }
    dat <- data_for_fit()
    fixed <- input[[id("fixed_params")]]
    if (is.null(fixed)) fixed <- character()
    dose_cmt <- dose_cmt_for(st)
    roles <- data_roles()
    # Valores iniciales: los que haya dejado el usuario o los propuestos
    # por los ajustes individuales.
    st$log_params <- intersect(
      as.character(input[[id("log_params")]]), st$params
    )
    starts <- read_param_inputs(input, st$params, prefix = id("start_"))
    for (p in st$params) {
      st$values[[p]] <- if (is_valid_number(starts[[p]])) {
        starts[[p]]
      } else {
        start_value(st, p)
      }
    }
    list(
      dat = dat, st = st, response = roles$response, group = roles$group,
      time = roles$time, dose_cmt = dose_cmt,
      # La dosis de cada paciente, en mg, está en la columna interna.
      dose_column = if (population_dose_mg %in% names(dat)) {
        population_dose_mg
      },
      observed = input[[id("observed")]], fixed = fixed
    )
  }

  # ---- Ajustes individuales (exploratorio, con nlsList) ---------------

  # Resultado de un ajuste que ha fallado: el mensaje y, si el error viene
  # del proceso de estimación, la causa, qué probar, el mensaje de R y el
  # registro.
  # Recibe: error.
  # Devuelve: lista con ok = FALSE y esos campos.
  fit_failure <- function(e) {
    list(
      ok = FALSE, error = conditionMessage(e), cause = e$cause,
      hints = e$hints, r_message = e$r_message, log = e$log
    )
  }

  # Lanza un ajuste en un proceso aparte sin bloquear la aplicación
  # (ExtendedTask): mientras dura, esta sesión y las de los demás usuarios
  # siguen respondiendo, y las salidas que dependen del ajuste se muestran
  # como «calculando».
  # Recibe: id del botón; prepare(), que reúne los datos del ajuste y
  # devuelve una lista con run (función que lanza el ajuste y devuelve su
  # promesa) y finish (convierte su resultado en lo que muestran las
  # salidas), y los textos de la barra de progreso. Si prepare() falla, el
  # error se muestra al momento.
  # Devuelve: lista con result (reactive con el resultado o el error, ver
  # fit_failure()) y running (reactive, TRUE mientras dura el ajuste).
  background_fit <- function(button, prepare, ...) {
    prepare_error <- shiny::reactiveVal(NULL)
    task <- shiny::ExtendedTask$new(function(job) {
      progress <- shiny::Progress$new(session)
      progress$set(..., value = 0.3)
      promises::finally(
        promises::catch(promises::then(job$run(), job$finish), fit_failure),
        function() progress$close()
      )
    })
    shiny::observeEvent(input[[id(button)]], {
      job <- tryCatch(prepare(), error = function(e) {
        prepare_error(fit_failure(e))
        NULL
      })
      if (!is.null(job)) {
        prepare_error(NULL)
        task$invoke(job)
      }
    })
    list(
      result = shiny::reactive({
        error <- prepare_error()
        if (is.null(error)) task$result() else error
      }),
      running = shiny::reactive(task$status() == "running")
    )
  }

  # Aviso mientras se ajusta en segundo plano.
  # Recibe: texto del aviso.
  # Devuelve: alerta informativa.
  running_ui <- function(text) {
    shiny::div(
      class = "alert alert-info pk-pop-alert",
      shiny::strong(text), " Puede tardar de segundos a varios minutos;",
      "mientras tanto puedes seguir usando la aplicación."
    )
  }

  # Nombre de un parámetro de los ajustes individuales, con log() si se
  # estima en escala logarítmica.
  # Recibe: nombres de los parámetros y parámetros en escala log.
  # Devuelve: vector con los nombres para mostrar.
  log_label <- function(p, logs) {
    ifelse(p %in% logs, paste0("log(", p, ")"), p)
  }

  # Parámetros que se estiman en cada paciente; los demás se dejan en su
  # valor inicial.
  output[[id("individual_params")]] <- shiny::renderUI({
    st <- structure_or_null()
    if (is.null(st) || !isTRUE(st$ok)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::checkboxGroupInput(
        id("individual_params"), "Parámetros que se estiman en cada paciente",
        choices = st$params, selected = st$params, inline = TRUE
      )
    )
  })

  # Resultado de pulsar "Ajustar cada paciente": estimaciones o error.
  individual_fit <- background_fit(
    "individual",
    function() {
      a <- fit_inputs()
      chosen <- input[[id("individual_params")]]
      if (!length(chosen)) {
        stop("Marca al menos un parámetro para ajustar cada paciente.")
      }
      a$fixed <- union(a$fixed, setdiff(a$st$params, chosen))
      list(
        run = function() {
          population_run_isolated(
            "population_individual_fits",
            list(
              a$dat, a$st, a$response, a$group, a$time, a$dose_cmt,
              a$dose_column, a$observed, a$fixed
            )
          )
        },
        finish = function(fits) {
          # Valores con los que se han dejado fijos los no marcados, para
          # explicar los ajustes que fallen.
          fixed <- a$st$values[setdiff(a$fixed, attr(fits, "inert"))]
          list(ok = TRUE, fits = fits, chosen = chosen, fixed = fixed)
        }
      )
    },
    message = "Ajustando cada paciente por separado..."
  )
  individual <- individual_fit$result

  # Resumen de los ajustes individuales y botón para usar sus medianas.
  output[[id("individual_status")]] <- shiny::renderUI({
    if (individual_fit$running()) {
      return(running_ui("Ajustando cada paciente por separado…"))
    }
    res <- individual()
    if (!isTRUE(res$ok)) {
      return(fit_error_ui(
        "No se han podido ajustar los pacientes.", res,
        if (length(res$log)) {
          shiny::tags$details(
            class = "pk-pop-log",
            shiny::tags$summary("Ver el registro del ajuste"),
            shiny::tags$pre(paste(res$log, collapse = "\n"))
          )
        }
      ))
    }
    ok <- tapply(is.finite(res$fits$Estimacion), res$fits$Paciente, all)
    inert <- attr(res$fits, "inert")
    # Si fallan pacientes con parámetros fijos, lo más probable es que los
    # marcados no puedan describir los datos con esos valores fijos: se
    # indica con qué parámetros hay que ajustarlos.
    fixed_alert <- if (sum(!ok) > 0L && length(res$fixed)) {
      units <- vapply(names(res$fixed), population_param_unit, character(1))
      values <- paste(
        sprintf(
          "%s = %s%s", names(res$fixed), signif(res$fixed, 4),
          ifelse(nzchar(units), paste0(" ", units), "")
        ),
        collapse = ", "
      )
      chosen <- paste(setdiff(res$chosen, inert), collapse = ", ")
      others <- paste(names(res$fixed), collapse = ", ")
      shiny::div(
        class = "alert alert-warning pk-pop-alert",
        shiny::strong(sprintf(
          "%d de %d pacientes no se han podido ajustar estimando solo %s. ",
          sum(!ok), length(ok), chosen
        )),
        sprintf(
          paste(
            "Los demás parámetros se han dejado fijos (%s) y, con esos",
            "valores, %s no basta para describir las concentraciones.",
            "Ajusta %s junto con %s o, antes, ajusta todos los parámetros",
            "y pulsa «Usar la mediana como valores iniciales» para fijarlos",
            "en valores de tus datos."
          ),
          values, chosen, chosen, others
        )
      )
    }
    shiny::tagList(fixed_alert, shiny::div(
      class = "pk-pop-statusbar",
      shiny::p(
        class = "pk-component-note",
        sprintf(
          paste(
            "Ajustados %d de %d pacientes. Si los intervalos de un",
            "parámetro no se solapan entre pacientes, ese parámetro parece",
            "variar entre ellos: candidato a efecto aleatorio."
          ),
          sum(ok), length(ok)
        ),
        if (length(inert)) {
          sprintf(
            paste(
              " No se estiman %s: con esta dosis no influyen en la salida",
              "medida y se dejan en su valor."
            ),
            paste(inert, collapse = ", ")
          )
        }
      ),
      shiny::actionButton(
        id("use_individual_starts"),
        "Usar la mediana como valores iniciales",
        class = "btn-sm btn-outline-primary"
      )
    ))
  })

  # Estimación e IC del 95 % de cada paciente, por parámetro. En el móvil los
  # parámetros van uno debajo de otro, y la altura crece con los pacientes.
  output[[id("individual_plot")]] <- shiny::renderPlot({
    narrow <- plot_is_narrow(session, id("individual_plot"))
    res <- individual()
    shiny::req(isTRUE(res$ok))
    d <- res$fits[is.finite(res$fits$Estimacion), , drop = FALSE]
    shiny::validate(shiny::need(
      nrow(d) > 0,
      paste(
        "Ningún paciente se ha podido ajustar. Revisa los valores",
        "iniciales, la dosis y su compartimento."
      )
    ))
    logs <- attr(res$fits, "log_params")
    d$Parametro <- factor(
      log_label(d$Parametro, logs),
      levels = log_label(unique(res$fits$Parametro), logs)
    )
    d$Paciente <- factor(d$Paciente, levels = rev(unique(res$fits$Paciente)))
    ggplot2::ggplot(d, ggplot2::aes(y = .data$Paciente)) +
      ggplot2::geom_segment(ggplot2::aes(
        x = .data$IC95_inferior, xend = .data$IC95_superior,
        yend = .data$Paciente
      ), colour = "#187A5B") +
      ggplot2::geom_point(
        ggplot2::aes(x = .data$Estimacion),
        colour = "#B06F20", size = 2
      ) +
      ggplot2::facet_wrap(
        ~Parametro,
        nrow = if (narrow) NULL else 1,
        ncol = if (narrow) 1 else NULL,
        scales = "free_x"
      ) +
      ggplot2::scale_x_continuous(n.breaks = if (narrow) 4 else 5) +
      ggplot2::theme_minimal(base_size = if (narrow) 10 else 11) +
      ggplot2::labs(x = "Estimación individual con IC del 95 %", y = "Paciente")
  }, height = function() {
    res <- individual()
    if (!plot_is_narrow(session, id("individual_plot")) ||
        !isTRUE(res$ok)) {
      return(340)
    }
    n_params <- length(unique(res$fits$Parametro))
    n_patients <- length(unique(res$fits$Paciente))
    max(340, 50 + n_params * (50 + 14 * n_patients))
  })

  # Tabla de los ajustes individuales con errores estándar y p-valores.
  output[[id("individual_table")]] <- shiny::renderTable(
    {
      res <- individual()
      shiny::req(isTRUE(res$ok))
      tab <- res$fits
      tab$p <- ifelse(
        is.finite(tab$p),
        format.pval(tab$p, digits = 3, eps = 1e-4),
        NA
      )
      tab$Parametro <- log_label(tab$Parametro, attr(res$fits, "log_params"))
      names(tab) <- c(
        "Paciente", "Parámetro", "Estimación", "Error estándar",
        "IC 95 % inferior", "IC 95 % superior", "t", "p-valor"
      )
      format_numbers(utils::head(tab, max_rows))
    },
    striped = TRUE
  )

  # Usa la mediana de los ajustes individuales como valores iniciales.
  shiny::observeEvent(input[[id("use_individual_starts")]], {
    res <- individual()
    shiny::req(isTRUE(res$ok))
    starts <- population_individual_starts(res$fits)
    shiny::req(length(starts))
    suggested_starts(starts)
    shiny::showNotification(
      "Valores iniciales actualizados (en «Opciones avanzadas»).",
      type = "message"
    )
  })

  # ---- Modelo mixto ---------------------------------------------------

  # Resultado de pulsar "Estimar parámetros": tablas del ajuste o error.
  mixed_fit <- background_fit(
    "fit",
    function() {
      a <- fit_inputs()
      random <- input[[id("estimate_random")]]
      if (!length(random)) {
        stop("Selecciona al menos un parámetro que varíe entre pacientes.")
      }
      omega <- input[[id("omega")]]
      if (is.null(omega)) omega <- "diag"
      effects <- input[[id("cov_effects")]]
      if (is.null(effects)) effects <- character()
      cov_form <- input[[id("cov_form")]]
      if (is.null(cov_form)) cov_form <- "power"
      error_model <- input[[id("error_model")]]
      list(
        run = function() {
          population_fit_isolated(
            a$dat, a$st, a$response, a$group, a$time, a$dose_cmt,
            random, effects, error_model,
            cov_form, a$dose_column, a$observed, a$fixed, omega
          )
        },
        finish = function(result) {
          c(
            list(
              ok = TRUE,
              has_ci = any(is.finite(result$fixed$IC95_inferior)),
              log = attr(result, "log")
            ),
            result
          )
        }
      )
    },
    message = "Ajustando el modelo mixto...",
    detail = "Puede tardar unos minutos."
  )
  pop_fit <- mixed_fit$result

  # Indicador para los conditionalPanel: el ajuste ha terminado bien.
  output[[id("fit_ok")]] <- shiny::reactive(isTRUE(pop_fit()$ok))
  shiny::outputOptions(output, id("fit_ok"), suspendWhenHidden = FALSE)

  # Descarga de un texto (resumen o registro) como .txt.
  # Recibe: nombre del archivo y función que devuelve las líneas.
  text_download <- function(filename, lines) {
    shiny::downloadHandler(
      filename = function() filename,
      content = function(file) writeLines(lines(), file, useBytes = TRUE)
    )
  }

  # Resumen del ajuste como summary() de nlme.
  output[[id("fit_summary")]] <- shiny::renderText({
    res <- pop_fit()
    shiny::req(isTRUE(res$ok))
    paste(res$summary, collapse = "\n")
  })
  output[[id("fit_summary_download")]] <- text_download(
    "pklaboratory_summary_nlme.txt",
    function() {
      res <- pop_fit()
      shiny::req(isTRUE(res$ok))
      res$summary
    }
  )

  # Registro del proceso de estimación (iteraciones, avisos y error), para
  # ver por qué ha fallado.
  output[[id("fit_log")]] <- shiny::renderText({
    res <- pop_fit()
    shiny::req(length(res$log) > 0)
    paste(res$log, collapse = "\n")
  })
  output[[id("fit_log_download")]] <- text_download(
    "pklaboratory_registro_ajuste.txt",
    function() {
      res <- pop_fit()
      shiny::req(length(res$log) > 0)
      res$log
    }
  )

  # Aviso de un ajuste que ha fallado: la causa, qué se puede probar y el
  # mensaje original de R.
  # Recibe: título, resultado con el error y elementos adicionales.
  # Devuelve: recuadro de error.
  fit_error_ui <- function(title, res, ...) {
    shiny::div(
      class = "alert alert-danger pk-pop-alert pk-pop-error",
      shiny::strong(title),
      shiny::p(if (is.null(res$cause)) res$error else res$cause),
      if (length(res$hints)) {
        shiny::tagList(
          shiny::p("Qué puedes probar:"),
          shiny::tags$ul(lapply(res$hints, shiny::tags$li))
        )
      },
      if (!is.null(res$r_message)) {
        shiny::p(
          class = "pk-pop-error-r",
          "Mensaje de R: ", shiny::tags$code(res$r_message)
        )
      },
      ...
    )
  }

  # Pacientes y parámetros del ajuste, o el error.
  output[[id("fit_status")]] <- shiny::renderUI({
    if (mixed_fit$running()) {
      return(running_ui("Ajustando el modelo mixto…"))
    }
    res <- pop_fit()
    if (!isTRUE(res$ok)) {
      return(fit_error_ui(
        "No se ha podido ajustar el modelo.", res,
        if (length(res$log)) {
          shiny::p(
            "El registro del ajuste, más abajo, muestra en qué iteración se",
            "ha detenido y qué parámetros recibió mrgsolve."
          )
        }
      ))
    }
    shiny::tagList(
      shiny::div(
        class = "pk-pop-stats",
        stat(res$n_units, "pacientes"),
        stat(nrow(res$fixed), "parámetros estimados")
      ),
      if (!res$has_ci) {
        shiny::div(
          class = "alert alert-warning pk-pop-alert",
          paste(
            "No se han podido calcular los intervalos de confianza: nlme no",
            "obtuvo la matriz de covarianza de los efectos fijos. Prueba",
            "con otros valores iniciales o con menos efectos aleatorios o",
            "covariables."
          )
        )
      }
    )
  })

  # Texto con 4 cifras significativas; los valores no calculados se
  # muestran como "—".
  format_numbers <- function(tab) {
    for (j in which(vapply(tab, is.numeric, logical(1)))) {
      tab[[j]] <- vapply(tab[[j]], function(v) {
        if (is.na(v)) {
          "—"
        } else {
          format(signif(v, 4), scientific = FALSE, trim = TRUE)
        }
      }, character(1))
    }
    tab
  }

  # Tabla de efectos fijos.
  output[[id("fit_fixed")]] <- shiny::renderTable(
    {
      res <- pop_fit()
      shiny::req(isTRUE(res$ok))
      format_numbers(population_display_fixed(res$fixed))
    },
    striped = TRUE,
    align = "lrrrr"
  )

  # Tabla de variabilidad.
  output[[id("fit_variability")]] <- shiny::renderTable(
    {
      res <- pop_fit()
      shiny::req(isTRUE(res$ok))
      format_numbers(population_display_variability(res$variability))
    },
    striped = TRUE,
    align = "lrrrr"
  )

  # Tabla de estimaciones de cada paciente.
  output[[id("fit_individual")]] <- shiny::renderTable(
    {
      res <- pop_fit()
      shiny::req(isTRUE(res$ok), !is.null(res$individual))
      format_numbers(utils::head(res$individual, max_rows))
    },
    striped = TRUE
  )

  # Descarga de las tablas de resultados en un único CSV.
  output[[id("fit_download")]] <- shiny::downloadHandler(
    filename = function() "pklaboratory_estimaciones_nlme.csv",
    content = function(file) {
      res <- pop_fit()
      shiny::req(isTRUE(res$ok))
      utils::write.csv(
        population_results_export(res$fixed, res$variability),
        file,
        row.names = FALSE
      )
    }
  )
}
