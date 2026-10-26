# ============================================================
# Archivo: ui_builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la interfaz para crear modelos escribiendo su código de mrgsolve,
# con todos sus bloques, y el asistente por componentes (formulario para
# definir parámetros, compartimentos y ecuaciones) que genera ese código.
# ============================================================

# Genera un ejemplo de modelo por componentes para inicializar el asistente.
# Devuelve: lista con parámetros, compartimentos, auxiliares, ecuaciones y
# salidas por defecto.
component_example <- function() {
  list(
    params = list(KA = 1, CL = 5, V = 50),
    initials = list(DEPOT = 0, CENT = 0),
    auxiliary = list(CP = "CENT / V"),
    equations = list(
      DEPOT = "-KA * DEPOT",
      CENT = "KA * DEPOT - CL / V * CENT"
    ),
    outputs = "CP"
  )
}

# Genera el ejemplo con el que empieza el editor de código: el mismo modelo
# que el ejemplo por componentes, escrito con los bloques de mrgsolve.
# Devuelve: lista con nombre, descripción y código.
code_example <- function() {
  list(
    name = "",
    description = "",
    code = paste(
      c(
        "$PROB Un compartimento con absorción de primer orden",
        "",
        "$PARAM @annotated",
        "CL : 5  : Aclaramiento (L/h)",
        "V  : 50 : Volumen de distribución (L)",
        "KA : 1  : Constante de absorción (1/h)",
        "",
        "$CMT @annotated",
        "DEPOT : Compartimento de dosificación",
        "CENT  : Compartimento central",
        "",
        "$ODE",
        "dxdt_DEPOT = -KA * DEPOT;",
        "dxdt_CENT  =  KA * DEPOT - CL / V * CENT;",
        "",
        "$TABLE",
        "capture CP = CENT / V;"
      ),
      collapse = "\n"
    )
  )
}

# Construye una tarjeta del formulario con un título y una indicación breve.
# Recibe: número de la tarjeta, título, pista, contenido y color de acento.
# Devuelve: objeto bslib::card() listo para insertarse en la interfaz.
component_card <- function(number, title, hint, ..., accent = "green") {
  bslib::card(
    class = paste("pk-component-card", paste0("pk-component-", accent)),
    fill = FALSE,
    bslib::card_header(
      shiny::span(class = "pk-component-number", number),
      shiny::h3(title)
    ),
    bslib::card_body(
      shiny::p(class = "pk-component-hint", hint),
      ...
    )
  )
}

# Nombre y descripción del modelo, comunes al constructor y al editor.
# Se escriben directamente, sin cajas (las etiquetas quedan para los
# lectores de pantalla).
# Recibe: prefijo para formar ids de entrada y especificación inicial.
# Devuelve: tarjeta con los dos campos.
model_identity <- function(prefix, spec) {
  id <- function(x) paste0(prefix, x)
  bslib::card(
    class = "pk-component-identity",
    fill = FALSE,
    bslib::card_body(
      shiny::textInput(
        id("name"), "Nombre del modelo",
        value = spec$name,
        placeholder = "Nombre del modelo, p. ej. AbsorcionTresCompartimentos",
        width = "100%"
      ),
      shiny::textAreaInput(
        id("description"), "Descripción (opcional)",
        value = spec$description,
        placeholder = "Añade una descripción opcional",
        rows = 1, width = "100%", resize = "none"
      )
    )
  )
}

# Formulario del asistente por componentes, que empieza con el ejemplo.
# Recibe: prefijo para formar ids de entrada.
# Devuelve: div de Shiny con el formulario completo del modelo por componentes.
component_form <- function(prefix) {
  id <- function(x) paste0(prefix, x)
  spec <- component_example()
  pairs <- function(x) {
    paste(vapply(names(x), function(n) paste(n, "=", x[[n]]), character(1)),
      collapse = "\n"
    )
  }

  shiny::div(
    class = "pk-component-form",
    shiny::div(
      class = "pk-component-grid",
      component_card(
        "01",
        "Parámetros",
        paste(
          "Las constantes de tu modelo,",
          "con sus valores iniciales."
        ),
        shiny::textAreaInput(
          id("params"), "Nombre = valor",
          value = pairs(spec$params),
          rows = 6, width = "100%", resize = "vertical"
        ),
        shiny::p(
          class = "pk-component-note",
          "Una línea por parámetro. Este campo puede quedar vacío."
        ),
        accent = "green"
      ),
      component_card(
        "02",
        "Compartimentos",
        "Las cantidades que evolucionarán durante la simulación.",
        shiny::textAreaInput(
          id("initials"), "Nombre = cantidad inicial",
          value = pairs(spec$initials),
          rows = 6, width = "100%", resize = "vertical"
        ),
        shiny::p(
          class = "pk-component-note",
          "Una línea por compartimento. Usa 0 si inicialmente está vacío."
        ),
        accent = "blue"
      ),
      component_card(
        "03",
        "Variables auxiliares",
        paste(
          "Expresiones que simplifican tus ecuaciones",
          "o calculan otras variables."
        ),
        shiny::textAreaInput(
          id("auxiliary"), "Nombre = expresión",
          value = pairs(spec$auxiliary),
          rows = 6, width = "100%", resize = "vertical"
        ),
        shiny::p(
          class = "pk-component-note",
          "Opcional. Declara primero las variables que utilizan las siguientes."
        ),
        accent = "violet"
      ),
      component_card(
        "04",
        "Ecuaciones diferenciales",
        "Cómo cambia la cantidad de cada compartimento con el tiempo.",
        shiny::textAreaInput(
          id("equations"), "Compartimento = expresión de su derivada",
          value = pairs(spec$equations),
          rows = 6, width = "100%", resize = "vertical"
        ),
        shiny::p(
          class = "pk-component-note",
          "Una ecuación por compartimento."
        ),
        accent = "coral"
      )
    ),
    component_card(
      "05",
      "Variables de salida",
      "Elige los parámetros o variables definidos que quieres consultar.",
      shiny::textInput(
        id("outputs"), "Nombres separados por comas",
        value = paste(spec$outputs, collapse = ", "),
        placeholder = "CP, TOTAL",
        width = "100%"
      ),
      shiny::p(
        class = "pk-component-note",
        "Los compartimentos también aparecen en los resultados."
      ),
      accent = "green"
    )
  )
}

# Enlace a la guía de mrgsolve que explica los bloques del modelo.
# Recibe: texto del enlace.
# Devuelve: etiqueta <a> que se abre en una pestaña nueva.
mrgsolve_spec_link <- function(text = "guía de especificación de mrgsolve") {
  external_link(mrgsolve_spec_url, text)
}

# Ventana del asistente por componentes: el formulario por componentes,
# cuyo código generado pasa al editor.
# Devuelve: diálogo modal de Shiny.
component_wizard_modal <- function() {
  modal <- shiny::modalDialog(
    title = "Asistente por componentes",
    size = "l",
    easyClose = FALSE,
    shiny::p(
      class = "pk-component-hint",
      "Define el modelo sin escribir código: al terminar, el código de",
      "mrgsolve generado sustituye al del editor, donde puedes completarlo",
      "con otros bloques ($OMEGA, $MAIN, $EVENT...)."
    ),
    shiny::uiOutput("wiz_errors"),
    component_form("wiz_"),
    footer = shiny::tagList(
      shiny::modalButton("Cancelar"),
      shiny::actionButton(
        "wiz_generate",
        "Generar código",
        class = "btn-primary"
      )
    )
  )
  htmltools::tagAppendAttributes(modal, class = "pk-editor-modal")
}

# Formulario del modelo: nombre, descripción y editor del código de mrgsolve
# con todos los bloques y la opción de cargar un archivo .cpp o de empezar
# con el asistente.
# Recibe: prefijo para formar ids de entrada, especificación inicial y si
# muestra las herramientas para cargar un archivo y abrir el asistente.
# Devuelve: div de Shiny con el formulario.
code_form <- function(prefix, spec = code_example(), tools = TRUE) {
  id <- function(x) paste0(prefix, x)

  shiny::div(
    class = "pk-component-form",
    model_identity(prefix, spec),
    component_card(
      "</>",
      "Código mrgsolve",
      shiny::tagList(
        "Escribe el modelo con los bloques de mrgsolve ($PARAM, $CMT,",
        "$MAIN, $ODE, $TABLE, $OMEGA...). Consulta qué hace cada bloque",
        "en la", mrgsolve_spec_link(), "."
      ),
      if (tools) {
        shiny::div(
          class = "pk-code-tools",
          shiny::fileInput(
            id("code_file"), NULL,
            accept = c(".cpp", ".mod", ".txt"),
            buttonLabel = "Cargar .cpp…",
            placeholder = "o escribe el código abajo",
            width = "100%"
          ),
          shiny::actionButton(
            id("wizard"),
            "Asistente por componentes",
            class = "btn-outline-secondary"
          )
        )
      },
      htmltools::tagAppendAttributes(
        shiny::textAreaInput(
          id("code_text"), "Código del modelo",
          value = spec$code,
          rows = 22, width = "100%", resize = "vertical"
        ),
        class = "pk-code-editor"
      ),
      accent = "violet"
    )
  )
}

# Página de creación, con la disposición común de las páginas de trabajo
# (layout.R): el formulario con el código a la izquierda y, a la derecha, un
# panel con la comprobación del código y los botones para descargarlo y
# guardar el modelo.
# Devuelve: UI de la vista del constructor del modelo.
builder_ui <- function() {
  page_container(
    class = "pk-builder-studio",
    page_bar(
      "Crear modelo",
      subtitle = shiny::tagList(
        "Escribe tu modelo con código de mrgsolve o créalo con el",
        "asistente por componentes. Consulta la",
        mrgsolve_spec_link("documentación de los bloques"), "."
      )
    ),
    page_workspace(
      panel_right = TRUE,
      page_panel(
        class = "pk-builder-panel",
        shiny::div(
          class = "pk-builder-panel-head",
          shiny::h4("Comprobación del código"),
          shiny::span(class = "pk-builder-filetype", ".cpp")
        ),
        shiny::p(
          class = "pk-component-note",
          "Se actualiza al modificar el código."
        ),
        shiny::verbatimTextOutput("bld_code"),
        footer = shiny::tagList(
          shiny::div(
            class = "pk-pop-panel-actions",
            shiny::downloadButton(
              "bld_download",
              "Descargar .cpp",
              class = "btn-outline-primary"
            ),
            shiny::actionButton(
              "bld_add",
              "Guardar modelo",
              class = "btn-primary"
            )
          ),
          shiny::p(
            class = "pk-pop-panel-hint",
            paste(
              "Al guardar se valida, se compila y se prueba;",
              "queda en Mis modelos como privado."
            )
          )
        )
      ),
      shiny::uiOutput("bld_errors"),
      code_form("bld_")
    )
  )
}
