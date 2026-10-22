# ============================================================
# Archivo: ui_builder.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la interfaz para crear modelos por componentes, con el
# formulario para definir parámetros, compartimentos y ecuaciones,
# así como la vista previa del código generado.
# ============================================================

# Genera un ejemplo de modelo por componentes para inicializar el constructor.
# Devuelve: lista con nombre, descripción, parámetros, compartimentos,
# auxiliares, ecuaciones y salidas por defecto.
component_example <- function() {
  list(
    name = "",
    description = "",
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

# Mantiene los identificadores del constructor y del editor.
# Recibe: prefijo para formar ids de entrada y especificación inicial.
# Devuelve: div de Shiny con el formulario completo del modelo por componentes.
component_form <- function(prefix, spec = component_example()) {
  id <- function(x) paste0(prefix, x)
  pairs <- function(x) {
    paste(vapply(names(x), function(n) paste(n, "=", x[[n]]), character(1)),
      collapse = "\n"
    )
  }

  shiny::div(
    class = "pk-component-form",
    # Nombre y descripción como título del modelo: se escriben directamente,
    # sin cajas (las etiquetas quedan para los lectores de pantalla).
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
          placeholder = paste(
            "Añade una descripción: qué representa y cuándo utilizarlo",
            "(opcional)"
          ),
          rows = 1, width = "100%", resize = "none"
        )
      )
    ),
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
          "Una ecuación por compartimento. Sin dxdt_ ni punto y coma."
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

# Página de creación, con la disposición común de las páginas de trabajo
# (layout.R): el formulario a la izquierda y, a la derecha, un panel con el
# código generado y los botones para descargarlo y guardar el modelo.
# Devuelve: UI de la vista del constructor del modelo.
builder_ui <- function() {
  page_container(
    class = "pk-builder-studio",
    page_bar(
      "Crear modelo",
      subtitle = paste(
        "Define tu modelo por componentes;",
        "el código de mrgsolve se genera solo."
      )
    ),
    page_workspace(
      panel_right = TRUE,
      page_panel(
        class = "pk-builder-panel",
        shiny::div(
          class = "pk-builder-panel-head",
          shiny::h4("Código generado"),
          shiny::span(class = "pk-builder-filetype", ".cpp")
        ),
        shiny::p(
          class = "pk-component-note",
          "Se actualiza al modificar el formulario."
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
      component_form("bld_")
    )
  )
}
