# ============================================================
# Archivo: ui_home.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para construir la página de inicio de la
# aplicación: su presentación, los accesos a cada sección (simulación,
# creación de modelos, análisis poblacional y modelos guardados) y los
# pasos del análisis poblacional.
# ============================================================

# Ilustración de una curva concentración-tiempo con varias dosis.
# Devuelve: SVG decorativo.
hero_curve <- function() {
  curve <- paste(
    "M10 200 C40 200 50 50 80 50 C110 50 120 150 150 150",
    "C175 150 185 40 215 40 C245 40 255 140 285 140",
    "C310 140 320 35 345 35 C370 35 380 120 395 125"
  )

  shiny::HTML(sprintf(
    paste0(
      "<svg class=\"pk-hero-curve\" viewBox=\"0 0 400 220\" ",
      "role=\"img\" aria-label=\"Curva de concentración frente al tiempo\">",
      "<defs><linearGradient id=\"pk-grad\" x1=\"0\" y1=\"0\" ",
      "x2=\"0\" y2=\"1\">",
      "<stop offset=\"0\" stop-color=\"#4F5BD5\" stop-opacity=\".25\"/>",
      "<stop offset=\"1\" stop-color=\"#187A5B\" stop-opacity=\"0\"/>",
      "</linearGradient>",
      "<linearGradient id=\"pk-line\" x1=\"0\" y1=\"0\" x2=\"1\" ",
      "y2=\"0\">",
      "<stop offset=\"0\" stop-color=\"#187A5B\"/>",
      "<stop offset=\".5\" stop-color=\"#4F5BD5\"/>",
      "<stop offset=\"1\" stop-color=\"#8B5CF6\"/>",
      "</linearGradient></defs>",
      "<path d=\"%s L395 210 L10 210 Z\" fill=\"url(#pk-grad)\"/>",
      "<path d=\"%s\" fill=\"none\" stroke=\"url(#pk-line)\" ",
      "stroke-width=\"3.5\" stroke-linecap=\"round\"/>",
      "<line x1=\"10\" y1=\"210\" x2=\"395\" y2=\"210\" ",
      "stroke=\"#187A5B\" stroke-opacity=\".25\"/>",
      "</svg>"
    ),
    curve, curve
  ))
}

# Icono de línea para las tarjetas de acceso.
# Recibe: nombre del icono.
# Devuelve: SVG decorativo.
home_icon <- function(name) {
  paths <- switch(
    name,
    simulation = paste0(
      "<path d=\"M3 20 C6 20 6 6 9 6 S12 16 15 16 S18 9 21 9\"/>",
      "<path d=\"M3 3v18h18\"/>"
    ),
    builder = paste0(
      "<rect x=\"3\" y=\"3\" width=\"7\" height=\"7\" rx=\"1.5\"/>",
      "<rect x=\"14\" y=\"14\" width=\"7\" height=\"7\" rx=\"1.5\"/>",
      "<path d=\"M10 6.5h4.5a2 2 0 0 1 2 2V14\"/>"
    ),
    population = paste0(
      "<circle cx=\"7\" cy=\"8\" r=\"3\"/>",
      "<circle cx=\"17\" cy=\"8\" r=\"3\"/>",
      "<path d=\"M2 20c0-3 2.2-5 5-5s5 2 5 5\"/>",
      "<path d=\"M12 20c0-3 2.2-5 5-5s5 2 5 5\"/>"
    ),
    models = paste0(
      "<path d=\"M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5",
      "a2 2 0 0 1-2-2z\"/>",
      "<path d=\"M8 13h8\"/>"
    )
  )
  shiny::HTML(paste0(
    "<svg class=\"pk-home-icon\" viewBox=\"0 0 24 24\" fill=\"none\" ",
    "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" ",
    "stroke-linejoin=\"round\" aria-hidden=\"true\">",
    paths, "</svg>"
  ))
}

# Tarjeta de acceso a una sección: toda la tarjeta es un enlace.
# Recibe: identificador del enlace, icono, título, texto y color de acento.
# Devuelve: enlace de Shiny con forma de tarjeta.
home_tool <- function(input_id, icon, title, text, accent) {
  shiny::actionLink(
    input_id,
    class = paste("pk-home-tool", paste0("pk-accent-", accent)),
    label = shiny::tagList(
      shiny::span(class = "pk-home-tool-icon", home_icon(icon)),
      shiny::span(class = "pk-home-tool-title", title),
      shiny::span(class = "pk-home-tool-text", text),
      shiny::span(class = "pk-home-tool-go", "Abrir →")
    )
  )
}

# Construye la página de inicio de la aplicación.
# Devuelve: conjunto de elementos de la interfaz de Shiny.
home_ui <- function() {
  shiny::div(
    class = "pk-home",

    shiny::tags$section(
      class = "pk-home-hero",
      `aria-labelledby` = "pk-home-title",

      shiny::div(
        class = "pk-hero-text",
        shiny::span(
          class = "pk-home-eyebrow",
          "Farmacocinética · simulación y análisis poblacional"
        ),
        shiny::h1(id = "pk-home-title", "PKLaboratory"),
        shiny::p(
          class = "pk-home-lead",
          "Simula cómo evoluciona un fármaco en el organismo con los modelos",
          "de mrgsolve o con los que tú crees, y estima sus parámetros en una",
          "población de pacientes con modelos de efectos mixtos no lineales."
        )
      ),

      shiny::div(class = "pk-hero-art", hero_curve())
    ),

    shiny::tags$section(
      class = "pk-home-section",
      `aria-labelledby` = "pk-home-tools-title",
      shiny::div(
        class = "pk-home-section-head pk-home-section-head-center",
        shiny::h2(id = "pk-home-tools-title", "¿Qué puedes hacer?")
      ),
      shiny::div(
        class = "pk-home-tools",
        home_tool(
          "home_open_simulation", "simulation", "Simulación",
          paste(
            "Elige un modelo de la biblioteca o uno tuyo, ajusta sus",
            "parámetros y la pauta de dosis, y descarga las curvas en CSV",
            "o PNG."
          ),
          "indigo"
        ),
        home_tool(
          "home_open_builder", "builder", "Crear modelo",
          paste(
            "Define la administración, la distribución y la eliminación, o",
            "escribe tus propias ecuaciones. El código de mrgsolve se",
            "genera solo."
          ),
          "amber"
        ),
        home_tool(
          "home_open_population", "population", "Análisis poblacional",
          paste(
            "Ajusta un modelo mixto no lineal a datos de varios pacientes,",
            "como Theoph, con cualquier modelo de la aplicación."
          ),
          "green"
        ),
        home_tool(
          "home_open_models", "models", "Mis modelos",
          paste(
            "Consulta, edita y comparte los modelos que has creado. Los",
            "compartidos aparecen en el catálogo de todos los usuarios."
          ),
          "violet"
        )
      )
    )
  )
}
