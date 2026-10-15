# ============================================================
# Archivo: ui_home.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene las funciones para construir la página de inicio de la
# aplicación, con su presentación y el acceso a la simulación.
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
      '<svg class="pk-hero-curve" viewBox="0 0 400 220" role="img" ',
      'aria-label="Curva de concentración frente al tiempo">',
      '<defs><linearGradient id="pk-grad" x1="0" y1="0" x2="0" y2="1">',
      '<stop offset="0" stop-color="#4F5BD5" stop-opacity=".25"/>',
      '<stop offset="1" stop-color="#187A5B" stop-opacity="0"/>',
      '</linearGradient>',
      '<linearGradient id="pk-line" x1="0" y1="0" x2="1" y2="0">',
      '<stop offset="0" stop-color="#187A5B"/>',
      '<stop offset=".5" stop-color="#4F5BD5"/>',
      '<stop offset="1" stop-color="#8B5CF6"/>',
      '</linearGradient></defs>',
      '<path d="%s L395 210 L10 210 Z" fill="url(#pk-grad)"/>',
      '<path d="%s" fill="none" stroke="url(#pk-line)" stroke-width="3.5" ',
      'stroke-linecap="round"/>',
      '<line x1="10" y1="210" x2="395" y2="210" stroke="#187A5B" ',
      'stroke-opacity=".25"/>',
      '</svg>'
    ),
    curve, curve
  ))
}

# Construye una tarjeta con un número, un título y una descripción.
# Recibe: número del paso, título, texto y color de acento de la tarjeta.
# Devuelve: tarjeta de bslib.
home_card <- function(number, title, text, accent = "green") {
  bslib::card(
    class = paste("pk-feature", paste0("pk-accent-", accent)),
    fill = FALSE,
    bslib::card_body(
      shiny::span(class = "pk-home-number", number),
      shiny::h3(title),
      shiny::p(text)
    )
  )
}

# Construye una pieza de la franja de datos de la portada.
# Recibe: valor destacado y etiqueta.
# Devuelve: elemento de la interfaz de Shiny.
home_stat <- function(value, label) {
  shiny::div(
    class = "pk-stat",
    shiny::span(class = "pk-stat-value", value),
    shiny::span(class = "pk-stat-label", label)
  )
}

# Construye la página de inicio de la aplicación.
# Devuelve: conjunto de elementos de la interfaz de Shiny.
home_ui <- function() {
  shiny::tagList(

    shiny::div(
      class = "pk-home",

      shiny::tags$section(
        class = "pk-home-hero",
        `aria-labelledby` = "pk-home-title",

        shiny::div(
          class = "pk-hero-text",

          shiny::span(
            class = "pk-home-eyebrow",
            "Simulación farmacocinética"
          ),

          shiny::h1(id = "pk-home-title", "PKLaboratory"),

          shiny::p(
            class = "pk-home-lead",
            "Explora cómo evoluciona un fármaco en el organismo.",
            "Elige un modelo, configura la administración y",
            "visualiza los resultados de la simulación."
          ),

          shiny::actionButton(
            "go_to_simulation",
            "Empezar a simular",
            class = "btn btn-primary btn-lg pk-cta"
          )
        ),

        shiny::div(class = "pk-hero-art", hero_curve())
      ),

      shiny::div(
        class = "pk-stats",
        home_stat(length(models_ids), "modelos disponibles"),
        home_stat("3", "pasos para simular"),
        home_stat("Online", "desde cualquier navegador")
      ),

      shiny::tags$section(
        class = "pk-home-section",
        `aria-labelledby` = "pk-home-features",

        shiny::h2(
          id = "pk-home-features",
          "Del modelo a los resultados"
        ),

        shiny::p(
          class = "pk-home-intro",
          "Configura tu simulación en tres pasos."
        ),

        bslib::layout_column_wrap(
          width = 1 / 3,
          fill = FALSE,

          home_card(
            "01",
            "Elige un modelo",
            paste(
              "Consulta los modelos disponibles en mrgsolve",
              "y ajusta sus parámetros."
            ),
            "indigo"
          ),

          home_card(
            "02",
            "Configura la administración",
            paste(
              "Define la dosis, el compartimento de administración,",
              "el intervalo y el número de dosis."
            ),
            "amber"
          ),

          home_card(
            "03",
            "Explora los resultados",
            paste(
              "Visualiza la evolución temporal de las variables",
              "y consulta los valores de la simulación en una tabla."
            ),
            "violet"
          )
        )
      ),

      shiny::tags$section(
        class = "pk-home-section pk-home-guide",
        `aria-labelledby` = "pk-home-guide-title",

        shiny::h2(
          id = "pk-home-guide-title",
          "Tu primera simulación"
        ),

        shiny::tags$ol(
          class = "pk-home-steps",
          shiny::tags$li(
            "Abre la pestaña Simulación y selecciona un modelo."
          ),
          shiny::tags$li(
            "Revisa los parámetros y modifica los valores que necesites."
          ),
          shiny::tags$li(
            "Configura la administración y el tiempo final."
          ),
          shiny::tags$li(
            "Pulsa Simular y selecciona las variables que quieres explorar."
          )
        ),

        shiny::p(
          class = "pk-home-note",
          "Si algún dato no es válido, la aplicación te indicará",
          "qué debes corregir."
        )
      )
    )
  )
}
