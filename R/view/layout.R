# ============================================================
# Archivo: layout.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Piezas comunes de las páginas de trabajo (Simulación, Crear modelo,
# Análisis poblacional y Mis modelos), para que todas compartan la misma
# disposición:
#   - una barra fija arriba con el título de la página y sus controles;
#   - un espacio de trabajo con un panel, que se desplaza por dentro y
#     deja siempre a la vista sus botones, y el contenido al lado;
#   - tarjetas con el mismo estilo.
# Los estilos están en www/styles.css (.pk-page, .pk-pagebar, .pk-pop-*).
# ============================================================

# Página de trabajo.
# Recibe: contenido y clases adicionales.
# Devuelve: contenedor de la página.
page_container <- function(..., class = NULL) {
  shiny::div(class = paste("pk-page container-fluid", class), ...)
}

# Barra fija con el título de la página y sus controles.
# Recibe: título, controles de la izquierda (junto al título), controles
# de la derecha y subtítulo opcional.
# Devuelve: barra de la página.
page_bar <- function(title, ..., right = NULL, subtitle = NULL) {
  shiny::div(
    class = "pk-pagebar",
    shiny::div(
      class = "pk-pagebar-controls",
      shiny::div(
        class = "pk-pagebar-title",
        shiny::h1(title),
        if (!is.null(subtitle)) shiny::p(subtitle)
      ),
      ...
    ),
    if (!is.null(right)) shiny::div(class = "pk-pagebar-right", right)
  )
}

# Panel de opciones con los botones anclados abajo.
# Recibe: contenido, botones del pie y clases adicionales.
# Devuelve: panel lateral.
page_panel <- function(..., footer = NULL, class = NULL) {
  shiny::tags$aside(
    class = paste("pk-pop-panel", class),
    shiny::div(class = "pk-pop-panel-body", ...),
    if (!is.null(footer)) shiny::div(class = "pk-pop-panel-footer", footer)
  )
}

# Espacio de trabajo: panel y contenido, uno al lado del otro.
# Recibe: panel, contenido y si el panel va a la derecha.
# Devuelve: rejilla de dos columnas.
page_workspace <- function(panel, ..., panel_right = FALSE) {
  main <- shiny::div(class = "pk-pop-main", ...)
  if (panel_right) {
    shiny::div(class = "pk-pop-workspace pk-pop-workspace-end", main, panel)
  } else {
    shiny::div(class = "pk-pop-workspace", panel, main)
  }
}

# Tarjeta con título y, opcionalmente, botones en la cabecera.
# Recibe: título, contenido y elementos de la cabecera.
# Devuelve: tarjeta de bslib.
page_card <- function(title, ..., header_extra = NULL) {
  bslib::card(
    class = "pk-pop-card",
    fill = FALSE,
    bslib::card_header(
      class = "pk-pop-card-header",
      shiny::span(title),
      header_extra
    ),
    bslib::card_body(...)
  )
}

# Sección de un panel o formulario con su título.
# Recibe: título y contenido.
# Devuelve: sección.
page_section <- function(title, ...) {
  shiny::div(class = "pk-form-section", shiny::h4(title), ...)
}

# Mensaje para cuando todavía no hay nada que mostrar.
# Recibe: título, texto y elementos adicionales (por ejemplo, un botón).
# Devuelve: recuadro con el mensaje.
page_empty <- function(title, text, ...) {
  shiny::div(
    class = "pk-empty",
    shiny::div(class = "pk-empty-icon", "PK"),
    shiny::h3(title),
    shiny::p(text),
    ...
  )
}

# Ventana con el código mrgsolve de un modelo, solo para consultarlo.
# Recibe: nombre, descripción y código del modelo (NULL si no se tiene).
# Devuelve: diálogo modal de Shiny.
model_code_modal <- function(name, description, code) {
  modal <- shiny::modalDialog(
    title = name,
    size = "l",
    easyClose = TRUE,
    if (nzchar(description)) {
      shiny::p(class = "pk-code-description", description)
    },
    if (is.null(code)) {
      shiny::p("No se ha podido obtener el código del modelo.")
    } else {
      shiny::tags$pre(class = "pk-code-view", code)
    },
    footer = shiny::modalButton("Cerrar")
  )
  htmltools::tagAppendAttributes(modal, class = "pk-editor-modal")
}

# Enlace externo que se abre en una pestaña nueva. Sin espacio detrás,
# porque suele seguirle un signo de puntuación.
# Recibe: dirección y texto del enlace.
# Devuelve: etiqueta <a>.
external_link <- function(href, text) {
  shiny::tags$a(
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    text,
    .noWS = "after"
  )
}
