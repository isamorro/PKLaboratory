# ============================================================
# Archivo: ui.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la interfaz de usuario de PKLaboratory: la animación de entrada y
# el contenedor donde se muestra el inicio de sesión o la aplicación
# principal (main_app_ui(), en R/view/ui_main.R). El contenido de cada
# página está en R/view/.
# ============================================================

# Interfaz de usuario de la aplicación, con login.
# Devuelve: página con la animación de entrada y el contenido de app_page.
ui <- shiny::fluidPage(
  theme = pk_theme,
  title = "PKLaboratory",
  lang = "es",
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "styles.css"),
    shiny::tags$script(src = "intro.js", defer = NA),
    shiny::tags$script(src = "session.js", defer = NA)
  ),
  # Fuera de app_page para no repetir la animación al entrar o salir.
  shiny::div(
    id = "pk-intro",
    class = "pk-intro",
    shiny::div(class = "pk-intro-panel pk-intro-top", `aria-hidden` = "true"),
    shiny::div(
      class = "pk-intro-panel pk-intro-bottom",
      `aria-hidden` = "true"
    ),
    shiny::div(
      class = "pk-intro-word",
      `aria-label` = "PKLaboratory",
      lapply(seq_along(strsplit("PKLaboratory", "")[[1]]), function(i) {
        shiny::span(
          class = "pk-intro-letter",
          `aria-hidden` = "true",
          style = paste0("--pk-letter-delay:", (i - 1) * 65, "ms"),
          strsplit("PKLaboratory", "")[[1]][i]
        )
      })
    )
  ),
  shiny::uiOutput("app_page")
)
