# ============================================================
# Archivo: ui_models.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la función para construir la página de gestión de modelos
# personales y compartidos, con la barra común de las páginas de trabajo
# (layout.R). Mantiene bld_my_models, utilizado por el controlador. Los
# estilos se definen en www/styles.css.
# ============================================================

# Construye la página de modelos guardados.
# Devuelve: barra de la página y lista de los modelos del usuario.
models_ui <- function() {
  page_container(
    class = "pk-builder-studio pk-models-page",
    page_bar(
      "Mis modelos",
      subtitle = paste(
        "Edita, elimina o comparte tus modelos.",
        "Los compartidos aparecen en el catálogo común."
      ),
      right = shiny::actionButton(
        "models_new", "+ Crear modelo nuevo",
        class = "btn-primary"
      )
    ),
    shiny::div(
      class = "pk-models-content",
      shiny::uiOutput("bld_my_models")
    )
  )
}
