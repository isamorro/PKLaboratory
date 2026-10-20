# ============================================================
# Archivo: ui_models.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Página de gestión de modelos propios y del catálogo compartido.
# Conserva los identificadores utilizados por los controladores.
# ============================================================

# Interfaz de usuario de la página de modelos.
# Devuelve: div con el contenido de la página.
models_ui <- function() {
  shiny::div(
    class = "pk-sim",
    shiny::h2("Mis modelos"),
    shiny::uiOutput("bld_my_models")
  )
}
