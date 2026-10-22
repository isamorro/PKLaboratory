# ============================================================
# Archivo: ui_login.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Página de acceso con usuario y contraseña para proteger la
# aplicación. No se permite registro desde la interfaz: los usuarios
# se crean desde la base de datos.
# ============================================================

# Interfaz de usuario de la página de login.
# Devuelve: div con el formulario de login.
login_ui <- function() {
  shiny::div(
    class = "pk-login-shell",
    shiny::div(
      class = "pk-login-card",
      shiny::div(
        class = "pk-login-brand",
        shiny::span(class = "pk-brand-mark", "PK"),
        shiny::h1("PKLaboratory")
      ),
      shiny::h2("Iniciar sesión"),
      shiny::textInput(
        "username", "Usuario", placeholder = "Tu usuario"
      ),
      shiny::passwordInput(
        "password", "Contraseña", placeholder = "Tu contraseña"
      ),
      shiny::actionButton(
        "login_submit", "Entrar", class = "btn-primary w-100 pk-login-submit"
      )
    ),
    # Enter en el usuario o la contraseña equivale a pulsar «Entrar». Antes
    # se envían los valores escritos, por si Shiny aún no los ha recibido.
    shiny::tags$script(shiny::HTML(
      "$(document).on('keydown', '#username, #password', function(e) {
         if (e.key !== 'Enter') return;
         e.preventDefault();
         Shiny.setInputValue('username', $('#username').val());
         Shiny.setInputValue('password', $('#password').val());
         $('#login_submit').click();
       });"
    ))
  )
}
