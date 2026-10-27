// ============================================================
// Archivo: session.js
// Autor: Isabel Morro Tabares
// Proyecto: TFG – PKLaboratory
// Aplicación Shiny para simulación farmacocinética con mrgsolve
// URL: https://pklaboratory.shinyapps.io/pklaboratory/
//
// Descripción:
// Guarda el token de sesión en la pestaña (sessionStorage) para que recargar
// la página no cierre la sesión. Al cerrar la pestaña el token se pierde y
// hay que volver a iniciar sesión.
// ============================================================

(function () {
  const KEY = 'pk-session-token';

  // Lee el token guardado; vacío si no hay o el navegador no lo permite.
  function readToken() {
    try {
      return sessionStorage.getItem(KEY) || '';
    } catch (e) {
      return '';
    }
  }

  // Guarda el token, o lo borra si es null.
  function writeToken(token) {
    try {
      if (token) sessionStorage.setItem(KEY, token);
      else sessionStorage.removeItem(KEY);
    } catch (e) {
      // Sin almacenamiento, al recargar habrá que iniciar sesión.
    }
  }

  Shiny.addCustomMessageHandler('pk_session_token', function (msg) {
    writeToken(msg.token);
  });

  // Al conectar, envía el token (o vacío) para recuperar la sesión.
  $(document).on('shiny:connected', function () {
    Shiny.setInputValue('resume_token', readToken(), { priority: 'event' });
  });
})();
