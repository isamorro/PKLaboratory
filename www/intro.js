// ============================================================
// Archivo: intro.js
// Autor: Isabel Morro Tabares
// Proyecto: TFG – PKLaboratory
// Aplicación Shiny para simulación farmacocinética con mrgsolve
// URL: https://pklaboratory.shinyapps.io/pklaboratory/
//
// Descripción:
// Gestiona la portada animada de la aplicación. La intro se ejecuta al cargar
// la página y oculta el contenido principal mientras dura la animación, salvo
// que el navegador esté en modo de movimiento reducido.
// ============================================================

// Inicia la animación de la portada al cargar la página.
(function () {
  function startIntro() {
    const intro = document.getElementById('pk-intro');
    const page = document.getElementById('app_page');
    if (!intro || !page) return;

    let timer;

    function finish() {
      clearTimeout(timer);
      page.removeAttribute('inert');
      intro.remove();
    }

    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      finish();
      return;
    }

    page.setAttribute('inert', '');
    timer = setTimeout(finish, 2900);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', startIntro, { once: true });
  } else {
    startIntro();
  }
})();
