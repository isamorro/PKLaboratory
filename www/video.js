// ============================================================
// Archivo: video.js
// Autor: Isabel Morro Tabares
// Proyecto: TFG – PKLaboratory
// Aplicación Shiny para simulación farmacocinética con mrgsolve
// URL: https://pklaboratory.shinyapps.io/pklaboratory/
//
// Descripción:
// Carga los vídeos de la guía (video_player() en layout.R) al pulsar su
// botón. Chrome y Firefox los reproducen mientras se descargan. Safari
// necesita pedir el archivo por partes, y el servidor de Shiny no lo
// admite, así que en Safari se descarga entero (mostrando el progreso) y se
// reproduce desde memoria.
// ============================================================

(function () {
  // Safari, también en iPhone y iPad (Chrome y Firefox en iOS incluidos,
  // porque usan el motor de Safari).
  const safari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent) ||
    /iP(hone|ad|od)/.test(navigator.userAgent);

  // Muestra el vídeo en lugar del botón y lo reproduce.
  function show(button, video, src) {
    video.src = src;
    button.remove();
    video.hidden = false;
    video.play().catch(function () {});
  }

  // Descarga el vídeo entero, indicando el progreso en el botón.
  function download(button, src) {
    return fetch(src).then(function (res) {
      if (!res.ok) throw new Error(res.status);
      const total = Number(res.headers.get('Content-Length'));
      if (!res.body || !total) return res.blob();
      const reader = res.body.getReader();
      const parts = [];
      let received = 0;
      function read() {
        return reader.read().then(function (step) {
          if (step.done) return new Blob(parts, { type: 'video/mp4' });
          parts.push(step.value);
          received += step.value.length;
          button.textContent = 'Cargando el vídeo… ' +
            Math.round(100 * received / total) + ' %';
          return read();
        });
      }
      return read();
    });
  }

  // Carga el vídeo del botón pulsado.
  function load(button) {
    const video = button.parentElement.querySelector('video[data-src]');
    if (!video) return;
    if (!safari) {
      show(button, video, video.dataset.src);
      return;
    }
    button.disabled = true;
    button.textContent = 'Cargando el vídeo…';
    download(button, video.dataset.src)
      .then(function (blob) {
        show(button, video, URL.createObjectURL(blob));
      })
      .catch(function () {
        button.disabled = false;
        button.textContent = 'No se ha podido cargar el vídeo. Reintentar';
      });
  }

  document.addEventListener('click', function (e) {
    const button = e.target.closest('.pk-video-play');
    if (button) load(button);
  });
})();
