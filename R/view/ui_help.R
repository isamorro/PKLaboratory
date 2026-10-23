# ============================================================
# Archivo: ui_help.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Define la interfaz de la guía de uso de PKLaboratory, organizada como
# un documento con índice y secciones numeradas: Simulación, Crear modelo,
# Análisis poblacional y Mis modelos. Incluye ejemplos prácticos de
# simulación con pk1, creación de un modelo PK/PD y análisis de los datos
# Theoph. Los nombres de las columnas y las unidades se obtienen de
# R/model/population.R para mantener la guía sincronizada con la aplicación.
# ============================================================

# Construye la guía de uso.
# Devuelve: documento con la guía.
help_ui <- function() {

  # Código en línea. Al generar el HTML, Shiny separa los elementos con
  # saltos de línea, que el navegador muestra como espacios; por eso se
  # quitan detrás del código (le siguen comas, puntos o paréntesis) y, con
  # tight = TRUE, también delante (cuando va justo después de un paréntesis).
  code <- function(x, tight = FALSE) {
    shiny::tags$code(x, .noWS = if (tight) "outside" else "after")
  }

  # Lista de nombres separados por comas, en código.
  names_list <- function(x, tight = FALSE) {
    shiny::tagList(lapply(seq_along(x), function(i) {
      shiny::tagList(if (i > 1) ", ", code(x[[i]], tight = tight && i == 1))
    }))
  }

  # Paso o elemento de una lista con el comienzo en negrita.
  item <- function(title, ...) {
    shiny::tags$li(shiny::strong(title), " ", ...)
  }

  # Sección numerada con su ancla para el índice.
  section <- function(anchor, number, title, ...) {
    shiny::tags$section(
      id = paste0("pk-help-", anchor),
      class = "pk-doc-section",
      shiny::h2(shiny::span(class = "pk-doc-number", number), title),
      ...
    )
  }

  # Subsección numerada con su ancla para el índice.
  subsection <- function(anchor, number, title, ...) {
    shiny::tagList(
      shiny::h3(
        id = paste0("pk-help-", anchor),
        class = "pk-doc-anchor",
        shiny::span(class = "pk-doc-number", number), title
      ),
      ...
    )
  }

  # Recuadro con un ejemplo paso a paso.
  # Recibe: título y contenido.
  # Devuelve: recuadro del ejemplo.
  example <- function(title, ...) {
    shiny::div(
      class = "pk-doc-example",
      shiny::p(class = "pk-doc-example-title", title),
      ...
    )
  }

  # Bloque de código para copiar en la aplicación.
  # Recibe: líneas del código.
  # Devuelve: etiqueta <pre>.
  code_block <- function(...) {
    shiny::tags$pre(class = "pk-doc-code", paste(c(...), collapse = "\n"))
  }

  # Modelo PK/PD del ejemplo de Crear modelo (apartado 2.7.2 de la guía de
  # usuario de mrgsolve).
  pkpd_code <- code_block(
    "$PROB Modelo PK/PD de respuesta indirecta",
    "",
    "$PARAM CL = 1, VC = 30, KA = 1.3",
    "KIN = 100, KOUT = 2, IC50 = 2",
    "",
    "$GLOBAL",
    "#define CP (CENT/VC)",
    "",
    "$CMT GUT CENT RESP",
    "",
    "$MAIN",
    "RESP_0 = KIN/KOUT;",
    "",
    "$ODE",
    "double INH = CP/(IC50+CP);",
    "dxdt_GUT = -KA*GUT;",
    "dxdt_CENT = KA*GUT - (CL/VC)*CENT;",
    "dxdt_RESP = KIN*(1-INH) - KOUT*RESP;",
    "",
    "$CAPTURE CP"
  )

  # Índice: secciones y, dentro, sus subsecciones (ancla = título).
  contents <- list(
    simulacion = list("Simulación"),
    crear = list("Crear modelo"),
    poblacional = list(
      "Análisis poblacional",
      pob_necesitas = "Qué necesitas",
      pob_csv = "Formato del CSV",
      pob_covariables = "Covariables",
      pob_datos = "Paso 1: Datos",
      pob_analisis = "Paso 2: Análisis",
      pob_resultados = "Paso 3: Resultados",
      pob_ejemplo = "Ejemplo",
      pob_avisos = "Avisos frecuentes",
      pob_limites = "Limitaciones"
    ),
    modelos = list("Mis modelos")
  )
  toc_link <- function(anchor, title) {
    shiny::tags$a(href = paste0("#pk-help-", anchor), title)
  }

  # Formato de las columnas del CSV.
  columns <- shiny::tags$table(
    class = "pk-doc-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Columna"), shiny::tags$th("Nombres que se reconocen"),
      shiny::tags$th("Tipo de dato"), shiny::tags$th("Cuándo es obligatoria")
    )),
    shiny::tags$tbody(
      shiny::tags$tr(
        shiny::tags$td("Paciente"),
        shiny::tags$td(names_list(population_column_names$group)),
        shiny::tags$td(
          "Texto o número (", code("1", tight = TRUE), ", ",
          code("P-01"), "...), sin vacíos"
        ),
        shiny::tags$td("Siempre")
      ),
      shiny::tags$tr(
        shiny::tags$td("Tiempo"),
        shiny::tags$td(names_list(population_column_names$time)),
        shiny::tags$td("Número, mayor o igual que 0, sin vacíos"),
        shiny::tags$td("Siempre")
      ),
      shiny::tags$tr(
        shiny::tags$td("Concentración"),
        shiny::tags$td(names_list(population_column_names$response)),
        shiny::tags$td("Número, mayor o igual que 0, sin vacíos"),
        shiny::tags$td("Siempre")
      ),
      shiny::tags$tr(
        shiny::tags$td("Dosis"),
        shiny::tags$td(
          "Cualquier nombre que empiece por ", code("Dose", tight = TRUE),
          " o ", code("Dosis", tight = TRUE), " (", code("DOSE", tight = TRUE),
          ", ", code("Dose_mg"), "...)"
        ),
        shiny::tags$td(
          "Número mayor que 0, el mismo en todas las filas de cada paciente"
        ),
        shiny::tags$td(
          "Si el modelo incluye compartimentos (como ocurre en la mayoría",
          "de los modelos, excepto en los definidos mediante $PRED)"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td("Covariables"),
        shiny::tags$td(
          "Cualquier otra columna; se reconocen, entre otras, ",
          names_list(c("WT", "SEX", "AGE", "HT", "BMI", "RACE"))
        ),
        shiny::tags$td(
          "Número o texto, el mismo en todas las filas de cada paciente",
          "(apartado 3.3)"
        ),
        shiny::tags$td("Opcionales")
      )
    )
  )

  # Qué indica cada covariable reconocida y sobre qué suele influir.
  covariate_info <- c(
    "Peso" = "Peso corporal en kg.",
    "Sexo" = "Sexo del paciente.",
    "Edad" = "Edad en años.",
    "Altura" = "Altura o talla en cm.",
    "Índice de masa corporal" = "IMC en kg/m².",
    "Raza" = "Raza u origen étnico, como categorías.",
    "Fumador" = "Si el paciente fuma."
  )
  # Tipo de dato de las covariables categóricas, con ejemplos de valores.
  categorical_values <- c(
    "Sexo" = "F / M, Mujer / Hombre, 0 / 1",
    "Raza" = "Texto o códigos 1, 2, 3...",
    "Fumador" = "Sí / No, 0 / 1"
  )
  labels <- unique(unname(population_known_covariates))
  covariates <- shiny::tags$table(
    class = "pk-doc-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Característica"),
      shiny::tags$th("Nombres que se reconocen"),
      shiny::tags$th("Tipo"), shiny::tags$th("Tipo de dato"),
      shiny::tags$th("Qué es")
    )),
    shiny::tags$tbody(lapply(labels, function(l) {
      shiny::tags$tr(
        shiny::tags$td(l),
        shiny::tags$td(names_list(
          names(population_known_covariates)[population_known_covariates == l]
        )),
        shiny::tags$td(
          if (l %in% population_categorical_labels) {
            "Categórica"
          } else {
            "Continua"
          }
        ),
        shiny::tags$td(
          if (l %in% population_categorical_labels) {
            paste0(
              "Texto o número, como mucho 6 valores (",
              categorical_values[[l]], ")"
            )
          } else {
            "Número (con decimales si hace falta)"
          }
        ),
        shiny::tags$td(unname(covariate_info[l]))
      )
    }))
  )

  # Unidad de los parámetros estimados, según su nombre.
  units <- shiny::tags$table(
    class = "pk-doc-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Parámetro"), shiny::tags$th("Nombres"),
      shiny::tags$th("Unidad")
    )),
    shiny::tags$tbody(
      shiny::tags$tr(
        shiny::tags$td("Aclaramientos"),
        shiny::tags$td("Empiezan por CL o Q (CL, CL2, Q, Q2...)"),
        shiny::tags$td("L/h")
      ),
      shiny::tags$tr(
        shiny::tags$td("Volúmenes"),
        shiny::tags$td("Empiezan por V (V, V2, VC, VP...)"),
        shiny::tags$td("L")
      ),
      shiny::tags$tr(
        shiny::tags$td("Constantes de velocidad"),
        shiny::tags$td("Empiezan por K (KA, KE, K10...)"),
        shiny::tags$td("1/h")
      ),
      shiny::tags$tr(
        shiny::tags$td("Otros"),
        shiny::tags$td("Cualquier otro nombre"),
        shiny::tags$td("Sin unidad indicada")
      )
    )
  )

  shiny::div(
    class = "pk-doc-page",
    shiny::tags$article(
      class = "pk-doc",

      shiny::tags$header(
        class = "pk-doc-header",
        shiny::p(class = "pk-doc-kicker", "PKLaboratory"),
        shiny::h1("Guía de uso")
      ),
      shiny::tags$nav(
        class = "pk-doc-toc",
        `aria-label` = "Índice",
        shiny::p(class = "pk-doc-toc-title", "Índice"),
        shiny::tags$ol(lapply(names(contents), function(a) {
          entry <- contents[[a]]
          subs <- entry[-1]
          shiny::tags$li(
            toc_link(a, entry[[1]]),
            if (length(subs)) {
              shiny::tags$ol(lapply(names(subs), function(s) {
                shiny::tags$li(toc_link(s, subs[[s]]))
              }))
            }
          )
        }))
      ),

      section(
        "simulacion", "1", "Simulación",
        shiny::p(
          "Esta sección permite simular la evolución de la concentración ",
          "de un fármaco a lo largo del tiempo mediante un modelo ",
          "farmacocinético y una pauta de dosificación definidos por el usuario. ",
          "No es necesario cargar datos externos."
        ),
        shiny::p(
          "El modelo puede seleccionarse de la biblioteca de mrgsolve, ",
          "del catálogo compartido o de los modelos guardados por el usuario."
        ),
        shiny::h3("Cómo se usa"),
        shiny::tags$ol(
          item(
            "Selecciona el modelo.",
            "Elige el modelo que deseas utilizar desde la barra superior."
          ),
          item(
            "Configura los parámetros del modelo.",
            "Introduce los valores correspondientes en el panel izquierdo.",
            " La aplicación comprueba que sean válidos y muestra un mensaje",
            " si detecta algún error. "
          ),
          item(
            "Define la pauta de administración.",
            "Selecciona el compartimento en el que se administra",
            " el fármaco e indica la dosis, el número de dosis,",
            " el intervalo entre dosis y el tiempo final de simulación.",
            " El intervalo entre dosis",
            "solo se utiliza cuando se administran varias dosis."
          ),
          item(
            "Establece la duración de la administración.",
            "Introduce un valor de 0 para administrar la dosis",
            " de forma instantánea (bolo) o un valor mayor que 0", 
            " para realizar una infusión a velocidad constante",
            " (dosis / duración).",
            " Si se administran varias dosis, la duración de la infusión", 
            " no puede superar el intervalo entre ellas. ",
          ),
          item(
            "Ejecuta la simulación.",
            "Pulsa el botón «Simular» para obtener los resultados."
          )
        ),
        shiny::h3("Qué obtienes"),
        shiny::tags$ul(
          item(
            "Evolución temporal.",
            "Representación gráfica de las variables seleccionadas",
            " a lo largo del tiempo. La gráfica puede descargarse",
            " en formato PNG."
          ),
          item(
            "Resultados de la simulación.",
            "Tabla con las primeras 150 filas de los resultados.",
            " Los resultados completos pueden descargarse en formato CSV",
            " o en una versión compatible con Excel."
          )
        ),
        example(
          "Ejemplo",
          shiny::p(
            "Al acceder a Simulación se carga, de forma predeterminada, ",
            "el ejemplo de la ",
            external_link(
              "https://mrgsolve.org/vignettes/01-get-started.html",
              "introducción a mrgsolve"
            ),
            ", con el modelo pk1. Pulsa Simular para ver el resultado."
          )
        ),
        shiny::h3("Limitaciones"),
        shiny::tags$ul(
          shiny::tags$li(
            "La pauta de administración debe mantener una dosis",
            "constante, un intervalo fijo entre dosis y, en caso",
            "de infusión, una duración uniforme. Todas las dosis",
            "se administran en el mismo compartimento."
          ),
          shiny::tags$li(
            "El número máximo de puntos temporales de simulación",
            "es de 20 000."
          )
        )
      ),

      section(
        "crear", "2", "Crear modelo",
        shiny::p(
          "Esta sección permite definir modelos propios mediante la sintaxis ",
          "de mrgsolve, incluidos modelos con efectos aleatorios, ",
          "biodisponibilidad, tiempos de latencia y eventos. El código ",
          "puede escribirse directamente, importarse desde un archivo .cpp ",
          "o generarse con el asistente por componentes. El panel derecho ",
          "muestra la validación del código durante su edición."
        ),
        shiny::h3("El código del modelo"),
        shiny::p(
          "El editor utiliza la sintaxis propia de mrgsolve, cada bloque ",
          "comienza con una directiva, como $PARAM, $CMT o $ODE, seguida ",
          "de su contenido. La finalidad y la sintaxis de cada bloque se ",
          "describen en la ", mrgsolve_spec_link(), "."
        ),
        shiny::p(
          "Dado que la compilación y la ejecución tienen lugar en el ",
          "servidor, el código se somete previamente a las siguientes ",
          "restricciones y comprobaciones:"
        ),
        shiny::tags$ul(
          item(
            "Nombre del modelo.", "Es obligatorio. Empieza por una letra y usa",
            "solo letras sin tilde, números, espacios, guiones o guiones",
            "bajos (máximo 30 caracteres). No puede coincidir con el de otro",
            "modelo tuyo ni con uno de la biblioteca. La descripción es",
            "opcional y admite hasta 200 caracteres."
          ),
          item(
            "Bloques admitidos.", "Se admiten los siguientes bloques: ", names_list(paste0("$", c(
              "PROB", "PARAM", "INIT", "CMT", "VCMT", "FIXED", "INPUT",
              "THETA", "OMEGA", "SIGMA", "SET", "PKMODEL", "CAPTURE",
              "PLUGIN", "GLOBAL", "PREAMBLE", "MAIN", "PK", "ODE", "DES",
              "TABLE", "ERROR", "EVENT", "PRED"
            ))), "."
          ),
          item(
            "Plugins admitidos.", "Se admiten los siguientes plugins: ", names_list(allowed_plugins), "."
          ),
          item(
            "Bloques de valores ($PARAM, $INIT, $OMEGA, $SET...).", "Solo se admiten",
            "números, también con el formato anotado",
            code("CL : 5 : Aclaramiento (L/h)"), "."
          ),
          item(
            "Bloques de C++ ($MAIN, $ODE, $TABLE, $EVENT...).", "Se admiten variables,",
            "if/else, operaciones, funciones matemáticas, las de mrgsolve",
            "(ETA(n), EPS(n), THETA(n), self.mevent()...), R::rnorm() y",
            "similares (con el plugin Rcpp) y las de evtools y tad."
          ),
          item(
            "No se admiten.", "Se excluyen $ENV, $INCLUDE, $NMXML y $NMEXT; las opciones",
            "que evalúan código R (@code, @param, @etas...); #include,",
            "cadenas de texto, punteros, corchetes, bucles y definiciones de",
            "funciones."
          )
        ),
        shiny::p(
          "Si el código no compila, se muestran los errores del",
          "compilador junto a la línea que los produce."
        ),
        shiny::h3("Asistente por componentes"),
        shiny::p(
          "Para crear el modelo sin escribir código, pulsa",
          shiny::strong("Asistente por componentes", .noWS = "after"),
          ", rellena el formulario y pulsa",
          shiny::strong("Generar código", .noWS = "after"), ". El",
          "código generado sustituye al del editor, donde puedes",
          "completarlo con otros bloques."
        ),
        shiny::tags$ol(
          item(
            "Parámetros.", "Introduce uno por línea, con la forma nombre = valor, por",
            "ejemplo", code("CL = 5"), "."
          ),
          item(
            "Compartimentos.", "Representan las cantidades que evolucionan, con su",
            "valor inicial (0 si empiezan vacío, por ejemplo",
            code("CENT = 0"), "."
          ),
          item(
            "Variables auxiliares (opcional).", "Permiten definir expresiones que simplifican",
            "las ecuaciones, como", code("CP = CENT / V"), "."
          ),
          item(
            "Ecuaciones diferenciales.", "Introduce una por compartimento, indicando ",
            "la derivada temporal de la cantidad que contiene, por ejemplo ",
            code("CENT = -CL / V * CENT"),
            ", sin dxdt_ ni punto y coma."
          ),
          item(
            "Variables de salida.", "Indica los parámetros o variables que quieres",
            "consultar, separados por comas. Los compartimentos aparecen",
            "siempre en los resultados."
          )
        ),
        shiny::p("El asistente aplica las siguientes reglas:"),
        shiny::tags$ul(
          item(
            "Nombres de parámetros, compartimentos y variables.", "Deben ser únicos,",
            "de hasta 30 caracteres, con letras, números y guion bajo, y",
            "empezando por una letra."
          ),
          item(
            "Valores.", "Se introducen como números con punto decimal. Como máximo, 50",
            "parámetros, 30 compartimentos (al menos uno) y 50 variables",
            "auxiliares."
          ),
          item(
            "Expresiones.", "Pueden usar los parámetros, los",
            "compartimentos, el tiempo (", code("TIME", tight = TRUE),
            ") y las variables auxiliares definidas antes; las operaciones",
            names_list(c("+", "-", "*", "/", "^")), ", las comparaciones y",
            names_list(c("&&", "||", "!")), "; y las funciones",
            names_list(c(
              "exp", "log", "log10", "sqrt", "abs", "sin", "cos", "tan",
              "floor", "ceiling", "min", "max", "pow", "ifelse"
            )), "."
          )
        ),
        shiny::h3("Guardar y descargar"),
        shiny::p(
          shiny::strong("Guardar modelo"),
          "valida el código, compila el modelo y lo prueba.",
          " Si todo va bien, queda en Mis modelos como",
          "privado y aparece en los selectores de Simulación y de Análisis",
          "poblacional.", shiny::strong("Descargar .cpp"),
          " permite descargar el código."
        ),
        example(
          "Ejemplo",
          shiny::p(
            "Este modelo es el ejemplo PK/PD de la ",
            mrgsolve_spec_link("guía de usuario de mrgsolve.")
          ),
          shiny::tags$ol(
            shiny::tags$li("Asigna un nombre al modelo.."),
            shiny::tags$li("Copia este código en el editor.", pkpd_code),
            shiny::tags$li("Pulsa Guardar modelo."),
            shiny::tags$li("En Simulación, pulsa Simular.")
          )
        )
      ),

      section(
        "poblacional", "3", "Análisis poblacional",
        shiny::p(
          "Esta sección permite analizar conjuntamente las concentraciones ",
          "observadas en diferentes pacientes. Mediante modelos no lineales ",
          "de efectos mixtos se estiman los parámetros farmacocinéticos ",
          "poblacionales, se cuantifica la variabilidad interindividual ",
          "y se evalúa la influencia de las covariables disponibles. ",
          "El ajuste emplea el algoritmo de Lindstrom y Bates, implementado ",
          "en el paquete nlme. El procedimiento comprende tres etapas: ",
          "Datos, Análisis y Resultados."
        ),
        subsection(
          "pob_necesitas", "3.1", "Qué necesitas",
          shiny::tags$ul(
            item(
              "Un modelo farmacocinético.",
              "Selecciónalo en la barra superior. Puedes utilizar",
              "cualquiera de los modelos disponibles en la aplicación,",
              "siempre que los datos proporcionados sean compatibles",
              "con el modelo seleccionado."
            ),
            item(
              "Un archivo CSV.",
              "Debe contener las concentraciones observadas de cada paciente,",
              "organizadas según el formato descrito en el apartado 3.2."
            )
          )
        ),

        subsection(
          "pob_csv", "3.2", "Formato del CSV",
          shiny::p(
            "El archivo CSV debe contener una fila por cada concentración",
            "medida, con columnas que identifiquen al paciente, el tiempo",
            "de muestreo y la concentración observada. También puede incluir",
            "características individuales (peso, sexo, edad, etc.) que se",
            "deseen evaluar como covariables.",
            "Se admiten archivos separados por comas (,) o por puntos",
            "y comas (;), estos últimos con coma decimal.",
            "El archivo debe contener entre 10 y 200 000 filas.",
            "Las columnas se identifican por su nombre:"
          ),
          columns,
          shiny::p("Los datos deben cumplir los siguientes requisitos:"),
          shiny::tags$ul(
            item(
              "Pacientes.",
              "Se requieren al menos 5 pacientes, con un mínimo de 3 concentraciones",
              " medidas por paciente."
            ),
            item(
              "Tiempo.",
              "Debe medirse desde la administración de la dosis,",
              " sin valores negativos y en ",
              names_list(names(population_time_units)), "."
            ),
            item(
              "Concentración.",
              "No debe contener valores negativos ni ausentes y debe expresarse en ",
              names_list(names(population_conc_units)), "."
            ),
            item(
              "Dosis.",
              "Se considera una única dosis administrada en t = 0.",
              " Su valor debe ser el mismo en todas las filas de cada",
              "paciente y expresarse en ",
              names_list(names(population_dose_units)), ".",
              "Si se utiliza mg/kg, la dosis se calcula a partir",
              "de una columna que contenga el peso del paciente en kg."
            ),
            item(
              "Covariables.",
              "Deben cumplir los requisitos descritos en el apartado 3.3."
            )
          ),
          shiny::p(
            "Si el archivo CSV contiene columnas adicionales que",
            " no son necesarias para el análisis, como las columnas",
            " de eventos de NONMEM (",
            names_list(population_event_columns, tight = TRUE),
            ") o los índices de fila añadidos al exportar datos",
            " desde R o Python (rownames, X, etc.),",
            " la aplicación las ignora o elimina automáticamente."
          )
        ),

        subsection(
          "pob_covariables", "3.3", "Covariables",
          shiny::p(
            "Las covariables son características individuales que pueden ",
            "explicar parte de las diferencias farmacocinéticas entre ",
            "pacientes; por ejemplo, la relación entre el peso corporal ",
            "y el aclaramiento. La aplicación examina como posibles ",
            "covariables las columnas del CSV distintas de las de ",
            "identificación, tiempo, concentración y dosis. Reconoce ",
            "automáticamente los siguientes nombres:"
          ),
          covariates,
          shiny::p(
            "También puedes incluir covariables con nombres de columna",
            " distintos de los reconocidos por la aplicación.",
            " En ese caso, la columna aparecerá como no reconocida",
            " y la aplicación determinará si es continua o categórica",
            " a partir de sus valores.",
            " Para poder utilizarla como covariable, debe cumplir",
            " los siguientes requisitos:"
          ),
          shiny::tags$ul(
            shiny::tags$li(
              "Mantener el mismo valor en todas las filas de un mismo",
              " paciente. No se admiten covariables que varíen",
              " a lo largo del tiempo."
            ),
            shiny::tags$li(
              "No contener valores ausentes."
            ),
            shiny::tags$li(
              "Presentar al menos dos valores diferentes entre",
              " los pacientes."
            ),
            shiny::tags$li(
              "Si es categórica, tener un máximo de 6 categorías."
            )
          ),
          shiny::p(
            shiny::strong("Tipos de covariables. "),
            "La aplicación clasifica las covariables en continuas",
            " o categóricas según sus valores:"
          ),
          shiny::tags$ul(
            shiny::tags$li(
              "Continuas: columnas numéricas con más de dos valores",
              " distintos, como el peso o la edad."
            ),
            shiny::tags$li(
              "Categóricas: columnas con valores de texto o con solo",
              " dos valores numéricos distintos, como el sexo o",
              " la condición de fumador."
            )
          ),
          shiny::p(
            "Las variables sexo, raza y fumador se consideran siempre",
            " categóricas, aunque estén codificadas mediante números",
            " (por ejemplo, raza = 1, 2 o 3)."
          ),
          shiny::p(
            shiny::strong("Efecto sobre los parámetros. "),
            "Las covariables se incorporan a los parámetros",
            " farmacocinéticos que selecciones, como el aclaramiento",
            " (CL) o el volumen de distribución (V).",
            " Una misma covariable puede influir en varios parámetros,",
            " por ejemplo, el efecto del peso sobre CL y sobre V",
            " se estima de forma independiente."
          ),
          shiny::p(
            "La forma en que una covariable modifica un parámetro",
            " depende de la escala utilizada. Si el parámetro se",
            " modeliza en escala logarítmica, la covariable produce",
            " un efecto multiplicativo sobre su valor típico.",
            " Si se modeliza en escala natural, su efecto es aditivo."
          ),
          shiny::tags$ul(
            item(
              "Covariable continua, forma de potencia (por defecto).",
              "Se utiliza la expresión ", code("θ · (cov / mediana)^β"),
              ". El efecto se calcula en relación con la mediana",
              " de la covariable. El coeficiente β determina cómo",
              " cambia el parámetro farmacocinético al variar",
              " la covariable. Es una relación habitual para describir",
              " el efecto del peso corporal."
            ),
            item(
              "Covariable continua, forma lineal en escala logarítmica.",
              "Se utiliza la expresión ", code("θ · exp(β · (cov − mediana))"),
              ". El coeficiente β determina el cambio proporcional",
              " del parámetro por cada unidad de aumento de la",
              " covariable. Esta forma permite utilizar covariables",
              " con valores positivos, negativos o iguales a cero."
            ),
            item(
              "Covariable categórica.",
              "Se utiliza la expresión ", code("θ · exp(β)"),
              ". Se toma como referencia la primera categoría",
              " en orden alfabético y se estima un coeficiente β",
              " para cada una de las categorías restantes.",
              " Cada coeficiente representa la diferencia proporcional",
              " respecto a la categoría de referencia."
            )
          ),
          shiny::p(
            "En estas expresiones, θ representa el valor típico",
            " del parámetro farmacocinético, cov es el valor de",
            " la covariable y β es el coeficiente que cuantifica",
            " su efecto. La mediana corresponde al valor central",
            " de la covariable en la población estudiada."
          ),
          shiny::p(
            "Las expresiones anteriores corresponden a parámetros",
            " modelizados en escala logarítmica, donde las covariables",
            " producen efectos multiplicativos. Si el parámetro se",
            " modeliza en escala natural, los efectos se incorporan",
            " de forma aditiva. La forma de potencia requiere",
            " que los valores de la covariable sean positivos."
          )

        ),

        subsection(
          "pob_datos", "3.4", "Paso 1: Datos",
          shiny::tags$ol(
            item(
              "Selecciona el modelo farmacocinético.",
              "Elígelo desde la barra superior."
            ),
            item(
              "Carga el archivo CSV.",
              "La aplicación comprueba que los datos cumplan",
              " los requisitos del modelo seleccionado.",
              " Si detecta algún problema, muestra un mensaje",
              " indicando el motivo y solicita cargar otro archivo."
            ),
            item(
              "Revisa las columnas identificadas.",
              "Comprueba las columnas para el paciente, el tiempo de muestreo,",
              " la concentración observada y, si el modelo",
              " lo requiere, la dosis. Comprueba que sean",
              " correctas e indica las unidades correspondientes.",
              " Si la dosis está en mg/kg, elige también la columna",
              " con el peso de cada paciente en kg."
            ),
            item(
              "Selecciona el compartimento de administración.",
              "Si la dosis se administró por vía intravenosa,",
              " selecciona el compartimento central (por ejemplo, CENT).",
              " Si se administró por vía oral u otra vía extravascular,",
              " selecciona el compartimento de absorción",
              " (por ejemplo, EV), según la estructura del modelo.",
              " Si la forma de las curvas no encaja con el compartimento",
              " elegido, se muestra un aviso en amarillo (apartado 3.8)."
            ),
            item(
              "Pulsa «Continuar con el análisis».",
              "El botón solo aparece cuando los datos sirven para el",
              " modelo elegido."
            )
          ),
          shiny::p(
            "En el panel de la derecha se muestran las posibles",
            " covariables identificadas, indicando cuáles pueden",
            " utilizarse y por qué se descartan las que no cumplen",
            " los requisitos. También se presentan las curvas",
            " de concentración-tiempo de cada paciente y una",
            " vista previa de las primeras filas del archivo CSV."
          ),
          shiny::p(
            "Antes de realizar el ajuste, la aplicación convierte",
            " automáticamente los datos a unidades de horas (h),",
            " miligramos (mg) y miligramos por litro (mg/L).",
            " Si el modelo expresa la concentración como cantidad",
            " dividida por volumen, los parámetros estimados se",
            " expresan en unidades coherentes con esta conversión:",
            " aclaramiento (CL) en L/h, volúmenes de distribución",
            " en L y constantes de velocidad en 1/h."
          )
        ),
        subsection(
          "pob_analisis", "3.5", "Paso 2: Análisis",

          shiny::p(
            shiny::strong("Ajustes individuales. "),
            "En el recuadro superior del panel izquierdo, marca los",
            " parámetros que se estiman en cada paciente y pulsa",
            " «Ajustar cada paciente». A la derecha se muestra la",
            " estimación de cada paciente con su intervalo de confianza",
            " del 95 % y, en una tabla desplegable, sus errores",
            " estándar y p-valores.",
            " Se recomienda realizar estos ajustes individuales mediante ",
            "regresión no lineal (nlsList) antes de estimar el modelo ",
            "poblacional. Sus resultados permiten explorar las diferencias ",
            "entre pacientes y proponer valores iniciales para el ajuste mixto.",
            " Los parámetros cuyas estimaciones presentan diferencias",
            " apreciables entre pacientes pueden ser candidatos",
            " a incluir efectos aleatorios."
          ),

          shiny::p(
            "La opción «Usar la mediana como valores iniciales»",
            " permite utilizar la mediana de las estimaciones",
            " individuales como punto de partida del modelo poblacional.",
            " Los ajustes individuales emplean la escala y los valores",
            " iniciales definidos en el panel del modelo mixto, dentro",
            " de «Opciones avanzadas». Los parámetros no seleccionados",
            " para su estimación se mantienen fijos en su valor inicial."
          ),

          shiny::p(
            shiny::strong("Modelo mixto. "),
            "A continuación, se configura el modelo poblacional",
            " mediante las siguientes opciones:"
          ),

          shiny::tags$ul(
            item(
              "Escala de los parámetros.",
              "Permite elegir entre escala natural y logarítmica.",
              " En escala logarítmica, el parámetro se mantiene",
              " positivo y se asume una distribución lognormal",
              " de su variabilidad interindividual, cuando incluye",
              " un efecto aleatorio normal. Los parámetros no",
              " seleccionados se modelizan en escala natural.",
              " En los modelos con aclaramiento (CL) y volumen",
              " central (V), se utiliza la constante de eliminación",
              " KE como parámetro de ajuste, calculándose",
              " posteriormente V = CL / KE."
            ),

            item(
              "Efectos aleatorios.",
              "Permite seleccionar los parámetros farmacocinéticos",
              " que presentan variabilidad interindividual.",
              " Todos los parámetros estimados tienen un valor",
              " típico poblacional (efecto fijo), mientras que",
              " los seleccionados incorporan además un efecto",
              " aleatorio que representa las diferencias entre",
              " pacientes no explicadas por el modelo."
            ),

            item(
              "Matriz de varianzas y covarianzas.",
              "Cuando se seleccionan dos o más efectos aleatorios,",
              " permite definir su relación. La estructura diagonal",
              " (pdDiag) supone que los efectos aleatorios son",
              " independientes, mientras que la estructura completa",
              " (pdSymm) permite estimar también las covarianzas",
              " y correlaciones entre ellos."
            ),

            item(
              "Covariables.",
              "Permite seleccionar qué características de los",
              " pacientes influyen sobre cada parámetro farmacocinético",
              " (por ejemplo, el efecto del peso sobre CL).",
              " Las covariables continuas se expresan en relación",
              " con su mediana y las categóricas se comparan",
              " con una categoría de referencia, como se explica",
              " en el apartado 3.3."
            ),

            item(
              "Error de medida (error residual).",
              "Describe la variabilidad de las concentraciones",
              " observadas respecto a las predicciones individuales",
              " del modelo. Se puede elegir entre tres opciones:"
            )
          ),

          shiny::tags$ul(
            class = "pk-doc-sublist",
            item(
              "Aditivo.",
              "La desviación típica del error es constante (σ),",
              " independientemente de la concentración.",
              " Es adecuado cuando la magnitud del error es similar",
              " para concentraciones bajas y altas."
            ),

            item(
              "Proporcional.",
              "La desviación típica del error es proporcional",
              " al valor absoluto de la concentración predicha",
              " (σ · |predicción|). El error aumenta a medida",
              " que aumenta la concentración."
            ),

            item(
              "Combinado.",
              "La desviación típica del error se expresa como",
              " σ · (δ + |predicción|). Combina un componente",
              " constante con otro proporcional, de modo que",
              " el primero predomina a concentraciones bajas",
              " y el segundo a concentraciones altas."
            )
          ),

          shiny::p(
            "Cuando se selecciona un modelo de error proporcional",
            " o combinado, la aplicación excluye las observaciones",
            " correspondientes a t = 0, donde la concentración",
            " predicha es cero."
          ),

          shiny::p(
            shiny::strong("Opciones avanzadas. "),
            "Permiten configurar otros aspectos del ajuste:"
          ),

          shiny::tags$ul(
            item(
              "Salida medida.",
              "Selecciona la variable de salida del modelo",
              " que corresponde a las concentraciones observadas",
              " en el archivo CSV (por defecto, CP o equivalente)."
            ),

            item(
              "Parámetros con valor fijado.",
              "Permite mantener determinados parámetros constantes",
              " durante el ajuste, utilizando su valor inicial.",
              " En estos casos no se estima su valor poblacional",
              " ni se incorpora variabilidad interindividual.",
              " Esta opción resulta útil cuando se dispone de",
              " información previa fiable sobre sus valores."
            ),

            item(
              "Forma del efecto de las covariables continuas.",
              "Permite elegir entre la forma de potencia",
              " y la lineal en escala logarítmica, descritas en el",
              " apartado 3.3."
            ),

            item(
              "Valores iniciales.",
              "Establece el valor de partida de cada parámetro",
              " para el procedimiento de estimación, con sus",
              " unidades correspondientes. Por defecto, se",
              " utilizan los valores definidos en el modelo,",
              " aunque pueden sustituirse por las medianas",
              " obtenidas en los ajustes individuales."
            )
          ),

          shiny::p(
            "Al pulsar «Estimar parámetros», comienza el ajuste del ",
            "modelo no lineal de efectos mixtos. El algoritmo de Lindstrom ",
            "y Bates, implementado en nlme, alterna etapas de mínimos ",
            "cuadrados no lineales penalizados (PNLS) y de ajuste de un ",
            "modelo lineal mixto (LME) hasta satisfacer el criterio de ",
            "convergencia o alcanzar los límites de iteración. El tiempo ",
            "necesario depende del volumen de datos y de la complejidad ",
            "del modelo."
          )
        ),
        subsection(
          "pob_resultados", "3.6", "Paso 3: Resultados",

          shiny::p(
            "Una vez finalizado el ajuste, la aplicación muestra",
            " los parámetros estimados del modelo poblacional,",
            " la variabilidad interindividual y las estimaciones",
            " correspondientes a cada paciente. Arriba se indican el",
            " número de pacientes y de parámetros estimados, con los",
            " botones para volver al análisis y descargar los",
            " resultados en CSV. Si nlme no ha podido calcular los",
            " intervalos de confianza, se avisa.",
            " Los resultados se organizan en los siguientes apartados:"
          ),

          shiny::tags$ul(
            item(
              "Efectos fijos.",
              "Muestra los valores estimados de los parámetros",
              " poblacionales (β) en la escala utilizada durante",
              " el ajuste (natural o logarítmica) y su transformación",
              " a escala natural, con las unidades correspondientes.",
              " Para los parámetros ajustados en escala logarítmica,",
              " esta transformación se realiza mediante exp(β).",
              " También se presentan sus intervalos de confianza",
              " del 95 % y los coeficientes que cuantifican",
              " el efecto de las covariables."
            ),

            item(
              "Variabilidad.",
              "Muestra la desviación típica de cada efecto aleatorio,",
              " que cuantifica la variabilidad entre pacientes",
              " no explicada por los efectos fijos y las covariables.",
              " Para los parámetros modelizados en escala logarítmica",
              " también se presenta el coeficiente de variación (CV).",
              " Si se han estimado correlaciones entre efectos",
              " aleatorios, estas también se muestran, junto con",
              " los parámetros del modelo de error residual.",
              " Los intervalos de confianza se calculan mediante",
              " la función intervals() del paquete nlme."
            ),

            item(
              "Estimaciones de cada paciente.",
              "Muestra los efectos aleatorios estimados para",
              " cada paciente, equivalentes a los obtenidos",
              " mediante ranef() de nlme, y los valores de sus",
              " parámetros farmacocinéticos individuales",
              " expresados en escala natural."
            ),

            item(
              "Resumen (summary de nlme).",
              "Contiene la salida de summary() de R para el modelo ajustado,",
              " que se puede descargar en TXT."
            )
          ),

          shiny::p(
            "Si el ajuste falla, en lugar de los resultados se muestra",
            " el registro del ajuste: la configuración, el código del",
            " modelo de mrgsolve, cada iteración del algoritmo (pasos",
            " LME y PNLS, con los efectos fijos y el criterio de",
            " convergencia), las llamadas a mrgsolve con parámetros no",
            " válidos (con sus valores), los avisos y el error final.",
            " Se puede descargar en TXT."
          ),

          shiny::p(
            "Las unidades de los parámetros farmacocinéticos",
            " se indican según su denominación:"
          ),

          units
        ),
        subsection(
          "pob_ejemplo", "3.7", "Ejemplo",
          shiny::p(
            "Este ejemplo usa los datos Theoph de R con el modelo pk1, como",
            "Pinheiro y Bates."
          ),
          shiny::div(
            class = "pk-doc-downloads",
            shiny::downloadButton(
              "help_example_theoph", "Descargar theoph.csv",
              class = "btn-sm btn-outline-primary"
            )
          ),
          example(
            "Paso a paso",
            shiny::tags$ol(
              shiny::tags$li("Elige el modelo pk1."),
              shiny::tags$li(
                "Carga theoph.csv. En la dosis, elige la unidad mg/kg y la",
                "columna Wt como peso."
              ),
              shiny::tags$li(
                "Elige EV como compartimento de la dosis y pulsa",
                "«Continuar con el análisis»."
              ),
              shiny::tags$li(
                "Pulsa «Ajustar cada paciente» y después «Usar la mediana",
                "como valores iniciales»."
              ),
              shiny::tags$li("Pulsa «Estimar parámetros».")
            ),
            shiny::p(
              "Obtendrás un CL de unos 2,7 L/h, una KE de unos 0,086 1/h y",
              "una KA de unos 1,6 1/h."
            )
          )
        ),
        subsection(
          "pob_avisos", "3.8", "Avisos frecuentes",
          shiny::tags$dl(
            shiny::tags$dt(
              "Este CSV no contiene los datos necesarios para el modelo"
            ),
            shiny::tags$dd(
              "Falta alguna columna con un nombre reconocido (apartado 3.2)",
              "o hay pocos pacientes o medidas. El mensaje indica qué falta,",
              "corrige el CSV y vuelve a subirlo, o elige otro modelo."
            ),
            shiny::tags$dt(
              "No hay fase de absorción o la concentración sube al",
              "principio"
            ),
            shiny::tags$dd(
              "La forma de las curvas no encaja con el compartimento donde",
              "entra la dosis. Elige el compartimento central si la dosis",
              "fue intravenosa, o el de absorción si fue oral."
            ),
            shiny::tags$dt(
              "Con la dosis en mg/kg, elige una columna de peso (kg)"
            ),
            shiny::tags$dd(
              "La columna elegida como peso no sirve, tiene valores",
              "ausentes, ceros o negativos. Elige la columna correcta o",
              "cambia la unidad de la dosis."
            ),
            shiny::tags$dt("Pacientes que no se pueden ajustar por separado"),
            shiny::tags$dd(
              "Los parámetros sin marcar se dejan fijos en su valor",
              "inicial; si ese valor no corresponde a tus datos, el resto no",
              "basta. Ajusta todos los parámetros, pulsa «Usar la mediana",
              "como valores iniciales» y vuelve a probar."
            ),
            shiny::tags$dt("El modelo mixto no converge"),
            shiny::tags$dd(
              "Revisa el registro del ajuste para ver en qué iteración",
              "falla. Usa la mediana de los ajustes individuales como",
              "valores iniciales y prueba con menos efectos aleatorios, con",
              "Ψ diagonal o sin covariables."
            )
          ),
          shiny::p(
            "La aplicación no sustituye la evaluación estadística y ",
            "farmacocinética del usuario. La elección del modelo y la ",
            "interpretación de los resultados requieren criterio técnico."
          )
        ),
        subsection(
          "pob_limites", "3.9", "Limitaciones",
          shiny::tags$ul(
            shiny::tags$li(
              "Se admite una única dosis por paciente, administrada",
              " en t = 0 y en un solo compartimento.",
              " No se permiten dosis múltiples, infusiones ni",
              " administraciones en distintos momentos."
            ),
            shiny::tags$li(
              "El análisis considera un único nivel de agrupación:",
              " el paciente. No permite modelizar por separado",
              " la variabilidad entre ocasiones o entre centros."
            ),
            shiny::tags$li(
              "Las covariables deben permanecer constantes para",
              " cada paciente durante todo el estudio.",
              " No se admiten covariables que varíen con el tiempo."
            ),
            shiny::tags$li(
              "La estimación se realiza mediante el algoritmo",
              " de Lindstrom y Bates, implementado en nlme,",
              " utilizando máxima verosimilitud.",
              " No se incluyen otros métodos de estimación."
            ),
            shiny::tags$li(
              "El análisis requiere datos observados proporcionados",
              " mediante un archivo CSV."
            )
          )
        )
      ),
      section(
        "modelos", "4", "Mis modelos",
        shiny::p(
          "Esta sección reúne los modelos farmacocinéticos guardados ",
          "por el usuario y permite administrarlos. Para cada modelo ",
          "se muestran sus principales características, su descripción ",
          "y su visibilidad, ya sea privado o compartido en el catálogo común."
        ),
        shiny::tags$ul(
          item(
            "Editar.",
            "Permite modificar el código del modelo en el mismo",
            " editor de la creación. Al guardar los cambios,",
            " la aplicación vuelve a validar y compilar el modelo",
            " para comprobar que funciona correctamente."
          ),
          item(
            "Añadir al catálogo / Retirar del catálogo.",
            "Permite compartir un modelo con el resto de usuarios",
            " o volver a hacerlo privado. Los modelos publicados",
            " en el catálogo común pueden ser utilizados por",
            " otros usuarios para realizar simulaciones y",
            " análisis farmacocinéticos poblacionales."
          ),
          item(
            "Eliminar.",
            "Permite eliminar un modelo de tu cuenta."
          ),
          item(
            "Crear modelo nuevo.",
            "Abre la sección «Crear modelo», donde puedes",
            " construir y guardar un nuevo modelo farmacocinético."
          )
        )
      )
    )
  )
}
