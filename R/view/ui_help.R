# ============================================================
# Archivo: ui_help.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
# Aplicación Shiny para simulación farmacocinética con mrgsolve
# URL: https://pklaboratory.shinyapps.io/pklaboratory/
#
# Descripción:
# Contiene la función para construir la guía de uso de la aplicación,
# maquetada como un documento (una hoja con índice y secciones
# numeradas) y dividida según las páginas de la aplicación: Simulación,
# Crear modelo, Análisis poblacional (con el formato del CSV y ejemplos
# para descargar) y Mis modelos. Los nombres de columna y las unidades se
# leen de R/model/population.R, para que la guía no se desfase.
# ============================================================

# Construye la guía de uso.
# Devuelve: documento con la guía.
help_ui <- function() {

  # Lista de nombres separados por comas, en código.
  names_list <- function(x) {
    shiny::tagList(lapply(seq_along(x), function(i) {
      shiny::tagList(if (i > 1) ", ", shiny::tags$code(x[[i]]))
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
          "Texto o número (", shiny::tags$code("1"), ", ",
          shiny::tags$code("P-01"), "...), sin vacíos"
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
          "Cualquier nombre que empiece por ", shiny::tags$code("Dose"),
          " o ", shiny::tags$code("Dosis"), " (", shiny::tags$code("DOSE"),
          ", ", shiny::tags$code("Dose_mg"), "...)"
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
          "Simula la evolución temporal de las concentraciones de un fármaco ",
          "a partir de un modelo farmacocinético y de la pauta de dosificación",
          " que elijas, sin necesidad de utilizar datos externos."
        ),
        shiny::p(
          "Puedes seleccionar el modelo de tres fuentes: la biblioteca de ",
          "mrgsolve, el catálogo común de modelos o tus propios modelos."
        ),
        shiny::h3("Cómo se usa"),
        shiny::tags$ol(
          item(
            "Selecciona el modelo",
            "en la barra superior."
          ),
          item(
            "Configura los parámetros del modelo",
            "en el panel de la izquierda. La aplicación comprueba",
            "que los valores introducidos sean válidos y muestra",
            "un mensaje de error si detecta alguno incorrecto."
          ),
          item(
            "Define la pauta de administración:",
            "selecciona el compartimento en el que se administra",
            "el fármaco e indica la dosis, el número de dosis,",
            "el intervalo entre dosis, el tiempo final de simulación",
            "y la duración de la infusión. El intervalo entre dosis",
            "solo se utiliza cuando se administran varias dosis."
          ),
          item(
            "Selecciona el tipo de administración:",
            "si la duración de la infusión es 0, la dosis se administra",
            "de forma instantánea (bolo). Si es mayor que 0,",
            "se administra a velocidad constante (dosis / duración)",
            "durante el tiempo indicado. La duración de la infusión",
            "no puede superar el intervalo entre dosis cuando",
            "se administran varias dosis."
          ),
          item(
            "Ejecuta la simulación",
            "pulsando el botón Simular."
          )
        ),
        shiny::h3("Qué obtienes"),
        shiny::tags$ul(
          item(
            "Evolución temporal:",
            "representación gráfica de las variables seleccionadas",
            "a lo largo del tiempo. La gráfica se puede descargar",
            "en formato PNG."
          ),
          item(
            "Resultados de la simulación:",
            "tabla con las primeras 150 filas de los resultados.",
            "La tabla completa se puede descargar en formato CSV",
            "o en una versión CSV compatible con Excel."
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
            "La simulación representa a un único individuo con",
            "los parámetros especificados, sin considerar",
            "la variabilidad interindividual."
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
          "Construye tu propio modelo farmacocinético seleccionando y",
          "configurando sus componentes, sin necesidad de escribir",
          "código de mrgsolve. El código se genera automáticamente",
          "en el panel de la derecha y se actualiza al modificar",
          "los valores del formulario."
        ),
        shiny::h3("Cómo se rellena"),
        shiny::tags$ol(
          item(
            "Nombre y descripción", "del modelo (la descripción es",
            "opcional)."
          ),
          item(
            "Parámetros:", "uno por línea, con la forma nombre = valor, por",
            "ejemplo", shiny::tags$code("CL = 5"), "."
          ),
          item(
            "Compartimentos:", "las cantidades que evolucionan, con su",
            "valor inicial (0 si empiezan vacíos):",
            shiny::tags$code("CENT = 0"), "."
          ),
          item(
            "Variables auxiliares", "(opcional): expresiones que simplifican",
            "las ecuaciones, como", shiny::tags$code("CP = CENT / V"), "."
          ),
          item(
            "Ecuaciones diferenciales:", "una por compartimento, con la",
            "derivada de su cantidad:",
            shiny::tags$code("CENT = -CL / V * CENT"),
            ", sin dxdt_ ni punto y coma."
          ),
          item(
            "Variables de salida:", "los parámetros o variables que quieres",
            "consultar, separados por comas. Los compartimentos aparecen",
            "siempre en los resultados."
          )
        ),
        shiny::h3("Reglas"),
        shiny::tags$ul(
          item(
            "Nombre del modelo:", "empieza por una letra y usa solo letras",
            "sin tilde, números, espacios, guiones o guiones bajos (máximo",
            "30 caracteres). No puede coincidir con el de otro modelo tuyo",
            "ni con uno de la biblioteca. La descripción admite hasta 200",
            "caracteres."
          ),
          item(
            "Nombres de parámetros, compartimentos y variables:", "únicos,",
            "de hasta 30 caracteres, con letras, números y guion bajo, y",
            "empezando por una letra."
          ),
          item(
            "Valores:", "números con punto decimal. Como máximo, 50",
            "parámetros, 30 compartimentos (al menos uno) y 50 variables",
            "auxiliares."
          ),
          item(
            "Expresiones:", "pueden usar los parámetros, los",
            "compartimentos, el tiempo (", shiny::tags$code("TIME"),
            ") y las variables auxiliares definidas antes; las operaciones",
            names_list(c("+", "-", "*", "/", "^")), ", las comparaciones y",
            names_list(c("&&", "||", "!")), "; y las funciones",
            names_list(c(
              "exp", "log", "log10", "sqrt", "abs", "sin", "cos", "tan",
              "floor", "ceiling", "min", "max", "pow", "ifelse"
            )), "."
          ),
          item(
            "Tipo de modelo:", "solo se crean modelos de ecuaciones",
            "diferenciales (con compartimentos)."
          )
        ),
        shiny::h3("Guardar y descargar"),
        shiny::p(
          shiny::strong("Guardar modelo"),
          "valida el formulario, compila el modelo y lo prueba con una",
          "dosis de 100 en el primer compartimento durante 24 unidades de",
          "tiempo. Si todo va bien, queda en Mis modelos como",
          "privado y aparece en los selectores de Simulación y de Análisis",
          "poblacional.", shiny::strong("Descargar .cpp"),
          "guarda el código generado para usarlo con mrgsolve fuera de la",
          "aplicación."
        )
      ),

      section(
        "poblacional", "3", "Análisis poblacional",
        shiny::p(
          "Realiza un análisis farmacocinético poblacional a partir",
          "de las concentraciones observadas en distintos pacientes.",
          "Permite estimar los parámetros farmacocinéticos típicos",
          "de la población, cuantificar la variabilidad interindividual",
          "e identificar las características de los pacientes",
          "(covariables) que pueden explicar parte de dicha variabilidad.",
          "El análisis utiliza modelos no lineales de efectos mixtos,",
          "ajustados mediante el algoritmo de Lindstrom y Bates,",
          "implementado en el paquete nlme.",
          "El procedimiento se organiza en tres etapas:",
          "Datos, Análisis y Resultados."
        ),
        subsection(
          "pob_necesitas", "3.1", "Qué necesitas",
          shiny::tags$ul(
            item(
              "Un modelo farmacocinético",
              "seleccionado en la barra superior. Puedes utilizar",
              "cualquiera de los modelos disponibles en la aplicación,",
              "siempre que los datos proporcionados sean compatibles",
              "con el modelo seleccionado."
            ),
            item(
              "Un archivo CSV",
              "que contenga las concentraciones observadas de cada paciente,",
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
              "Pacientes:",
              "al menos 5 pacientes, con un mínimo de 3 concentraciones",
              "medidas por paciente."
            ),
            item(
              "Tiempo:",
              "expresado desde la administración de la dosis,",
              "sin valores negativos y en ",
              names_list(names(population_time_units)), "."
            ),
            item(
              "Concentración:",
              "sin valores negativos ni ausentes y expresada en ",
              names_list(names(population_conc_units)), "."
            ),
            item(
              "Dosis:",
              "se considera una única dosis administrada en t = 0.",
              "Su valor debe ser el mismo en todas las filas de cada",
              "paciente y expresarse en ",
              names_list(names(population_dose_units)), ".",
              "Si se utiliza mg/kg, la dosis se calcula a partir",
              "de una columna que contenga el peso del paciente en kg."
            ),
            item(
              "Covariables:",
              "deben cumplir los requisitos descritos en el apartado 3.3."
            )
          ),
          shiny::p(
            "Si el archivo CSV contiene columnas adicionales que",
            " no son necesarias para el análisis, como las columnas",
            " de eventos de NONMEM (",
            names_list(population_event_columns),
            ") o los índices de fila añadidos al exportar datos",
            " desde R o Python (rownames, X, etc.),",
            " la aplicación las ignora o elimina automáticamente."
          )
        ),

        subsection(
          "pob_covariables", "3.3", "Covariables",
          shiny::p(
            "Una covariable es una característica del paciente que puede",
            "explicar por qué sus parámetros difieren de los de otros",
            "pacientes, como el peso sobre el aclaramiento. Cualquier columna",
            "del CSV que no sea el paciente, el tiempo, la concentración o",
            "la dosis se revisa como posible covariable. Estas se reconocen",
            "por su nombre:"
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
            " Una misma covariable puede influir en varios parámetros:",
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
              "Covariable continua, forma de potencia (por defecto):",
              shiny::tags$code("θ · (cov / mediana)^β"),
              ". El efecto se calcula en relación con la mediana",
              " de la covariable. El coeficiente β determina cómo",
              " cambia el parámetro farmacocinético al variar",
              " la covariable. Es una relación habitual para describir",
              " el efecto del peso corporal, como en los modelos",
              " alométricos, donde suele utilizarse un exponente",
              " de 0,75 para el aclaramiento (CL)."
            ),
            item(
              "Covariable continua, forma lineal en escala logarítmica:",
              shiny::tags$code("θ · exp(β · (cov − mediana))"),
              ". El coeficiente β determina el cambio proporcional",
              " del parámetro por cada unidad de aumento de la",
              " covariable. Esta forma permite utilizar covariables",
              " con valores positivos, negativos o iguales a cero."
            ),
            item(
              "Covariable categórica:",
              shiny::tags$code("θ · exp(β)"),
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
              "Selecciona el modelo farmacocinético",
              "en la barra superior."
            ),
            item(
              "Carga el archivo CSV.",
              "La aplicación comprueba que los datos cumplan",
              " los requisitos del modelo seleccionado.",
              " Si detecta algún problema, muestra un mensaje",
              " indicando el motivo y solicita cargar otro archivo."
            ),
            item(
              "Revisa las columnas identificadas",
              "para el paciente, el tiempo de muestreo,",
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
              " elegido, se muestra un aviso en amarillo (apartado 3.7)."
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
            " Antes de ajustar el modelo poblacional, se recomienda",
            " ajustar los datos de cada paciente por separado mediante",
            " regresión no lineal (nlsList), sin efectos aleatorios.",
            " Estos ajustes permiten explorar las diferencias entre",
            " pacientes y obtener valores iniciales para el modelo mixto.",
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
              "Escala de los parámetros:",
              "permite elegir entre escala natural y logarítmica.",
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
              "Efectos aleatorios",
              "(«Parámetros que cambian entre pacientes»):",
              "permite seleccionar los parámetros farmacocinéticos",
              " que presentan variabilidad interindividual.",
              " Todos los parámetros estimados tienen un valor",
              " típico poblacional (efecto fijo), mientras que",
              " los seleccionados incorporan además un efecto",
              " aleatorio que representa las diferencias entre",
              " pacientes no explicadas por el modelo."
            ),

            item(
              "Matriz de varianzas y covarianzas (Ψ):",
              "cuando se seleccionan dos o más efectos aleatorios,",
              " permite definir su relación. La estructura diagonal",
              " (pdDiag) supone que los efectos aleatorios son",
              " independientes, mientras que la estructura completa",
              " (pdSymm) permite estimar también las covarianzas",
              " y correlaciones entre ellos."
            ),

            item(
              "Covariables:",
              "permite seleccionar qué características de los",
              " pacientes influyen sobre cada parámetro farmacocinético",
              " (por ejemplo, el efecto del peso sobre CL).",
              " Las covariables continuas se expresan en relación",
              " con su mediana y las categóricas se comparan",
              " con una categoría de referencia, como se explica",
              " en el apartado 3.3."
            ),

            item(
              "Error de medida (error residual):",
              "describe la variabilidad de las concentraciones",
              " observadas respecto a las predicciones individuales",
              " del modelo. Se puede elegir entre tres opciones:"
            )
          ),

          shiny::tags$ul(
            class = "pk-doc-sublist",
            item(
              "Aditivo:",
              "la desviación típica del error es constante (σ),",
              " independientemente de la concentración.",
              " Es adecuado cuando la magnitud del error es similar",
              " para concentraciones bajas y altas."
            ),

            item(
              "Proporcional:",
              "la desviación típica del error es proporcional",
              " al valor absoluto de la concentración predicha",
              " (σ · |predicción|). El error aumenta a medida",
              " que aumenta la concentración."
            ),

            item(
              "Combinado:",
              "la desviación típica del error se expresa como",
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
              "Salida medida:",
              "selecciona la variable de salida del modelo",
              " que corresponde a las concentraciones observadas",
              " en el archivo CSV (por defecto, CP o equivalente)."
            ),

            item(
              "Parámetros con valor fijado:",
              "permite mantener determinados parámetros constantes",
              " durante el ajuste, utilizando su valor inicial.",
              " En estos casos no se estima su valor poblacional",
              " ni se incorpora variabilidad interindividual.",
              " Esta opción resulta útil cuando se dispone de",
              " información previa fiable sobre sus valores."
            ),

            item(
              "Forma del efecto de las covariables continuas:",
              "permite elegir entre la forma de potencia",
              " y la lineal en escala logarítmica, descritas en el",
              " apartado 3.3."
            ),

            item(
              "Valores iniciales:",
              "establece el valor de partida de cada parámetro",
              " para el procedimiento de estimación, con sus",
              " unidades correspondientes. Por defecto, se",
              " utilizan los valores definidos en el modelo,",
              " aunque pueden sustituirse por las medianas",
              " obtenidas en los ajustes individuales."
            )
          ),

          shiny::p(
            "Al pulsar «Estimar parámetros», la aplicación ajusta",
            " el modelo no lineal de efectos mixtos mediante",
            " el algoritmo de Lindstrom y Bates, implementado",
            " en el paquete nlme. Este procedimiento alterna",
            " etapas de mínimos cuadrados no lineales penalizados",
            " (PNLS) y de ajuste de un modelo lineal mixto (LME)",
            " hasta alcanzar la convergencia.",
            " El tiempo de cálculo depende de la complejidad",
            " del modelo y del volumen de datos."
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
              "Efectos fijos:",
              "muestra los valores estimados de los parámetros",
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
              "Variabilidad:",
              "muestra la desviación típica de cada efecto aleatorio,",
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
              "Estimaciones de cada paciente:",
              "muestra los efectos aleatorios estimados para",
              " cada paciente (b_i), equivalentes a los obtenidos",
              " mediante ranef() de nlme, y los valores de sus",
              " parámetros farmacocinéticos individuales",
              " expresados en escala natural."
            ),

            item(
              "Resumen (summary de nlme):",
              "la salida de summary() de R para el modelo ajustado,",
              " que se puede descargar en TXT. Incluye la",
              " log-verosimilitud, el AIC y el BIC (el ajuste es por",
              " máxima verosimilitud, así que sirven para comparar",
              " modelos sobre los mismos datos: un valor menor indica",
              " un mejor equilibrio entre ajuste y complejidad), las",
              " desviaciones de los efectos aleatorios, los efectos",
              " fijos con su error estándar, su t y su p-valor, sus",
              " correlaciones y los residuos estandarizados. Los",
              " parámetros en escala logarítmica aparecen con una l",
              " delante (lCL, lKA...), como lKe, lKa y lCl en Pinheiro",
              " y Bates, y los efectos de las covariables como",
              " parámetro.covariable (CL.Wt, KA.SEX=M...)."
            )
          ),

          shiny::p(
            "Si el ajuste falla, en lugar de los resultados se muestra",
            " el registro del ajuste: la configuración, el código del",
            " modelo de mrgsolve, cada iteración del algoritmo (pasos",
            " LME y PNLS, con los efectos fijos y el criterio de",
            " convergencia), las llamadas a mrgsolve con parámetros no",
            " válidos (con sus valores), los avisos y el error final.",
            " Se puede descargar en TXT, y el botón de descarga de los",
            " resultados no aparece. Al ajustar cada paciente por separado,",
            " si falla, el registro aparece debajo del mensaje de",
            " error."
          ),

          shiny::p(
            "Las unidades de los parámetros farmacocinéticos",
            " se indican según su denominación:"
          ),

          units
        ),
        subsection(
          "pob_avisos", "3.7", "Avisos frecuentes",
          shiny::tags$dl(
            shiny::tags$dt(
              "Este CSV no contiene los datos necesarios para el modelo"
            ),
            shiny::tags$dd(
              "Falta alguna columna con un nombre reconocido (apartado 3.2)",
              "o hay pocos pacientes o medidas. El mensaje indica qué falta:",
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
              "La columna elegida como peso no sirve: tiene valores",
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
            "La aplicación no valora si lo elegido es adecuado: interpreta",
            "los resultados según tu criterio."
          )
        ),
        subsection(
          "pob_limites", "3.8", "Limitaciones",
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
          "En esta sección puedes consultar y gestionar los modelos",
          " farmacocinéticos que has guardado. Para cada modelo",
          " se muestra un resumen de sus características,",
          " su descripción y su estado de visibilidad",
          " (privado o disponible en el catálogo común)."
        ),
        shiny::tags$ul(
          item(
            "Editar:",
            "permite modificar el modelo mediante el formulario",
            " de creación por componentes. Al guardar los cambios,",
            " la aplicación vuelve a validar y compilar el modelo",
            " para comprobar que funciona correctamente."
          ),
          item(
            "Añadir al catálogo / Retirar del catálogo:",
            "permite compartir un modelo con el resto de usuarios",
            " o volver a hacerlo privado. Los modelos publicados",
            " en el catálogo común pueden ser utilizados por",
            " otros usuarios para realizar simulaciones y",
            " análisis farmacocinéticos poblacionales."
          ),
          item(
            "Eliminar:",
            "permite eliminar un modelo de tu cuenta."
          ),
          item(
            "Crear modelo nuevo:",
            "abre la sección «Crear modelo», donde puedes",
            " construir y guardar un nuevo modelo farmacocinético."
          )
        )
      )
    )
  )
}
