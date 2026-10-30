# ============================================================
# Archivo: test-integration-server.R
# Autor: Isabel Morro Tabares
# Proyecto: TFG – PKLaboratory
#
# Descripción:
# Pruebas de integración del servidor de Shiny con shiny::testServer(): se
# ejecuta server() completo, con todos sus controladores, y se simula al
# usuario cambiando los controles. Así se prueba la comunicación entre la
# interfaz, los controladores y mrgsolve (CU-1, CU-2, CU-3, CU-6 y CU-11)
# sin abrir un navegador. No hay base de datos (database_pool = NULL).
# ============================================================

# Entra en la aplicación sin pasar por el formulario de acceso (que
# necesita la base de datos), con start_session(), que es lo que hace el
# servidor tras comprobar la contraseña. Se evalúa dentro de testServer().
# Recibe: entorno del servidor (por defecto, el de quien la llama).
log_in <- function(env = parent.frame()) {
  evalq({
    session$setInputs(resume_token = "")
    start_session(list(username = "test", full_name = "Test"))
    session$flushReact()
  }, env)
}

# Configura la simulación del ejemplo de pk1 de la guía de uso.
set_pk1 <- function(session, ...) {
  inputs <- utils::modifyList(
    list(model_id = "pk1", param_CL = 1, param_V = 20, param_KA = 1,
         dose_cmt = "EV", dose = 100, n_doses = 10, interval = 24,
         end_time = 480, infusion = 0, plot_vars = "CP"),
    list(...)
  )
  do.call(session$setInputs, inputs)
}

test_that("sin sesión solo se muestra el formulario de acceso", {
  testServer(server, {
    session$setInputs(resume_token = "")
    page <- output$app_page$html
    expect_match(page, "login_submit")
    expect_no_match(page, "simulate")
  })
})

test_that("sin base de datos no se puede iniciar sesión", {
  testServer(server, {
    session$setInputs(resume_token = "")
    session$setInputs(username = "User", password = "TFG_2026",
                      login_submit = 1)
    expect_false(logged_in())
    expect_null(current_user())
  })
})

test_that("un token guardado no válido no da acceso", {
  testServer(server, {
    session$setInputs(resume_token = strrep("a", 64))
    expect_false(logged_in())
    expect_true(resume_checked())
  })
})

test_that("tras iniciar sesión se muestra la aplicación", {
  testServer(server, {
    log_in()
    expect_match(output$app_page$html, "simulate")
  })
})

test_that("al pulsar Simular se obtienen los mismos datos que con mrgsolve", {
  testServer(server, {
    log_in()
    set_pk1(session)
    session$setInputs(simulate = 1)

    expected <- simulate_model(load_model("pk1"),
                               list(CL = 1, V = 20, KA = 1),
                               100, "EV", 24, 10, 480)
    expect_equal(simulation(), expected)
    expect_null(output$sim_errors)
    # La tabla muestra solo las 150 primeras filas.
    expect_equal(lengths(regmatches(output$results,
                                    gregexpr("<tr", output$results))),
                 150 + 1)
  })
})

test_that("cambiar un parámetro no simula hasta pulsar el botón", {
  testServer(server, {
    log_in()
    set_pk1(session)
    session$setInputs(simulate = 1)
    before <- simulation()
    session$setInputs(param_CL = 5)
    expect_identical(simulation(), before)
    session$setInputs(simulate = 2)
    expect_false(identical(simulation(), before))
  })
})

test_that("con datos no válidos se muestran los errores y no hay resultados", {
  testServer(server, {
    log_in()
    set_pk1(session, dose = 0, param_CL = -1)
    session$setInputs(simulate = 1)
    errors <- output$sim_errors$html
    expect_match(errors, "dosis")
    expect_match(errors, "CL")
    expect_error(simulation(), class = "shiny.silent.error")
  })
})

test_that("el formulario de parámetros cambia con el modelo elegido", {
  testServer(server, {
    log_in()
    session$setInputs(model_id = "pk2")
    form <- output$param_inputs$html
    for (p in names(default_params("pk2"))) {
      expect_match(form, paste0("param_", p))
    }
  })
})

test_that("las descargas CSV contienen todos los resultados", {
  testServer(server, {
    log_in()
    set_pk1(session)
    session$setInputs(simulate = 1)

    csv <- utils::read.csv(output$download_csv)
    expect_equal(nrow(csv), nrow(simulation()))
    excel <- utils::read.csv2(output$download_csv_excel)
    expect_equal(excel$CP, simulation()$CP, tolerance = 1e-12)
    expect_match(readLines(output$download_csv_excel, n = 1), ";")
  })
})

test_that("un modelo creado se compila y se puede simular (sin base: sesión)", {
  testServer(server, {
    log_in()
    code <- paste(
      "$PARAM CL = 1, V = 20, KA = 1",
      "$CMT DEPOT CENT",
      "$ODE",
      "dxdt_DEPOT = -KA * DEPOT;",
      "dxdt_CENT = KA * DEPOT - CL / V * CENT;",
      "$TABLE",
      "capture CP = CENT / V;",
      sep = "\n"
    )
    session$setInputs(bld_name = "Oral", bld_description = "",
                      bld_code_text = code)
    session$setInputs(bld_add = 1)

    id <- names(user_models())
    expect_equal(id, "tmp_1")
    expect_false(user_models()[[id]]$saved)
    expect_equal(selected_model_id(), id)

    session$setInputs(model_id = id, param_CL = 1, param_V = 20,
                      param_KA = 1, dose_cmt = "DEPOT", dose = 100,
                      n_doses = 1, interval = 0, end_time = 24,
                      infusion = 0, simulate = 1)
    expect_true(all(is.finite(simulation()$CP)))

    # Mismo nombre: se rechaza (RF-2.6).
    session$setInputs(bld_add = 2)
    expect_length(user_models(), 1)

    # CU-6: eliminar un modelo de la sesión.
    session$setInputs(model_delete = id)
    expect_length(user_models(), 0)
    expect_equal(selected_model_id(), default_model_id)
  })
})

test_that("al cerrar sesión no queda nada del usuario", {
  testServer(server, {
    log_in()
    user_models(list(tmp_1 = list(label = "x")))
    assign("db_1", list(), envir = shared_cache)
    session$setInputs(logout = 1)

    expect_false(logged_in())
    expect_null(current_user())
    expect_length(user_models(), 0)
    expect_length(ls(shared_cache), 0)
    expect_equal(selected_model_id(), default_model_id)
    expect_match(output$app_page$html, "login_submit")
  })
})

test_that("editar un modelo cambia el mismo modelo sin duplicarlo (CU-5)", {
  testServer(server, {
    log_in()
    session$setInputs(bld_name = "Oral", bld_description = "",
                      bld_code_text = paste(
                        "$PARAM CL = 1, V = 20, KA = 1",
                        "$CMT DEPOT CENT",
                        "$ODE",
                        "dxdt_DEPOT = -KA * DEPOT;",
                        "dxdt_CENT = KA * DEPOT - CL / V * CENT;",
                        "$TABLE",
                        "capture CP = CENT / V;",
                        sep = "\n"
                      ))
    session$setInputs(bld_add = 1)
    id <- names(user_models())

    # Se abre el editor; sus campos llevan un prefijo nuevo en cada apertura.
    session$setInputs(model_edit = id)
    ns <- "edit1_"
    new_code <- sub("CL = 1", "CL = 4", user_models()[[id]]$spec$code)

    # Código no admitido: se rechaza y el modelo no cambia.
    inputs <- list(name = "Oral", description = "",
                   code_text = paste(new_code, "$ENV x <- 1", sep = "\n"))
    do.call(session$setInputs,
            stats::setNames(inputs, paste0(ns, names(inputs))))
    session$setInputs(edit_save = 1)
    expect_match(output$edit_errors$html, "ENV")
    expect_equal(get_model_params(user_models()[[id]]$mod)$CL, 1)

    # Código correcto: se guarda en el mismo modelo.
    session$setInputs(edit1_code_text = new_code, edit1_name = "Oral IV")
    session$setInputs(edit_save = 2)
    expect_equal(names(user_models()), id)
    expect_equal(user_models()[[id]]$label, "Oral IV")
    expect_equal(get_model_params(user_models()[[id]]$mod)$CL, 4)
    expect_null(output$edit_errors)
  })
})

test_that("el asistente genera el código y lo lleva al editor (RF-2.3)", {
  sent <- list()
  assign("updateTextAreaInput", function(session, inputId, value, ...) {
    sent[[inputId]] <<- value
  }, envir = globalenv())
  withr::defer(rm("updateTextAreaInput", envir = globalenv()))

  testServer(server, {
    log_in()
    session$setInputs(bld_wizard = 1)
    session$setInputs(
      wiz_params = "KA = 1\nCL = 5\nV = 50",
      wiz_initials = "DEPOT = 0\nCENT = 0",
      wiz_auxiliary = "KE = CL / V",
      wiz_equations = "DEPOT = -KA * DEPOT\nCENT = KA * DEPOT - KE * CENT",
      wiz_outputs = "CENT, KE"
    )
    session$setInputs(wiz_generate = 1)
    expect_null(output$wiz_errors)
    expect_match(sent$bld_code_text, "dxdt_CENT")
    expect_length(validate_model_code(sent$bld_code_text), 0)

    # Una fila mal escrita no genera código y se explica el error.
    sent$bld_code_text <<- NULL
    session$setInputs(wiz_initials = "DEPOT 0", wiz_generate = 2)
    expect_match(output$wiz_errors$html, "nombre = valor")
    expect_null(sent$bld_code_text)
  })
})

test_that("se puede cargar el código desde un archivo .cpp (RF-2.2)", {
  sent <- list()
  assign("updateTextAreaInput", function(session, inputId, value, ...) {
    sent[[inputId]] <<- value
  }, envir = globalenv())
  withr::defer(rm("updateTextAreaInput", envir = globalenv()))

  path <- tempfile(fileext = ".cpp")
  writeLines(c("$PARAM K = 1", "$CMT A", "$ODE", "dxdt_A = -K * A;"), path)

  testServer(server, {
    log_in()
    session$setInputs(bld_code_file = data.frame(
      name = "Eliminacion.cpp", size = file.size(path), type = "",
      datapath = path
    ))
    expect_match(sent$bld_code_text, "dxdt_A")
    expect_null(output$bld_errors)
  })
})
