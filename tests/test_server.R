sys.source("app.R", envir = .GlobalEnv)

shiny::testServer(server, {
  fixture_path <- normalizePath("tests/fixture.csv")
  session$setInputs(file = data.frame(
    name = "fixture.csv",
    size = file.info(fixture_path)$size,
    type = "text/csv",
    datapath = fixture_path,
    stringsAsFactors = FALSE
  ))

  session$setInputs(add_filter_condition = 1)
  session$setInputs(
    filter_variable_1 = "score",
    filter_operator_1 = "greater_than",
    filter_value_1 = 70
  )

  session$setInputs(add_filter_condition = 2)
  session$setInputs(
    filter_variable_2 = "X",
    filter_operator_2 = "equals",
    filter_value_2 = 1,
    filter_join_2 = "OR"
  )

  session$setInputs(add_filter_condition = 3)
  session$setInputs(
    filter_variable_3 = "gender",
    filter_operator_3 = "equals",
    filter_value_3 = "female",
    filter_join_3 = "AND"
  )

  stopifnot(identical(as.numeric(general_filtered_data()$Y), c(1, 2, 6)))
  stopifnot(grepl(" | ", make_general_filter_code(uploaded_data(), filter_conditions()), fixed = TRUE))

  session$setInputs(data_preview_source = "filtered")
  stopifnot(grepl(
    "3 of 6 rows match",
    output$data_preview_status_ui$html,
    fixed = TRUE
  ))
})

cat("Server filter test passed.\n")
