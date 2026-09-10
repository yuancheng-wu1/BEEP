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
  filter_ui <- output$filter_conditions_ui$html
  stopifnot(grepl("value=\"80\"", filter_ui, fixed = TRUE))
  stopifnot(grepl("value=\"70\"", filter_ui, fixed = TRUE))

  session$setInputs(data_preview_source = "filtered")
  stopifnot(grepl(
    "3 of 6 rows match",
    output$data_preview_status_ui$html,
    fixed = TRUE
  ))

  session$setInputs(
    plot_type = "Line plot",
    x_var = "X",
    y_var = "Y",
    group_var = "gender",
    group_color = TRUE,
    group_shape = TRUE,
    group_linetype = TRUE,
    show_se = TRUE,
    x_as_factor = FALSE,
    plot_title = "",
    x_label_custom = "",
    y_label_custom = "",
    legend_title = "",
    x_min = NA_real_,
    x_max = NA_real_,
    y_min = NA_real_,
    y_max = NA_real_,
    plot_theme = "minimal"
  )
  styled_line_plot <- plot_object()
  stopifnot(inherits(styled_line_plot, "ggplot"))
  stopifnot(all(c("colour", "shape", "linetype") %in% names(styled_line_plot$mapping)))
  stopifnot(length(styled_line_plot$layers) == 3)

  session$setInputs(plot_type = "Bar plot", show_se = TRUE)
  bar_plot_with_se <- plot_object()
  stopifnot(inherits(bar_plot_with_se, "ggplot"))
  stopifnot(length(bar_plot_with_se$layers) == 2)

  session$setInputs(show_se = FALSE)
  bar_plot_without_se <- plot_object()
  stopifnot(length(bar_plot_without_se$layers) == 1)
})

cat("Server filter test passed.\n")
