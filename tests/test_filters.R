source("plot_code.R")

dat <- data.frame(
  Y = 1:6,
  X = c(0, 1, 1, 0, 1, NA),
  score = c(80, 60, 65, 40, NA, 90),
  gender = c("male", "female", "male", "female", NA, "female"),
  stringsAsFactors = FALSE
)

conditions <- list(
  list(variable = "score", operator = "greater_than", value = 70, join = "AND"),
  list(variable = "X", operator = "equals", value = 1, join = "OR"),
  list(variable = "gender", operator = "equals", value = "female", join = "AND")
)

# AND precedes OR: score > 70 OR (X == 1 AND gender == "female").
filtered <- apply_general_filters(dat, conditions)
stopifnot(identical(filtered$Y, c(1L, 2L, 6L)))

missing_condition <- list(
  list(variable = "gender", operator = "is_missing", value = NULL, join = "AND")
)
stopifnot(identical(apply_general_filters(dat, missing_condition)$Y, 5L))
missing_code <- make_general_filter_code(dat, missing_condition, data_name = ".data")
missing_code_result <- eval(parse(text = paste0("dplyr::filter(dat, ", missing_code, ")")))
stopifnot(identical(missing_code_result$Y, 5L))

contains_condition <- list(
  list(variable = "gender", operator = "contains", value = "fem", join = "AND")
)
stopifnot(identical(apply_general_filters(dat, contains_condition)$Y, c(2L, 4L, 6L)))

not_contains_condition <- list(
  list(variable = "gender", operator = "not_contains", value = "fem", join = "AND")
)
stopifnot(identical(apply_general_filters(dat, not_contains_condition)$Y, c(1L, 3L)))

inclusive_conditions <- list(
  list(variable = "score", operator = "greater_equal", value = 65, join = "AND"),
  list(variable = "score", operator = "less_equal", value = 80, join = "AND")
)
stopifnot(identical(apply_general_filters(dat, inclusive_conditions)$Y, c(1L, 3L)))

numeric_operator_labels <- names(filter_operator_choices(dat$score))
stopifnot(all(c("equals", "does not equal", ">", ">=", "<", "<=") %in% numeric_operator_labels))

stopifnot(identical(apply_general_filters(dat, list()), dat))

filter_code <- make_general_filter_code(dat, conditions, data_name = ".data")
code_result <- eval(parse(text = paste0("dplyr::filter(dat, ", filter_code, ")")))
stopifnot(identical(code_result$Y, filtered$Y))

plot_input <- list(
  x_var = "X",
  y_var = "Y",
  group_var = "None",
  facet_var = "None",
  x_as_factor = FALSE,
  exclude_x_levels = character(0),
  exclude_groups = character(0),
  exclude_facets = character(0),
  legend_title = "",
  plot_theme = "minimal",
  plot_type = "Scatterplot",
  add_smooth = FALSE,
  plot_title = "",
  x_label_custom = "",
  y_label_custom = "",
  x_min = NA_real_,
  x_max = NA_real_,
  y_min = NA_real_,
  y_max = NA_real_,
  data_object = "dat",
  file = list(name = "fixture.csv")
)

plot_code <- make_plot_code(plot_input, dat, conditions)
stopifnot(grepl("plot_data <- plot_data %>%", plot_code, fixed = TRUE))
stopifnot(grepl("  filter(", plot_code, fixed = TRUE))
stopifnot(!grepl("!is.na(.data[[\"score\"]])", plot_code, fixed = TRUE))

plot_environment <- new.env(parent = globalenv())
plot_environment$dat <- dat
generated_plot <- eval(parse(text = plot_code), envir = plot_environment)
stopifnot(inherits(generated_plot, "ggplot"))

cat("All filter tests passed.\n")
