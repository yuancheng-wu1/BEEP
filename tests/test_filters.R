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
missing_code <- make_general_filter_code(dat, missing_condition, data_name = "dat")
missing_code_result <- eval(parse(text = paste0("dat[", missing_code, ", , drop = FALSE]")))
stopifnot(identical(missing_code_result$Y, 5L))

contains_condition <- list(
  list(variable = "gender", operator = "contains", value = "fem", join = "AND")
)
stopifnot(identical(apply_general_filters(dat, contains_condition)$Y, c(2L, 4L, 6L)))

not_contains_condition <- list(
  list(variable = "gender", operator = "not_contains", value = "fem", join = "AND")
)
stopifnot(identical(apply_general_filters(dat, not_contains_condition)$Y, c(1L, 3L)))

stopifnot(identical(apply_general_filters(dat, list()), dat))

filter_code <- make_general_filter_code(dat, conditions, data_name = "dat")
code_result <- eval(parse(text = paste0("dat[", filter_code, ", , drop = FALSE]")))
stopifnot(identical(code_result, filtered))

cat("All filter tests passed.\n")
