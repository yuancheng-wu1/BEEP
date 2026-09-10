`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

r_column_name <- function(x) {
  paste(deparse(as.name(x), backtick = TRUE), collapse = "")
}

filter_operator_choices <- function(column) {
  missing_choices <- c(
    "is missing" = "is_missing",
    "is not missing" = "not_missing"
  )

  if (is.numeric(column) || inherits(column, c("Date", "POSIXt"))) {
    return(c(
      "equals" = "equals",
      "does not equal" = "not_equals",
      ">" = "greater_than",
      ">=" = "greater_equal",
      "<" = "less_than",
      "<=" = "less_equal",
      missing_choices
    ))
  }

  c(
    "equals" = "equals",
    "does not equal" = "not_equals",
    "contains" = "contains",
    "does not contain" = "not_contains",
    "starts with" = "starts_with",
    "ends with" = "ends_with",
    missing_choices
  )
}

evaluate_filter_condition <- function(dat, condition) {
  variable <- condition$variable %||% ""
  operator <- condition$operator %||% ""

  if (!nzchar(variable) || !variable %in% names(dat) || !nzchar(operator)) {
    return(NULL)
  }

  column <- dat[[variable]]
  missing_values <- is.na(column)

  if (operator == "is_missing") {
    return(is.na(column))
  }

  if (operator == "not_missing") {
    return(!is.na(column))
  }

  value <- condition$value
  if (is.null(value) || length(value) == 0 || is.na(value[1]) ||
      !nzchar(trimws(as.character(value[1])))) {
    return(NULL)
  }

  if (is.numeric(column)) {
    comparison_value <- suppressWarnings(as.numeric(value[1]))
    if (is.na(comparison_value)) {
      return(NULL)
    }
  } else if (inherits(column, "Date")) {
    comparison_value <- suppressWarnings(as.Date(value[1]))
    if (is.na(comparison_value)) {
      return(NULL)
    }
  } else if (inherits(column, "POSIXt")) {
    comparison_value <- suppressWarnings(as.POSIXct(value[1]))
    if (is.na(comparison_value)) {
      return(NULL)
    }
  } else {
    column <- as.character(column)
    comparison_value <- as.character(value[1])
  }

  result <- switch(
    operator,
    "equals" = column == comparison_value,
    "not_equals" = column != comparison_value,
    "greater_than" = column > comparison_value,
    "greater_equal" = column >= comparison_value,
    "less_than" = column < comparison_value,
    "less_equal" = column <= comparison_value,
    "contains" = grepl(comparison_value, column, fixed = TRUE),
    "not_contains" = !grepl(comparison_value, column, fixed = TRUE),
    "starts_with" = startsWith(column, comparison_value),
    "ends_with" = endsWith(column, comparison_value),
    NULL
  )

  if (is.null(result)) {
    return(NULL)
  }

  result[is.na(result)] <- FALSE
  result[missing_values] <- FALSE
  result
}

apply_general_filters <- function(dat, conditions = list()) {
  if (length(conditions) == 0) {
    return(dat)
  }

  valid_conditions <- lapply(conditions, function(condition) {
    condition$result <- evaluate_filter_condition(dat, condition)
    condition
  })
  valid_conditions <- Filter(function(condition) !is.null(condition$result), valid_conditions)

  if (length(valid_conditions) == 0) {
    return(dat)
  }

  # AND binds more tightly than OR: build AND groups, then OR their results.
  group_results <- list(valid_conditions[[1]]$result)

  if (length(valid_conditions) > 1) {
    for (i in 2:length(valid_conditions)) {
      join <- toupper(valid_conditions[[i]]$join %||% "AND")

      if (join == "OR") {
        group_results[[length(group_results) + 1]] <- valid_conditions[[i]]$result
      } else {
        last_group <- length(group_results)
        group_results[[last_group]] <-
          group_results[[last_group]] & valid_conditions[[i]]$result
      }
    }
  }

  keep <- Reduce(`|`, group_results)
  dat[keep, , drop = FALSE]
}

make_general_filter_code <- function(dat, conditions = list(), data_name = NULL) {
  quote_r <- function(x) paste(deparse(as.character(x)), collapse = "")

  condition_code <- lapply(conditions, function(condition) {
    if (is.null(evaluate_filter_condition(dat, condition))) {
      return(NULL)
    }

    variable <- condition$variable
    operator <- condition$operator
    column <- dat[[variable]]
    column_code <- if (is.null(data_name)) {
      r_column_name(variable)
    } else {
      paste0(data_name, "[[", quote_r(variable), "]]")
    }
    join <- toupper(condition$join %||% "AND")

    if (operator == "is_missing") {
      return(list(
        expression = paste0("is.na(", column_code, ")"),
        join = join
      ))
    }
    if (operator == "not_missing") {
      return(list(
        expression = paste0("!is.na(", column_code, ")"),
        join = join
      ))
    }

    raw_value <- condition$value[1]
    value_code <- if (is.numeric(column)) {
      format(as.numeric(raw_value), trim = TRUE, scientific = FALSE)
    } else if (inherits(column, "Date")) {
      paste0("as.Date(", quote_r(raw_value), ")")
    } else if (inherits(column, "POSIXt")) {
      paste0("as.POSIXct(", quote_r(raw_value), ")")
    } else {
      quote_r(raw_value)
    }

    expression <- switch(
      operator,
      "equals" = paste(column_code, "==", value_code),
      "not_equals" = paste(column_code, "!=", value_code),
      "greater_than" = paste(column_code, ">", value_code),
      "greater_equal" = paste(column_code, ">=", value_code),
      "less_than" = paste(column_code, "<", value_code),
      "less_equal" = paste(column_code, "<=", value_code),
      "contains" = paste0("grepl(", value_code, ", as.character(", column_code, "), fixed = TRUE)"),
      "not_contains" = paste0(
        "ifelse(is.na(", column_code, "), NA, !grepl(", value_code,
        ", as.character(", column_code, "), fixed = TRUE))"
      ),
      "starts_with" = paste0("startsWith(as.character(", column_code, "), ", value_code, ")"),
      "ends_with" = paste0("endsWith(as.character(", column_code, "), ", value_code, ")"),
      NULL
    )

    if (is.null(expression)) {
      return(NULL)
    }

    list(
      expression = expression,
      join = join
    )
  })
  condition_code <- Filter(Negate(is.null), condition_code)

  if (length(condition_code) == 0) {
    return("")
  }

  groups <- list(condition_code[[1]]$expression)
  if (length(condition_code) > 1) {
    for (i in 2:length(condition_code)) {
      if (condition_code[[i]]$join == "OR") {
        groups[[length(groups) + 1]] <- condition_code[[i]]$expression
      } else {
        last_group <- length(groups)
        groups[[last_group]] <- paste(
          groups[[last_group]],
          condition_code[[i]]$expression,
          sep = " & "
        )
      }
    }
  }

  paste(vapply(groups, function(group) paste0("(", group, ")"), character(1)), collapse = " | ")
}

filter_plot_data <- function(input, dat, conditions = list()) {
  dat <- apply_general_filters(dat, conditions)
  x_var <- input$x_var %||% "None"
  group_var <- input$group_var %||% "None"
  facet_var <- input$facet_var %||% "None"

  has_x <- x_var != "None" && x_var %in% names(dat)
  has_group <- group_var != "None" && group_var %in% names(dat)
  has_facet <- facet_var != "None" && facet_var %in% names(dat)

  exclude_x_levels <- input$exclude_x_levels %||% character(0)
  exclude_groups <- input$exclude_groups %||% character(0)
  exclude_facets <- input$exclude_facets %||% character(0)

  if (has_x && isTRUE(input$x_as_factor) && length(exclude_x_levels) > 0) {
    dat <- dat[
      !as.character(dat[[x_var]]) %in% exclude_x_levels,
      ,
      drop = FALSE
    ]
  }

  if (has_group && length(exclude_groups) > 0) {
    dat <- dat[
      !as.character(dat[[group_var]]) %in% exclude_groups,
      ,
      drop = FALSE
    ]
  }

  if (has_facet && length(exclude_facets) > 0) {
    dat <- dat[
      !as.character(dat[[facet_var]]) %in% exclude_facets,
      ,
      drop = FALSE
    ]
  }

  dat
}

make_plot_code <- function(input, dat, conditions = list()) {
  
  x_var <- input$x_var %||% "None"
  y_var <- input$y_var %||% "None"
  group_var <- input$group_var %||% "None"
  facet_var <- input$facet_var %||% "None"
  
  has_x <- x_var != "None" && x_var %in% names(dat)
  has_y <- y_var != "None" && y_var %in% names(dat)
  has_group <- group_var != "None" && group_var %in% names(dat)
  supports_group_point_styles <- input$plot_type %in% c("Scatterplot", "Line plot")
  use_group_color <- has_group && supports_group_point_styles &&
    isTRUE(input$group_color)
  use_group_shape <- has_group && supports_group_point_styles &&
    isTRUE(input$group_shape)
  use_group_linetype <- has_group && input$plot_type == "Line plot" &&
    isTRUE(input$group_linetype)
  
  legend_title <- trimws(input$legend_title %||% "")
  if (has_group && !nzchar(legend_title)) {
    legend_title <- group_var
  }
  plot_theme <- input$plot_theme %||% "minimal"
  
  has_facet <- facet_var != "None" && facet_var %in% names(dat)
  exclude_x_levels <- input$exclude_x_levels %||% character(0)
  exclude_groups <- input$exclude_groups %||% character(0)
  exclude_facets <- input$exclude_facets %||% character(0)
  filtered_dat <- filter_plot_data(input, dat, conditions)

  x_is_discrete <- has_x &&
    (
      isTRUE(input$x_as_factor) ||
        is.factor(dat[[x_var]]) ||
        is.character(dat[[x_var]]) ||
        is.logical(dat[[x_var]])
    )

  x_levels <- character(0)
  edited_x_labels <- character(0)

  if (x_is_discrete) {
    x_values <- filtered_dat[[x_var]]

    if (isTRUE(input$x_as_factor) || is.factor(x_values)) {
      x_levels <- levels(droplevels(factor(x_values)))
    } else {
      x_levels <- sort(unique(as.character(x_values)))
      x_levels <- x_levels[!is.na(x_levels)]
    }

    edited_x_labels <- vapply(
      seq_along(x_levels),
      function(i) {
        new_label <- input[[paste0("x_label_", i)]]

        if (is.null(new_label) || !nzchar(trimws(new_label))) {
          x_levels[i]
        } else {
          trimws(new_label)
        }
      },
      character(1)
    )
  }

  group_levels <- character(0)
  edited_group_labels <- character(0)

  if (has_group) {
  
    group_levels <- unique(as.character(filtered_dat[[group_var]]))
    group_levels <- group_levels[!is.na(group_levels)]
  
    edited_group_labels <- vapply(
      seq_along(group_levels),
      function(i) {
  
        new_label <- input[[paste0("group_label_", i)]]
  
        if (
          is.null(new_label) ||
          !nzchar(trimws(new_label))
        ) {
          group_levels[i]
        } else {
          trimws(new_label)
        }
      },
      character(1)
    )
  }

  facet_levels <- character(0)
  edited_facet_labels <- character(0)
  
  if (has_facet) {
  
    facet_levels <- unique(
      as.character(filtered_dat[[facet_var]])
    )
  
    facet_levels <- facet_levels[!is.na(facet_levels)]
  
    edited_facet_labels <- vapply(
      seq_along(facet_levels),
      function(i) {
  
        new_label <- input[[paste0("facet_label_", i)]]
  
        if (
          is.null(new_label) ||
          !nzchar(trimws(new_label))
        ) {
          facet_levels[i]
        } else {
          trimws(new_label)
        }
      },
      character(1)
    )
  }

  
  
  if (!has_x && !has_y) {
    return("# Select at least one X or Y variable.")
  }

  if (nrow(filtered_dat) == 0) {
    return("# No observations remain after applying the filters and exclusions.")
  }
  
  quote_r <- function(x) {
    paste(deparse(as.character(x)), collapse = "")
  }
  
  data_name <- input$data_object %||% "dat"
  
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", data_name)) {
    data_name <- "dat"
  }
  
  lines <- c(
    "library(ggplot2)",
    "library(dplyr)",
    ""
  )

  file_name <- input$file$name %||% ""
  is_excel_file <- tolower(tools::file_ext(file_name)) %in% c("xlsx", "xls")
  selected_sheet <- input$excel_sheet %||% ""

  if (is_excel_file && nzchar(selected_sheet)) {
    lines <- c(
      lines,
      paste0("# Excel sheet selected in BEEP: ", quote_r(selected_sheet)),
      ""
    )
  }

  general_filter_code <- make_general_filter_code(dat, conditions)
  if (nzchar(general_filter_code)) {
    lines <- c(
      lines,
      paste0("plot_data <- ", data_name, " %>%"),
      "  filter(",
      paste0("    ", general_filter_code),
      "  )"
    )
  } else {
    lines <- c(lines, paste0("plot_data <- ", data_name))
  }
  
  if (has_x &&
      isTRUE(input$x_as_factor) &&
      length(exclude_x_levels) > 0) {

    excluded_x_text <- paste(
      vapply(exclude_x_levels, quote_r, character(1)),
      collapse = ", "
    )
    
    lines <- c(
      lines,
      "plot_data <- plot_data %>%",
      paste0(
        "  filter(!as.character(", r_column_name(x_var), ") %in% c(",
        excluded_x_text, "))"
      )
    )
  }
  
  if (has_group && length(exclude_groups) > 0) {
    
    excluded_group_text <- paste(
      vapply(exclude_groups, quote_r, character(1)),
      collapse = ", "
    )
    
    lines <- c(
      lines,
      "plot_data <- plot_data %>%",
      paste0(
        "  filter(!as.character(", r_column_name(group_var), ") %in% c(",
        excluded_group_text, "))"
      )
    )
  }

  if (has_facet && length(exclude_facets) > 0) {
  
    excluded_facet_text <- paste(
      vapply(
        exclude_facets,
        quote_r,
        character(1)
      ),
      collapse = ", "
    )
    
    lines <- c(
      lines,
      "plot_data <- plot_data %>%",
      paste0(
        "  filter(!as.character(", r_column_name(facet_var), ") %in% c(",
        excluded_facet_text, "))"
      )
    )
  }

  lines <- c(lines, "")
 
  # ----- determine X expression -----
  
  if (has_x) {
    x_name <- r_column_name(x_var)
    x_expr <- if (isTRUE(input$x_as_factor)) {
      paste0("factor(", x_name, ")")
    } else {
      x_name
    }
    
  } else if (input$plot_type == "Boxplot") {
    x_expr <- '"All data"'
    
  } else if (input$plot_type %in% c("Scatterplot", "Line plot")) {
    x_expr <- "seq_len(nrow(plot_data))"
    
  } else {
    x_expr <- r_column_name(y_var)
  }
  
  y_expr <- if (has_y) r_column_name(y_var) else "NULL"
  group_expr <- if (has_group) {
    paste0("factor(", r_column_name(group_var), ")")
  } else {
    "NULL"
  }
  
  # ----- define aesthetics -----
  
  if (input$plot_type == "Histogram") {
    
    aes_parts <- paste0("x = ", x_expr)
    
    if (has_group) {
      aes_parts <- c(aes_parts, paste0("fill = ", group_expr))
    }
    
  } else if (input$plot_type == "Bar plot") {
    
    aes_parts <- c(
      paste0("x = ", x_expr),
      paste0("y = ", y_expr)
    )
    
    if (has_group) {
      aes_parts <- c(aes_parts, paste0("fill = ", group_expr))
    }
    
  } else {
    
    aes_parts <- c(
      paste0("x = ", x_expr),
      paste0("y = ", y_expr)
    )
    
    if (has_group && input$plot_type == "Boxplot") {
      aes_parts <- c(aes_parts, paste0("color = ", group_expr))
    } else if (use_group_color) {
      aes_parts <- c(aes_parts, paste0("color = ", group_expr))
    }

    if (use_group_shape && input$plot_type %in% c("Scatterplot", "Line plot")) {
      aes_parts <- c(aes_parts, paste0("shape = ", group_expr))
    }

    if (use_group_linetype && input$plot_type == "Line plot") {
      aes_parts <- c(aes_parts, paste0("linetype = ", group_expr))
    }
    
    if (input$plot_type == "Boxplot" && has_group) {
      aes_parts <- c(
        aes_parts,
        paste0(
          "group = interaction(",
          x_expr,
          ", ",
          group_expr,
          ", drop = TRUE)"
        )
      )
    }

    if (input$plot_type == "Line plot" && has_group) {
      aes_parts <- c(
        aes_parts,
        paste0("group = ", group_expr)
      )
    } else if (input$plot_type == "Line plot") {
      aes_parts <- c(aes_parts, "group = 1")
    }
  }
  
  plot_call <- paste0(
    "ggplot(plot_data, aes(",
    paste(aes_parts, collapse = ", "),
    "))"
  )
  plot_components <- list()
  
  # ----- add plot layers -----
  
  if (input$plot_type == "Histogram") {

    histogram_is_numeric <- if (has_x) {
      !isTRUE(input$x_as_factor) && is.numeric(filtered_dat[[x_var]])
    } else {
      has_y && is.numeric(filtered_dat[[y_var]])
    }

    if (!histogram_is_numeric) {
      return("# Histogram requires a numeric X or Y variable.")
    }
    
    if (has_group) {
      plot_components <- c(
        plot_components,
        list('geom_histogram(bins = 30, alpha = 0.6, position = "identity")')
      )
    } else {
      plot_components <- c(plot_components, list("geom_histogram(bins = 30)"))
    }
    
  } else if (input$plot_type == "Bar plot") {
    if (!has_x) {
      return("# Bar plot requires an X variable.")
    }
    
    if (!has_y) {
      return("# Bar plot requires a numeric Y variable.")
    }

    if (!is.numeric(filtered_dat[[y_var]])) {
      return("# Bar plot requires a numeric Y variable.")
    }
    
    if (has_group) {
      plot_components <- c(
        plot_components,
        list(c(
        "stat_summary(",
        "  fun = mean,",
        "  geom = 'col',",
        "  position = position_dodge(width = 0.9),",
        "  na.rm = TRUE",
        ")"
        ))
      )
    } else {
      plot_components <- c(
        plot_components,
        list(c(
        "stat_summary(",
        "  fun = mean,",
        "  geom = 'col',",
        "  na.rm = TRUE",
        ")"
        ))
      )
    }

    if (isTRUE(input$show_se)) {
      errorbar_lines <- c(
        "stat_summary(",
        "  fun.data = mean_se,",
        "  geom = 'errorbar',",
        "  width = 0.2,"
      )
      if (has_group) {
        errorbar_lines <- c(
          errorbar_lines,
          "  position = position_dodge(width = 0.9),"
        )
      }
      plot_components <- c(
        plot_components,
        list(c(errorbar_lines, "  na.rm = TRUE", ")"))
      )
    }
    
    
  } else if (input$plot_type == "Boxplot") {
    
    if (!has_y) {
      return("# Boxplot requires a Y variable.")
    }
    
    if (has_group) {
      plot_components <- c(
        plot_components,
        list("geom_boxplot()"),
        list(c(
          "geom_point(",
          "  position = position_jitterdodge(",
          "    jitter.width = 0.08,",
          "    seed = 123",
          "  ),",
          "  size = 1.5, alpha = 0.3",
          ")"
        ))
      )
    } else {
      plot_components <- c(
        plot_components,
        list("geom_boxplot()"),
        list(c(
          "geom_point(",
          "  position = position_jitter(width = 0.08, seed = 123),",
          "  size = 1.5, alpha = 0.3",
          ")"
        ))
      )
    }
    
  } else if (input$plot_type == "Scatterplot") {
    
    if (!has_y) {
      return("# Scatterplot requires a Y variable.")
    }
    
    plot_components <- c(plot_components, list("geom_point()"))
    
    if (isTRUE(input$add_smooth)) {
      plot_components <- c(
        plot_components,
        list('geom_smooth(method = "lm", se = TRUE)')
      )
    }
    
  } else if (input$plot_type == "Line plot") {
    
    if (!has_y) {
      return("# Line plot requires a Y variable.")
    }
    
    plot_components <- c(
      plot_components,
      list("stat_summary(fun = mean, geom = 'line', na.rm = TRUE)"),
      list("stat_summary(fun = mean, geom = 'point', na.rm = TRUE)")
    )

    if (isTRUE(input$show_se)) {
      plot_components <- c(
        plot_components,
        list(c(
        "stat_summary(",
        "  fun.data = mean_se,",
        "  geom = 'errorbar',",
        "  width = 0.1,",
        "  na.rm = TRUE",
        ")"
        ))
      )
    }
  }
  
  
  # ----- facet -----

if (has_facet) {

  facet_labels_changed <- !identical(facet_levels, edited_facet_labels)

  if (length(facet_levels) > 0 && facet_labels_changed) {

    facet_breaks_code <- paste(
      vapply(
        facet_levels,
        quote_r,
        character(1)
      ),
      collapse = ", "
    )

    facet_labels_code <- paste(
      vapply(
        edited_facet_labels,
        quote_r,
        character(1)
      ),
      collapse = ", "
    )

    lines <- c(
      lines,
      paste0(
        "facet_labels <- stats::setNames(",
        "c(", facet_labels_code, "), ",
        "c(", facet_breaks_code, ")",
        ")"
      )
    )
    plot_components <- c(
      plot_components,
      list(c(
        "facet_wrap(",
        paste0("  vars(", r_column_name(facet_var), "),"),
        "  labeller = as_labeller(facet_labels)",
        ")"
      ))
    )

  } else {

    plot_components <- c(
      plot_components,
      list(paste0("facet_wrap(vars(", r_column_name(facet_var), "))"))
    )
  }
}
  
  # ----- axis limits -----
  
  x_min <- input$x_min %||% NA_real_
  x_max <- input$x_max %||% NA_real_
  y_min <- input$y_min %||% NA_real_
  y_max <- input$y_max %||% NA_real_
  
  has_x_limits <- !is.na(x_min) || !is.na(x_max)
  has_y_limits <- !is.na(y_min) || !is.na(y_max)
  
  # X can be limited only when its displayed scale is numeric
  can_limit_x <- has_x &&
    !isTRUE(input$x_as_factor) &&
    is.numeric(filtered_dat[[x_var]])
  
  num_code <- function(x) {
    if (is.na(x)) "NA" else format(x, trim = TRUE, scientific = FALSE)
  }
  
  if ((can_limit_x && has_x_limits) || has_y_limits) {
    
    x_limit_code <- if (can_limit_x && has_x_limits) {
      paste0(
        "c(",
        num_code(x_min), ", ",
        num_code(x_max),
        ")"
      )
    } else {
      "NULL"
    }
    
    y_limit_code <- if (has_y_limits) {
      paste0(
        "c(",
        num_code(y_min), ", ",
        num_code(y_max),
        ")"
      )
    } else {
      "NULL"
    }
    
    plot_components <- c(
      plot_components,
      list(paste0(
        "coord_cartesian(",
        "xlim = ", x_limit_code,
        ", ylim = ", y_limit_code,
        ")"
      ))
    )
  }

  # ----- renamed discrete X-axis levels -----

  if (
    x_is_discrete &&
      length(x_levels) > 0 &&
      !identical(x_levels, edited_x_labels)
  ) {
    x_labels_code <- paste(
      vapply(edited_x_labels, quote_r, character(1)),
      collapse = ", "
    )

    lines <- c(
      lines,
      paste0(
        "x_levels <- levels(droplevels(factor(plot_data[[", quote_r(x_var), "]])))"
      ),
      paste0("x_labels <- stats::setNames(c(", x_labels_code, "), x_levels)")
    )
    plot_components <- c(
      plot_components,
      list(c(
        "scale_x_discrete(",
        "  breaks = x_levels,",
        "  labels = x_labels[x_levels]",
        ")"
      ))
    )
  }
  
  # ----- labels -----
  
  x_label <- if (!has_x) {
    if (input$plot_type %in% c("Scatterplot", "Line plot")) {
      "Observation"
    } else if (input$plot_type == "Boxplot") {
      ""
    } else {
      y_var
    }
    
  } else {
    x_var
  }
  
  y_label <- if (input$plot_type == "Histogram") {
    "Count"
  } else {
    y_var
  }
  
  x_label_custom <- input$x_label_custom %||% ""
  y_label_custom <- input$y_label_custom %||% ""
  
  if (nzchar(x_label_custom)) {
    x_label <- x_label_custom
  }
  
  if (nzchar(y_label_custom)) {
    y_label <- y_label_custom
  }
  
  label_parts <- c(
    paste0("title = ", quote_r(input$plot_title %||% "")),
    paste0("x = ", quote_r(x_label)),
    paste0("y = ", quote_r(y_label))
  )
  
  plot_components <- c(
    plot_components,
    list(paste0(
      "labs(",
      paste(label_parts, collapse = ", "),
      ")"
    ))
  )
  
  # ----- legend title and renamed group levels -----
  
  if (has_group && length(group_levels) > 0) {
    scale_group_levels <- levels(droplevels(factor(filtered_dat[[group_var]])))
    group_label_lookup <- stats::setNames(edited_group_labels, group_levels)
    scale_group_labels <- unname(group_label_lookup[scale_group_levels])
    group_labels_changed <- !identical(scale_group_levels, scale_group_labels)

    manual_values_lines <- function(values, quote_values = TRUE) {
      value_code <- if (quote_values) {
        vapply(values, quote_r, character(1))
      } else {
        as.character(values)
      }
      entries <- paste0(
        "    ",
        vapply(scale_group_levels, quote_r, character(1)),
        " = ",
        value_code
      )
      if (length(entries) > 1) {
        entries[-length(entries)] <- paste0(entries[-length(entries)], ",")
      }
      c("  values = c(", entries, "  )")
    }

    label_line <- if (group_labels_changed) {
      paste0(
        "  labels = c(",
        paste(vapply(scale_group_labels, quote_r, character(1)), collapse = ", "),
        ")"
      )
    } else {
      NULL
    }
    breaks_line <- if (group_labels_changed) {
      paste0(
        "  breaks = c(",
        paste(vapply(scale_group_levels, quote_r, character(1)), collapse = ", "),
        "),"
      )
    } else {
      NULL
    }

    color_lines <- manual_values_lines(
      scales::hue_pal()(length(scale_group_levels))
    )
    shape_lines <- manual_values_lines(
      rep(c(16, 17, 15, 3, 7, 8), length.out = length(scale_group_levels)),
      quote_values = FALSE
    )
    linetype_lines <- manual_values_lines(
      rep(
        c("solid", "22", "42", "44", "13", "1343"),
        length.out = length(scale_group_levels)
      )
    )

    if (group_labels_changed) {
      color_lines[length(color_lines)] <- paste0(color_lines[length(color_lines)], ",")
      shape_lines[length(shape_lines)] <- paste0(shape_lines[length(shape_lines)], ",")
      linetype_lines[length(linetype_lines)] <- paste0(
        linetype_lines[length(linetype_lines)], ","
      )
    }

    if (input$plot_type %in% c("Histogram", "Bar plot")) {
      plot_components <- c(
        plot_components,
        list(c(
        "scale_fill_manual(",
        paste0("  name = ", quote_r(legend_title), ","),
        color_lines,
        breaks_line,
        label_line,
        ")"
        ))
      )
      
    } else if (input$plot_type == "Boxplot" || use_group_color) {
      plot_components <- c(
        plot_components,
        list(c(
        "scale_colour_manual(",
        paste0("  name = ", quote_r(legend_title), ","),
        color_lines,
        breaks_line,
        label_line,
        ")"
        ))
      )
    }


    if (use_group_shape && input$plot_type %in% c("Scatterplot", "Line plot")) {
      plot_components <- c(
        plot_components,
        list(c(
        "scale_shape_manual(",
        paste0("  name = ", quote_r(legend_title), ","),
        shape_lines,
        breaks_line,
        label_line,
        ")"
        ))
      )
    }

    if (use_group_linetype && input$plot_type == "Line plot") {
      plot_components <- c(
        plot_components,
        list(c(
        "scale_linetype_manual(",
        paste0("  name = ", quote_r(legend_title), ","),
        linetype_lines,
        breaks_line,
        label_line,
        ")"
        ))
      )
    }
  }
  
  # ----- theme -----
  
  theme_code <- switch(
    plot_theme,
    "classic" = "theme_classic(base_size = 14)",
    "bw" = "theme_bw(base_size = 14)",
    "light" = "theme_light(base_size = 14)",
    "theme_minimal(base_size = 14)"
  )
  
  plot_components <- c(plot_components, list(theme_code))

  component_lines <- unlist(
    lapply(seq_along(plot_components), function(i) {
      component <- plot_components[[i]]
      component <- paste0("  ", component)
      if (i < length(plot_components)) {
        component[length(component)] <- paste0(component[length(component)], " +")
      }
      component
    }),
    use.names = FALSE
  )

  lines <- c(
    lines,
    paste0("p <- ", plot_call, " +"),
    component_lines,
    "",
    "p"
  )
  
  paste(lines, collapse = "\n")
}
