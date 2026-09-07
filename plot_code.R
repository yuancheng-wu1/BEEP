`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

filter_plot_data <- function(input, dat) {
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

make_plot_code <- function(input, dat) {
  
  x_var <- input$x_var %||% "None"
  y_var <- input$y_var %||% "None"
  group_var <- input$group_var %||% "None"
  facet_var <- input$facet_var %||% "None"
  
  has_x <- x_var != "None" && x_var %in% names(dat)
  has_y <- y_var != "None" && y_var %in% names(dat)
  has_group <- group_var != "None" && group_var %in% names(dat)
  
  legend_title <- trimws(input$legend_title %||% "")
  if (has_group && !nzchar(legend_title)) {
    legend_title <- group_var
  }
  plot_theme <- input$plot_theme %||% "minimal"
  
  has_facet <- facet_var != "None" && facet_var %in% names(dat)
  exclude_x_levels <- input$exclude_x_levels %||% character(0)
  exclude_groups <- input$exclude_groups %||% character(0)
  exclude_facets <- input$exclude_facets %||% character(0)
  filtered_dat <- filter_plot_data(input, dat)

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
    return("# No observations remain after applying the exclusions.")
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

  lines <- c(lines, paste0("plot_data <- ", data_name))
  
  if (has_x &&
      isTRUE(input$x_as_factor) &&
      length(exclude_x_levels) > 0) {
    
    excluded_x_text <- paste(
      vapply(exclude_x_levels, quote_r, character(1)),
      collapse = ", "
    )
    
    lines <- c(
      lines,
      paste0(
        "plot_data <- plot_data[!",
        "as.character(plot_data[[",
        quote_r(x_var),
        "]]) %in% c(",
        excluded_x_text,
        "), , drop = FALSE]"
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
      paste0(
        "plot_data <- plot_data[!",
        "as.character(plot_data[[",
        quote_r(group_var),
        "]]) %in% c(",
        excluded_group_text,
        "), , drop = FALSE]"
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
      paste0(
        "plot_data <- plot_data[",
        "!as.character(plot_data[[",
        quote_r(facet_var),
        "]]) %in% c(",
        excluded_facet_text,
        "), , drop = FALSE]"
      )
    )
  }
 
  # ----- prepare variables -----
  
  if (has_x) {
    lines <- c(lines, paste0("x_var <- ", quote_r(x_var)))
    
    if (isTRUE(input$x_as_factor)) {
      lines <- c(
        lines,
        "plot_data$.plot_x <- droplevels(factor(plot_data[[x_var]]))",
        'x_var_plot <- ".plot_x"'
      )
    } else {
      lines <- c(lines, "x_var_plot <- x_var")
    }
  }
  
  if (has_y) {
    lines <- c(lines, paste0("y_var <- ", quote_r(y_var)))
  }
  
  if (has_group) {
    lines <- c(
      lines,
      paste0("group_var <- ", quote_r(group_var)),
      "plot_data$.plot_group <- droplevels(factor(plot_data[[group_var]]))",
      'group_var_plot <- ".plot_group"'
    )
  }
  
  if (has_facet) {
    lines <- c(
      lines,
      paste0("facet_var <- ", quote_r(facet_var)),
      "plot_data$.plot_facet <- droplevels(factor(plot_data[[facet_var]]))",
      'facet_var_plot <- ".plot_facet"'
    )
  }
  
  lines <- c(lines, "")
  
  # ----- determine X expression -----
  
  if (has_x) {
    x_expr <- ".data[[x_var_plot]]"
    
  } else if (input$plot_type == "Boxplot") {
    lines <- c(
      lines,
      'plot_data$.single_box <- "All data"',
      ""
    )
    x_expr <- '.data[[".single_box"]]'
    
  } else if (input$plot_type %in% c("Scatterplot", "Line plot")) {
    lines <- c(
      lines,
      "plot_data$.plot_index <- seq_len(nrow(plot_data))",
      ""
    )
    x_expr <- '.data[[".plot_index"]]'
    
  } else {
    x_expr <- ".data[[y_var]]"
  }
  
  y_expr <- ".data[[y_var]]"
  group_expr <- ".data[[group_var_plot]]"
  
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
    
    if (has_group) {
      aes_parts <- c(
        aes_parts,
        paste0("color = ", group_expr)
      )
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
    }
  }
  
  lines <- c(
    lines,
    paste0(
      "p <- ggplot(plot_data, aes(",
      paste(aes_parts, collapse = ", "),
      "))"
    )
  )
  
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
      lines <- c(
        lines,
        'p <- p + geom_histogram(bins = 30, alpha = 0.6, position = "identity")'
      )
    } else {
      lines <- c(lines, "p <- p + geom_histogram(bins = 30)")
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
      lines <- c(
        lines,
        "p <- p + stat_summary(",
        "  fun = mean,",
        "  geom = 'col',",
        "  position = position_dodge(width = 0.9),",
        "  na.rm = TRUE",
        ")"
      )
    } else {
      lines <- c(
        lines,
        "p <- p + stat_summary(",
        "  fun = mean,",
        "  geom = 'col',",
        "  na.rm = TRUE",
        ")"
      )}
    
    
  } else if (input$plot_type == "Boxplot") {
    
    if (!has_y) {
      return("# Boxplot requires a Y variable.")
    }
    
    if (has_group) {
      lines <- c(
        lines,
        "p <- p +",
        "  geom_boxplot(position = position_dodge(width = 0.75)) +",
        "  geom_point(",
        "    position = position_jitterdodge(",
        "      jitter.width = 0.08,",
        "      dodge.width = 0.75,",
        "      seed = 123",
        "    ),",
        "    size = 1.5, alpha = 0.3",
        "  )"
      )
    } else {
      lines <- c(
        lines,
        "p <- p +",
        "  geom_boxplot() +",
        "  geom_point(",
        "    position = position_jitter(width = 0.08, seed = 123),",
        "    size = 1.5, alpha = 0.3",
        "  )"
      )
    }
    
  } else if (input$plot_type == "Scatterplot") {
    
    if (!has_y) {
      return("# Scatterplot requires a Y variable.")
    }
    
    lines <- c(lines, "p <- p + geom_point()")
    
    if (isTRUE(input$add_smooth)) {
      lines <- c(
        lines,
        'p <- p + geom_smooth(method = "lm", se = TRUE)'
      )
    }
    
  } else if (input$plot_type == "Line plot") {
    
    if (!has_y) {
      return("# Line plot requires a Y variable.")
    }
    
    lines <- c(lines, "p <- p + geom_line()")
  }
  
  
  # ----- facet -----

if (has_facet) {

  if (length(facet_levels) > 0) {

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
      "",
      paste0(
        "facet_labels <- stats::setNames(",
        "c(", facet_labels_code, "), ",
        "c(", facet_breaks_code, ")",
        ")"
      ),
      "p <- p + facet_wrap(",
      "  vars(.data[[facet_var_plot]]),",
      "  labeller = as_labeller(facet_labels)",
      ")"
    )

  } else {

    lines <- c(
      lines,
      "p <- p + facet_wrap(vars(.data[[facet_var_plot]]))"
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
    
    lines <- c(
      lines,
      paste0(
        "p <- p + coord_cartesian(",
        "xlim = ", x_limit_code,
        ", ylim = ", y_limit_code,
        ")"
      )
    )
  }

  # ----- renamed discrete X-axis levels -----

  if (x_is_discrete && length(x_levels) > 0) {
    x_breaks_code <- paste(
      vapply(x_levels, quote_r, character(1)),
      collapse = ", "
    )

    x_labels_code <- paste(
      vapply(edited_x_labels, quote_r, character(1)),
      collapse = ", "
    )

    lines <- c(
      lines,
      "",
      "p <- p + scale_x_discrete(",
      paste0("  breaks = c(", x_breaks_code, "),"),
      paste0("  labels = c(", x_labels_code, ")"),
      ")"
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
  
  lines <- c(
    lines,
    "",
    paste0(
      "p <- p + labs(",
      paste(label_parts, collapse = ", "),
      ")"
    )
  )
  
  # ----- legend title and renamed group levels -----
  
  if (has_group && length(group_levels) > 0) {
    
    breaks_code <- paste(
      vapply(group_levels, quote_r, character(1)),
      collapse = ", "
    )
    
    labels_code <- paste(
      vapply(edited_group_labels, quote_r, character(1)),
      collapse = ", "
    )
    
    if (input$plot_type %in% c("Histogram", "Bar plot")) {
      lines <- c(
        lines,
        "",
        "p <- p + scale_fill_discrete(",
        paste0("  name = ", quote_r(legend_title), ","),
        paste0("  breaks = c(", breaks_code, "),"),
        paste0("  labels = c(", labels_code, ")"),
        ")"
      )
      
    } else {
      lines <- c(
        lines,
        "",
        "p <- p + scale_color_discrete(",
        paste0("  name = ", quote_r(legend_title), ","),
        paste0("  breaks = c(", breaks_code, "),"),
        paste0("  labels = c(", labels_code, ")"),
        ")"
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
  
  lines <- c(
    lines,
    "",
    paste0("p <- p + ", theme_code),
    "",
    "p"
  )
  
  paste(lines, collapse = "\n")
}
