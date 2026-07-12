`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

make_plot_code <- function(input, dat) {
  
  x_var <- input$x_var %||% "None"
  y_var <- input$y_var %||% "None"
  group_var <- input$group_var %||% "None"
  facet_var <- input$facet_var %||% "None"
  
  has_x <- x_var != "None" && x_var %in% names(dat)
  has_y <- y_var != "None" && y_var %in% names(dat)
  has_group <- group_var != "None" && group_var %in% names(dat)
  has_facet <- facet_var != "None" && facet_var %in% names(dat)
  exclude_x_levels <- input$exclude_x_levels %||% character(0)
  exclude_groups <- input$exclude_groups %||% character(0)
  exclude_facets <- input$exclude_facets %||% character(0)
  
  if (!has_x && !has_y) {
    return("# Select at least one X or Y variable.")
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
    "",
    paste0("plot_data <- ", data_name)
  )
  
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
        "plot_data$.plot_x <- factor(plot_data[[x_var]])",
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
      "plot_data$.plot_group <- factor(plot_data[[group_var]])",
      'group_var_plot <- ".plot_group"'
    )
  }
  
  if (has_facet) {
    lines <- c(
      lines,
      paste0("facet_var <- ", quote_r(facet_var)),
      "plot_data$.plot_facet <- factor(plot_data[[facet_var]])",
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
  
  if (input$plot_type %in% c("Histogram", "Bar plot")) {
    
    aes_parts <- paste0("x = ", x_expr)
    
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
    
    if (input$plot_type %in% c("Boxplot", "Line plot") && has_group) {
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
        "      dodge.width = 0.75",
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
        "    position = position_jitter(width = 0.08),",
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
    lines <- c(
      lines,
      "p <- p + facet_wrap(vars(.data[[facet_var_plot]]))"
    )
  }
  
  # ----- axis limits -----
  
  x_min <- input$x_min %||% NA_real_
  x_max <- input$x_max %||% NA_real_
  y_min <- input$y_min %||% NA_real_
  y_max <- input$y_max %||% NA_real_
  
  has_x_limits <- !is.na(x_min) || !is.na(x_max)
  has_y_limits <- !is.na(y_min) || !is.na(y_max)
  
  # X can be limited only when its displayed scale is numeric
  can_limit_x <- (
    has_x &&
      !isTRUE(input$x_as_factor) &&
      is.numeric(dat[[x_var]])
  ) ||
    (
      !has_x &&
        input$plot_type == "Histogram" &&
        has_y &&
        is.numeric(dat[[y_var]])
    ) ||
    (
      !has_x &&
        input$plot_type %in% c("Scatterplot", "Line plot")
    )
  
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
  
  # ----- labels -----
  
  x_label <- if (!has_x) {
    if (input$plot_type %in% c("Scatterplot", "Line plot")) {
      "Observation"
    } else if (input$plot_type == "Boxplot") {
      ""
    } else {
      y_var
    }
    
  } else if (isTRUE(input$x_as_factor)) {
    paste0(x_var, " (factor)")
  } else {
    x_var
  }
  
  y_label <- if (input$plot_type %in% c("Histogram", "Bar plot")) {
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
  
  if (has_group) {
    label_parts <- c(
      label_parts,
      paste0("color = ", quote_r(group_var)),
      paste0("fill = ", quote_r(group_var))
    )
  }
  
  lines <- c(
    lines,
    paste0(
      "p <- p + labs(",
      paste(label_parts, collapse = ", "),
      ") +"
    ),
    "  theme_minimal(base_size = 14)",
    "",
    "p"
  )
  
  paste(lines, collapse = "\n")
}
