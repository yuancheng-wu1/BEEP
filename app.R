required_packages <- c(
  "bruceR",
  "bslib",
  "shiny",
  "ggplot2",
  "dplyr",
  "readxl"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))


source("plot_code.R")
source("ui.R")


server <- function(input, output, session) {

  # ---------- keep download dimensions aligned with the preview ----------

  observe({
    preview_width <- session$clientData$output_plot_width
    preview_height <- session$clientData$output_plot_height

    req(
      length(preview_width) == 1,
      length(preview_height) == 1,
      is.finite(preview_width),
      is.finite(preview_height),
      preview_width > 0,
      preview_height > 0
    )

    preview_resolution <- 72

    updateNumericInput(
      session,
      "plot_width",
      value = round(preview_width / preview_resolution, 2)
    )

    updateNumericInput(
      session,
      "plot_height",
      value = round(preview_height / preview_resolution, 2)
    )
  })

  # ---------- Excel sheet selection ----------

  is_excel_file <- reactive({
    req(input$file)
    tolower(tools::file_ext(input$file$name)) %in% c("xlsx", "xls")
  })

  excel_sheet_names <- reactive({
    req(is_excel_file())
    readxl::excel_sheets(input$file$datapath)
  })

  output$sheet_selector_ui <- renderUI({
    req(input$file)

    if (!is_excel_file()) {
      return(NULL)
    }

    sheets <- excel_sheet_names()

    validate(
      need(length(sheets) > 0, "No worksheets were found in this file.")
    )

    selectInput(
      inputId = "excel_sheet",
      label = "Sheet to import",
      choices = sheets,
      selected = sheets[1]
    )
  })
  
  # ---------- import data ----------
  
  uploaded_data <- reactive({
    
    req(input$file)
    
    if (is_excel_file()) {
      sheets <- excel_sheet_names()
      selected_sheet <- input$excel_sheet

      req(
        !is.null(selected_sheet),
        selected_sheet %in% sheets
      )

      bruceR::import(
        input$file$datapath,
        sheet = selected_sheet,
        as = "data.frame"
      )
    } else {
      bruceR::import(
        input$file$datapath,
        as = "data.frame"
      )
    }
  })

  # ---------- general row filters ----------

  filter_condition_ids <- reactiveVal(integer(0))
  next_filter_condition_id <- reactiveVal(0L)

  observeEvent(uploaded_data(), {
    filter_condition_ids(integer(0))
    next_filter_condition_id(0L)
  }, ignoreInit = TRUE)

  observeEvent(input$add_filter_condition, {
    req(input$file)
    new_id <- next_filter_condition_id() + 1L
    next_filter_condition_id(new_id)
    filter_condition_ids(c(filter_condition_ids(), new_id))
  })

  observeEvent(input$remove_filter_condition, {
    remove_id <- suppressWarnings(as.integer(input$remove_filter_condition))
    if (!is.na(remove_id)) {
      filter_condition_ids(setdiff(filter_condition_ids(), remove_id))
    }
  })

  observeEvent(input$clear_filter_conditions, {
    filter_condition_ids(integer(0))
  })

  output$filter_conditions_ui <- renderUI({
    if (is.null(input$file)) {
      return(tags$small("Upload data to add conditions.", class = "text-muted"))
    }

    ids <- filter_condition_ids()
    if (length(ids) == 0) {
      return(tags$small("No filters applied.", class = "text-muted"))
    }

    dat <- uploaded_data()
    variable_names <- names(dat)

    tagList(lapply(seq_along(ids), function(position) {
      id <- ids[position]
      variable_id <- paste0("filter_variable_", id)
      operator_id <- paste0("filter_operator_", id)
      value_id <- paste0("filter_value_", id)
      join_id <- paste0("filter_join_", id)

      selected_variable <- input[[variable_id]] %||% variable_names[1]
      if (!selected_variable %in% variable_names) {
        selected_variable <- variable_names[1]
      }

      operator_choices <- filter_operator_choices(dat[[selected_variable]])
      selected_operator <- input[[operator_id]] %||% unname(operator_choices[1])
      if (!selected_operator %in% unname(operator_choices)) {
        selected_operator <- unname(operator_choices[1])
      }

      value_control <- NULL
      if (!selected_operator %in% c("is_missing", "not_missing")) {
        column <- dat[[selected_variable]]
        current_value <- isolate(input[[value_id]])
        values <- sort(unique(column[!is.na(column)]))
        values <- as.character(values)
        fallback_value <- if (length(values) > 0) values[1] else ""
        default_value <- as.character(current_value %||% fallback_value)
        if (length(default_value) == 0 || is.na(default_value)) default_value <- ""
        value_choices <- unique(c(values, default_value[nzchar(default_value)]))

        value_control <- selectizeInput(
          value_id,
          "Value",
          choices = value_choices,
          selected = default_value,
          options = list(
            create = TRUE,
            persist = FALSE,
            placeholder = "Choose an existing value or type a new one"
          )
        )
      }

      join_control <- if (position > 1) {
        div(
          class = "beep-filter-join",
          selectInput(
            join_id,
            label = NULL,
            choices = c("AND", "OR"),
            selected = isolate(input[[join_id]]) %||% "AND"
          )
        )
      } else {
        NULL
      }

      tagList(
        join_control,
        div(
          class = "beep-filter-row",
          fluidRow(
            column(
              6,
              selectInput(
                variable_id,
                "Variable",
                choices = variable_names,
                selected = selected_variable
              )
            ),
            column(
              6,
              selectInput(
                operator_id,
                "Requirement",
                choices = operator_choices,
                selected = selected_operator
              )
            )
          ),
          fluidRow(
            column(10, value_control),
            column(
              2,
              tags$label("Remove", class = "form-label invisible"),
              actionButton(
                paste0("remove_filter_", id),
                label = icon("trash"),
                class = "btn-outline-danger btn-sm",
                onclick = paste0(
                  "Shiny.setInputValue('remove_filter_condition', '",
                  id,
                  "', {priority: 'event'})"
                )
              )
            )
          )
        )
      )
    }))
  })

  filter_conditions <- reactive({
    ids <- filter_condition_ids()

    lapply(seq_along(ids), function(position) {
      id <- ids[position]
      list(
        variable = input[[paste0("filter_variable_", id)]] %||% "",
        operator = input[[paste0("filter_operator_", id)]] %||% "",
        value = input[[paste0("filter_value_", id)]],
        join = if (position == 1) {
          "AND"
        } else {
          input[[paste0("filter_join_", id)]] %||% "AND"
        }
      )
    })
  })

  general_filtered_data <- reactive({
    req(input$file)
    apply_general_filters(uploaded_data(), filter_conditions())
  })

  output$filter_status_ui <- renderUI({
    if (is.null(input$file)) return(NULL)

    total_rows <- nrow(uploaded_data())
    filtered_rows <- nrow(general_filtered_data())

    div(
      class = "beep-filter-status",
      paste0(
        format(filtered_rows, big.mark = ","),
        " of ",
        format(total_rows, big.mark = ","),
        " rows included"
      )
    )
  })
  
  # ---------- update variable menus after upload ----------
  
  observeEvent(uploaded_data(), {
    
    variable_names <- names(uploaded_data())
    
    updateSelectInput(
      session,
      "x_var",
      choices = c("None", variable_names),
      selected = "None"
    )
    
    updateSelectInput(
      session,
      "y_var",
      choices = c("None", variable_names),
      selected = "None"
    )
    
    updateSelectInput(
      session,
      "group_var",
      choices = c("None", variable_names),
      selected = "None"
    )
    
    updateSelectInput(
      session,
      "facet_var",
      choices = c("None", variable_names),
      selected = "None"
    )
  })

  
  output$x_filter_ui <- renderUI({
    
    req(input$file)
    
    x_var <- input$x_var
    
    if (is.null(x_var) ||
        x_var == "None" ||
        !isTRUE(input$x_as_factor)) {
      return(NULL)
    }
    
    dat <- uploaded_data()
    
    if (!x_var %in% names(dat)) {
      return(NULL)
    }
    
    x_levels <- sort(unique(as.character(dat[[x_var]])))
    x_levels <- x_levels[!is.na(x_levels)]
    
    selectizeInput(
      "exclude_x_levels",
      "X levels to exclude",
      choices = x_levels,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select X levels to remove"
      )
    )
  })
  
  output$group_filter_ui <- renderUI({
    
    req(input$file)
    
    group_var <- input$group_var
    
    if (is.null(group_var) || group_var == "None") {
      return(NULL)
    }
    
    dat <- uploaded_data()
    
    if (!group_var %in% names(dat)) {
      return(NULL)
    }
    
    group_levels <- sort(unique(as.character(dat[[group_var]])))
    group_levels <- group_levels[!is.na(group_levels)]
    
    selectizeInput(
      "exclude_groups",
      "Groups to exclude",
      choices = group_levels,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select groups to remove"
      )
    )
  })

  output$facet_filter_ui <- renderUI({
    req(input$file)
  
    facet_var <- input$facet_var
  
    if (
      is.null(facet_var) ||
      facet_var == "None"
    ) {
      return(NULL)
    }
  
    dat <- uploaded_data()
  
    if (!facet_var %in% names(dat)) {
      return(NULL)
    }
  
    facet_levels <- sort(
      unique(as.character(dat[[facet_var]]))
    )
  
    facet_levels <- facet_levels[!is.na(facet_levels)]
  
    selectizeInput(
      inputId = "exclude_facets",
      label = "Facet levels to exclude",
      choices = facet_levels,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select facet levels to remove"
      )
    )
  })

  
  # ---------- preview imported data ----------
  
  output$data_preview <- renderTable({
    
    if (is.null(input$file)) {
      return(data.frame(Message = "No data imported"))
    }
    
    preview_data <- if ((input$data_preview_source %||% "full") == "filtered") {
      general_filtered_data()
    } else {
      uploaded_data()
    }

    head(preview_data, 10)
  })

  output$data_preview_status_ui <- renderUI({
    if (is.null(input$file)) return(NULL)

    full_rows <- nrow(uploaded_data())
    filtered_rows <- nrow(general_filtered_data())
    selected_source <- input$data_preview_source %||% "full"

    tags$p(
      if (selected_source == "filtered") {
        paste0(
          "Showing the first 10 filtered rows. ",
          format(filtered_rows, big.mark = ","),
          " of ",
          format(full_rows, big.mark = ","),
          " rows match the current conditions."
        )
      } else {
        paste0(
          "Showing the first 10 rows of the full dataset (",
          format(full_rows, big.mark = ","),
          " rows)."
        )
      },
      class = "text-muted"
    )
  })
  
  # ---------- create plot ----------
  plot_data <- reactive({
    req(input$file)
    filter_plot_data(input, uploaded_data(), filter_conditions())
  })

  # ---------- X-axis level name change ----------
  x_levels_current <- reactive({

    req(input$file)

    x_var <- input$x_var %||% "None"

    if (x_var == "None") {
      return(character(0))
    }

    dat <- plot_data()

    if (!x_var %in% names(dat)) {
      return(character(0))
    }

    x_is_discrete <- isTRUE(input$x_as_factor) ||
      is.factor(dat[[x_var]]) ||
      is.character(dat[[x_var]]) ||
      is.logical(dat[[x_var]])

    if (!x_is_discrete) {
      return(character(0))
    }

    if (isTRUE(input$x_as_factor) || is.factor(dat[[x_var]])) {
      return(levels(droplevels(factor(dat[[x_var]]))))
    }

    x_levels <- sort(unique(as.character(dat[[x_var]])))
    x_levels[!is.na(x_levels)]
  })

  output$x_refinement_ui <- renderUI({

    x_levels <- x_levels_current()

    if (length(x_levels) == 0) {
      return(NULL)
    }

    tagList(
      tags$hr(),
      tags$strong("X-axis level names"),
      tags$p(
        "Edit the label shown for each discrete X-axis level.",
        class = "text-muted"
      ),
      lapply(seq_along(x_levels), function(i) {
        textInput(
          inputId = paste0("x_label_", i),
          label = paste0("Rename “", x_levels[i], "”"),
          value = x_levels[i]
        )
      })
    )
  })

  x_label_values <- reactive({

    x_levels <- x_levels_current()

    if (length(x_levels) == 0) {
      return(NULL)
    }

    edited_labels <- vapply(
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

    stats::setNames(edited_labels, x_levels)
  })

  # ---------- group name change ----------
    group_levels_current <- reactive({
    
      req(input$file)
      
      group_var <- input$group_var %||% "None"
      
      if (group_var == "None") {
        return(character(0))
      }
    
    dat <- plot_data()
    
    if (!group_var %in% names(dat)) {
      return(character(0))
    }
    
    group_levels <- unique(
      as.character(dat[[group_var]])
    )
    
    group_levels <- group_levels[!is.na(group_levels)]
    
    group_levels
  })

  output$group_refinement_ui <- renderUI({
  
    group_levels <- group_levels_current()
    
    if (length(group_levels) == 0) {
      return(NULL)
    }
    
    tagList(
      
      tags$p(
        "Edit the name for each legend level.",
        class = "text-muted"
      ),
      
      lapply(seq_along(group_levels), function(i) {
        
        textInput(
          inputId = paste0("group_label_", i),
          label = paste0(
            "Rename “",
            group_levels[i],
            "”"
          ),
          value = group_levels[i]
        )
      })
    )
  }) 

  group_label_values <- reactive({
  
    group_levels <- group_levels_current()
    
      if (length(group_levels) == 0) {
        return(NULL)
      }
      
      edited_labels <- vapply(
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
      
      stats::setNames(
        edited_labels,
        group_levels
      )
    })


  # ---------- facet name change ----------
  facet_levels_current <- reactive({

    req(input$file)
  
    facet_var <- input$facet_var %||% "None"
  
    if (facet_var == "None") {
      return(character(0))
    }
  
    dat <- plot_data()
  
    if (!facet_var %in% names(dat)) {
      return(character(0))
    }
  
    facet_levels <- unique(
      as.character(dat[[facet_var]])
    )
  
    facet_levels <- facet_levels[!is.na(facet_levels)]
  
    facet_levels
  })

  output$facet_refinement_ui <- renderUI({

    facet_levels <- facet_levels_current()
  
    if (length(facet_levels) == 0) {
      return(NULL)
    }
  
    tagList(
  
      tags$p(
        "Edit the label shown at the top of each facet panel.",
        class = "text-muted"
      ),
  
      lapply(seq_along(facet_levels), function(i) {
  
        textInput(
          inputId = paste0("facet_label_", i),
          label = paste0(
            "Rename “",
            facet_levels[i],
            "”"
          ),
          value = facet_levels[i]
        )
      })
    )
  })

  facet_label_values <- reactive({

    facet_levels <- facet_levels_current()
  
    if (length(facet_levels) == 0) {
      return(NULL)
    }
  
    edited_labels <- vapply(
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
  
    stats::setNames(
      edited_labels,
      facet_levels
    )
  })


  # ---------- draw plot ----------
  plot_object <- reactive({
    
    validate(
      need(!is.null(input$file), "No data imported.")
    )
    
    dat <- plot_data()
    
    # Protect against temporary NULL values during app initialization
    x_var <- if (is.null(input$x_var)) "None" else input$x_var
    y_var <- if (is.null(input$y_var)) "None" else input$y_var
    group_var <- if (is.null(input$group_var)) "None" else input$group_var
    facet_var <- if (is.null(input$facet_var)) "None" else input$facet_var
    
    validate(
      need(
        x_var != "None" || y_var != "None",
        "Please select at least one of X or Y."
      )
    )
    
    has_x <- x_var != "None" && x_var %in% names(dat)
    has_y <- y_var != "None" && y_var %in% names(dat)
    has_group <- group_var != "None" && group_var %in% names(dat)
    has_facet <- facet_var != "None" && facet_var %in% names(dat)

    validate(need(nrow(dat) > 0, "No observations remain after applying the filters and exclusions."))
    
    # ---------- temporary plotting variables ----------
    
    x_var_plot <- NULL
    group_var_plot <- NULL
    facet_var_plot <- NULL
    
    if (has_x) {
  
      if (isTRUE(input$x_as_factor)) {
        dat$.plot_x <- droplevels(factor(dat[[x_var]]))
        x_var_plot <- ".plot_x"
      } else {
        x_var_plot <- x_var
      }
    }

    if (has_group) {
      dat$.plot_group <- droplevels(factor(dat[[group_var]]))
      group_var_plot <- ".plot_group"
    }
    
    if (has_facet) {
      dat$.plot_facet <- droplevels(factor(dat[[facet_var]]))
      facet_var_plot <- ".plot_facet"
    }
    
    # ---------- histogram ----------
    
    if (input$plot_type == "Histogram") {
      
      hist_var <- if (has_x) x_var_plot else y_var
      
      validate(
        need(
          is.numeric(dat[[hist_var]]),
          "Histogram requires a numeric X or Y variable."
        )
      )
      
      if (!has_group) {
        
        p <- ggplot(
          dat,
          aes(x = .data[[hist_var]])
        ) +
          geom_histogram(bins = 30)
        
      } else {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[hist_var]],
            fill = .data[[group_var_plot]]
          )
        ) +
          geom_histogram(
            bins = 30,
            alpha = 0.6,
            position = "identity"
          )
      }
    }
    
    # ---------- bar plot ----------
    
    else if (input$plot_type == "Bar plot") {
      
      validate(
        need(has_x, "Bar plots require an X variable."),
        need(has_y, "Bar plots require a numeric Y variable."),
        need(is.numeric(dat[[y_var]]), "Bar plots require a numeric Y variable.")
      )
      
      if (!has_group) {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]]
          )
        ) +
          stat_summary(
            fun = mean,
            geom = "col",
            na.rm = TRUE
          )
        
      } else {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]],
            fill = .data[[group_var_plot]]
          )
        ) +
          stat_summary(
            fun = mean,
            geom = "col",
            position = position_dodge(width = 0.9),
            na.rm = TRUE
          )
      }
    }
    
    # ---------- boxplot ----------
    
    else if (input$plot_type == "Boxplot") {
      
      validate(
        need(has_y, "Boxplot requires a Y variable.")
      )

      if (!has_x) {
        dat$.single_box <- "All data"
        box_x_var <- ".single_box"
      } else {
        box_x_var <- x_var_plot
      }

      if (has_group) {
        p <- ggplot(
          dat,
          aes(
            x = .data[[box_x_var]],
            y = .data[[y_var]],
            color = .data[[group_var_plot]],
            group = interaction(
              .data[[box_x_var]],
              .data[[group_var_plot]],
              drop = TRUE
            )
          )
        ) +
          geom_boxplot() +
          geom_point(
            position = position_jitterdodge(
              jitter.width = 0.08,
              seed = 123
            ),
            size = 1.5,
            alpha = 0.3
          )
      } else {
        p <- ggplot(
          dat,
          aes(
            x = .data[[box_x_var]],
            y = .data[[y_var]]
          )
        ) +
          geom_boxplot() +
          geom_point(
            position = position_jitter(
              width = 0.08,
              seed = 123
            ),
            size = 1.5,
            alpha = 0.3
          )
      }
    }
    
    # ---------- scatterplot ----------
    
    else if (input$plot_type == "Scatterplot") {
      
      validate(
        need(has_y, "Scatterplot requires a Y variable.")
      )
      
      if (!has_x) {
        dat$.plot_index <- seq_len(nrow(dat))
        x_var_plot <- ".plot_index"
      }
      
      if (!has_group) {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]]
          )
        ) +
          geom_point()
        
      } else {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]],
            color = .data[[group_var_plot]]
          )
        ) +
          geom_point()
      }
      
      if (input$add_smooth) {
        p <- p + geom_smooth(
          method = "lm",
          se = TRUE
        )
      }
    }
    
    # ---------- line plot ----------
    
    else if (input$plot_type == "Line plot") {
      
      validate(
        need(has_y, "Line plot requires a Y variable.")
      )
      
      if (!has_x) {
        dat$.plot_index <- seq_len(nrow(dat))
        x_var_plot <- ".plot_index"
      }
      
      if (!has_group) {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]]
          )
        ) +
          geom_line()
        
      } else {
        
        p <- ggplot(
          dat,
          aes(
            x = .data[[x_var_plot]],
            y = .data[[y_var]],
            color = .data[[group_var_plot]],
            group = .data[[group_var_plot]]
          )
        ) +
          geom_line()
      }
    }
    
    # ---------- facets ----------
    
    if (has_facet) {

      facet_labels <- facet_label_values()

      if (!is.null(facet_labels)) {
    
        p <- p +
          facet_wrap(
            vars(.data[[facet_var_plot]]),
            labeller = as_labeller(facet_labels)
          )
    
      } else {
    
        p <- p +
          facet_wrap(
            vars(.data[[facet_var_plot]])
          )
      }
    }
    
    # ---------- labels ----------
    
    x_label <- if (!has_x) {
      
      if (input$plot_type %in% c("Scatterplot", "Line plot")) {
        "Observation"
        
      } else if (input$plot_type == "Boxplot") {
        ""
        
      } else if (input$plot_type == "Histogram") {
        y_var
        
      } else {
        ""
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
    
    # ---------- legend title ----------
    
    legend_title <- if (has_group) {
      
      custom_title <- trimws(input$legend_title %||% "")
      
      if (nzchar(custom_title)) {
        custom_title
      } else {
        group_var
      }
      
    } else {
      NULL
    }
    
    # ---------- axis limits ----------
    
    x_limits <- c(input$x_min, input$x_max)
    y_limits <- c(input$y_min, input$y_max)
    
    x_limits[is.na(x_limits)] <- NA_real_
    y_limits[is.na(y_limits)] <- NA_real_
    
    has_x_limits <- any(!is.na(x_limits))
    has_y_limits <- any(!is.na(y_limits))
    
    can_limit_x <- has_x &&
      !isTRUE(input$x_as_factor) &&
      is.numeric(dat[[x_var]])
    
    if (
      (can_limit_x && has_x_limits) ||
      has_y_limits
    ) {
      
      p <- p +
        coord_cartesian(
          xlim = if (
            can_limit_x &&
            has_x_limits
          ) {
            x_limits
          } else {
            NULL
          },
          
          ylim = if (has_y_limits) {
            y_limits
          } else {
            NULL
          }
        )
    }
    
    # ---------- general plot labels ----------
    
    p <- p +
      labs(
        title = input$plot_title,
        x = x_label,
        y = y_label
      )
    
    # ---------- legend level labels ----------
    
    if (has_group) {
      
      legend_labels <- group_label_values()
      
      if (!is.null(legend_labels)) {
        
        p <- p +
          scale_color_discrete(
            name = legend_title,
            limits = names(legend_labels),
            breaks = names(legend_labels),
            labels = unname(legend_labels),
            drop = FALSE
          ) +
          scale_fill_discrete(
            name = legend_title,
            limits = names(legend_labels),
            breaks = names(legend_labels),
            labels = unname(legend_labels),
            drop = FALSE
          )
      }
    }

    # ---------- discrete X-axis level labels ----------

    x_is_discrete <- has_x &&
      (
        isTRUE(input$x_as_factor) ||
          is.factor(dat[[x_var]]) ||
          is.character(dat[[x_var]]) ||
          is.logical(dat[[x_var]])
      )

    if (x_is_discrete) {
      x_level_labels <- x_label_values()

      if (!is.null(x_level_labels)) {
        p <- p +
          scale_x_discrete(
            breaks = names(x_level_labels),
            labels = unname(x_level_labels)
          )
      }
    }
    
    # ---------- theme ----------
    
    selected_theme <- switch(
      input$plot_theme,
      "classic" = theme_classic(base_size = 14),
      "bw" = theme_bw(base_size = 14),
      "light" = theme_light(base_size = 14),
      theme_minimal(base_size = 14)
    )

    p <- p + selected_theme
    
    p
  })

  #### summay statistics
  summary_data <- reactive({
    
    if (is.null(input$file)) {
      return(data.frame(Message = "No data imported"))
    }
    
    dat <- plot_data()
    
    x_var <- if (is.null(input$x_var)) "None" else input$x_var
    y_var <- if (is.null(input$y_var)) "None" else input$y_var
    group_var <- if (is.null(input$group_var)) "None" else input$group_var
    facet_var <- if (is.null(input$facet_var)) "None" else input$facet_var
    
    has_x <- x_var != "None" && x_var %in% names(dat)
    has_y <- y_var != "None" && y_var %in% names(dat)
    has_group <- group_var != "None" && group_var %in% names(dat)
    has_facet <- facet_var != "None" && facet_var %in% names(dat)
    
    if (!has_y) {
      return(
        data.frame(
          Message = "Select a numeric Y variable to show mean, median, SD, and range."
        )
      )
    }
    
    if (!is.numeric(dat[[y_var]])) {
      return(
        data.frame(
          Message = "Summary statistics require a numeric Y variable."
        )
      )
    }
    
    # Safe functions for groups with all missing values
    safe_mean <- function(x) {
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    }
    
    safe_median <- function(x) {
      if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
    }

    safe_sd <- function(x) {
      if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
    }
    
    safe_range <- function(x) {
      
      if (all(is.na(x))) {
        return(NA_character_)
      }
      
      paste0("(", format(round(min(x, na.rm = TRUE),2), trim = TRUE), ", ",
        format(round(max(x, na.rm = TRUE),2), trim = TRUE), ")"
      )
    }
    
    group_columns <- character(0)
    
    # Include X only when it is categorical
    x_is_categorical <- has_x &&
      (
        isTRUE(input$x_as_factor) ||
          is.factor(dat[[x_var]]) ||
          is.character(dat[[x_var]])
      )
    
    if (x_is_categorical) {
      dat$.summary_x <- factor(dat[[x_var]])
      group_columns <- c(group_columns, ".summary_x")
    }
    
    if (has_group) {
      dat$.summary_group <- factor(dat[[group_var]])
      group_columns <- c(group_columns, ".summary_group")
    }
    
    if (has_facet) {
      dat$.summary_facet <- factor(dat[[facet_var]])
      group_columns <- c(group_columns, ".summary_facet")
    }
    
    if (length(group_columns) == 0) {
      
      result <- data.frame(
        N = sum(!is.na(dat[[y_var]])),
        Mean = safe_mean(dat[[y_var]]),
        Median = safe_median(dat[[y_var]]),
        SD = safe_sd(dat[[y_var]]),
        Range = safe_range(dat[[y_var]])
      )
      
    } else {
      
      result <- dat %>%
        group_by(across(all_of(group_columns))) %>%
        summarise(
          N = sum(!is.na(.data[[y_var]])),
          Mean = safe_mean(.data[[y_var]]),
          Median = safe_median(.data[[y_var]]),
          SD = safe_sd(.data[[y_var]]),
          Range = safe_range(.data[[y_var]]),
          .groups = "drop"
        )
    }
    
    # Replace temporary names with the original variable names
    names(result)[names(result) == ".summary_x"] <- x_var
    names(result)[names(result) == ".summary_group"] <- group_var
    names(result)[names(result) == ".summary_facet"] <- facet_var
    
    result
  })


      
  
  
  # ---------- save R codes ----------
  plot_code <- reactive({
    
    if (is.null(input$file)) {
      return("# No data imported yet.")
    }
    
    make_plot_code(
      input = input,
      dat = uploaded_data(),
      conditions = filter_conditions()
    )
  })
  
  # ---------- render plot ----------
  
  output$plot <- renderPlot({
    plot_object()
  })
  
  ## summary table of the plot
  output$summary_stats <- renderTable({  
    summary_data()
  }, digits = 3)
  # ---------- download plot ----------
  
  output$download_plot <- downloadHandler(
  
    filename = function() {
      paste0(
        "BEEP_plot_",
        Sys.Date(),
        ".",
        input$plot_format
      )
    },
    
    content = function(file) {
      
      format <- input$plot_format %||% "png"
      
      if (format == "tiff") {
        
        ggsave(
          filename = file,
          plot = plot_object(),
          device = "tiff",
          width = input$plot_width,
          height = input$plot_height,
          units = "in",
          dpi = input$plot_dpi,
          compression = "lzw",
          bg = "white"
        )
        
      } else {
        
        ggsave(
          filename = file,
          plot = plot_object(),
          device = format,
          width = input$plot_width,
          height = input$plot_height,
          units = "in",
          dpi = input$plot_dpi,
          bg = "white"
        )
      }
    }
  )
  
  ## download R codes for the plot
  output$plot_code <- renderText({
    plot_code()
  })
  
  output$download_code <- downloadHandler(
    
    filename = function() {
      paste0("ggplot_code_", Sys.Date(), ".R")
    },
    
    content = function(file) {
      writeLines(plot_code(), file)
    }
  )
}

shinyApp(ui, server)
