library(shiny)
library(ggplot2)
library(bruceR)
library(dplyr)
app_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(app_dir, "plot_code.R"))

ui <- fluidPage(
  
  titlePanel("Build Elegant and Efficient Plots (BEEP)"),
  
  tags$head(
    tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
      navigator.clipboard.writeText(message.code).then(function() {
        $('#copy_code').text('Copied!');
        
        setTimeout(function() {
          $('#copy_code').text('Copy');
        }, 1500);
      });
    });
  "))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "file",
        "Upload data",
        accept = c(".csv", ".xlsx", ".xls", ".rds")
      ),
      
      hr(),
      
      selectInput(
        "plot_type",
        "Plot type",
        choices = c(
          "Scatterplot",
          "Boxplot",
          "Bar plot",
          "Histogram",
          "Line plot"
        )
      ),
      
      selectInput(
        "x_var",
        "X variable",
        choices = "None",
        selected = "None"
      ),
      
      checkboxInput(
        "x_as_factor",
        "Treat X variable as a factor",
        value = FALSE
      ),
      uiOutput("x_filter_ui"),
      
      selectInput(
        "y_var",
        "Y variable",
        choices = "None",
        selected = "None"
      ),
      
      selectInput(
        "group_var",
        "Color / group variable",
        choices = "None",
        selected = "None"
      ),
      uiOutput("group_filter_ui"),
      
      selectInput(
        "facet_var",
        "Facet variable",
        choices = "None",
        selected = "None"
      ),
      
      textInput("plot_title", "Plot title", ""),
      textInput("x_label_custom", "X-axis label",""),
      textInput("y_label_custom","Y-axis label",""),
      
      h4("Axis limits"),
      
      fluidRow(
        column(
          width = 6,
          numericInput("x_min", "X min", value = NA)
        ),
        column(
          width = 6,
          numericInput("x_max", "X max", value = NA)
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          numericInput("y_min", "Y min", value = NA)
        ),
        column(
          width = 6,
          numericInput("y_max", "Y max", value = NA)
        )
      ),
      
      checkboxInput(
        "add_smooth",
        "Add regression line",
        value = FALSE
      ),
      
      hr(),
      
      numericInput(
        "plot_width",
        "Width (inches)",
        value = 7,
        min = 1
      ),
      
      numericInput(
        "plot_height",
        "Height (inches)",
        value = 5,
        min = 1
      ),
      
      numericInput(
        "plot_dpi",
        "Resolution (dpi)",
        value = 300,
        min = 72
      ),
      
      downloadButton("download_plot", "Download plot")
    ),
    
    mainPanel(
      
      h3("Data preview"),
      tableOutput("data_preview"),
      
      hr(),
      
      tabsetPanel(
        
        tabPanel(
          "Plot preview",
          br(),
          
          h4(""),
          plotOutput("plot", height = "650px"),
          
          hr(),
          
          h4("Summary statistics"),
          p("Statistics for the dependent variable shown in the plot."),
          tableOutput("summary_stats")
        ),
        
        tabPanel(
          "R code",
          br(),
          p(
            "This code assumes your imported data are stored in the object ",
            code("dat"),
            ". Change the variable name if needed."
          ),
          verbatimTextOutput("plot_code"),
          br(),
          fluidRow(
            column(
              width = 2,
              actionButton("copy_code", "Copy")
            ),
            column(
              width = 3,
              downloadButton("download_code", "Download R code")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---------- import data ----------
  
  uploaded_data <- reactive({
    
    req(input$file)
    
    bruceR::import(
      input$file$datapath,
      as = "data.frame"
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
  
  # ---------- preview imported data ----------
  
  output$data_preview <- renderTable({
    
    if (is.null(input$file)) {
      return(data.frame(Message = "No data imported"))
    }
    
    head(uploaded_data(), 10)
  })
  
  # ---------- create plot ----------
  plot_data <- reactive({
    
    req(input$file)
    
    dat <- uploaded_data()
    
    x_var <- if (is.null(input$x_var)) "None" else input$x_var
    group_var <- if (is.null(input$group_var)) "None" else input$group_var
    
    has_x <- x_var != "None" && x_var %in% names(dat)
    has_group <- group_var != "None" && group_var %in% names(dat)
    
    exclude_x_levels <- input$exclude_x_levels %||% character(0)
    exclude_groups <- input$exclude_groups %||% character(0)
    
    # Remove selected X levels only when X is treated as categorical
    if (has_x &&
        isTRUE(input$x_as_factor) &&
        length(exclude_x_levels) > 0) {
      
      dat <- dat[
        !as.character(dat[[x_var]]) %in% exclude_x_levels,
        ,
        drop = FALSE
      ]
    }
    
    # Remove selected group levels
    if (has_group && length(exclude_groups) > 0) {
      
      dat <- dat[
        !as.character(dat[[group_var]]) %in% exclude_groups,
        ,
        drop = FALSE
      ]
    }
    
    dat
  })
  
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
    
    # Remove selected X levels before converting X to a factor
    exclude_x_levels <- input$exclude_x_levels %||% character(0)
    
    if (has_x &&
        isTRUE(input$x_as_factor) &&
        length(exclude_x_levels) > 0) {
      
      dat <- dat[
        !as.character(dat[[x_var]]) %in% exclude_x_levels,
        ,
        drop = FALSE
      ]
    }
    
    # Remove selected group levels
    exclude_groups <- input$exclude_groups %||% character(0)
    
    if (has_group && length(exclude_groups) > 0) {
      
      dat <- dat[
        !as.character(dat[[group_var]]) %in% exclude_groups,
        ,
        drop = FALSE
      ]
    }
    
    # ---------- temporary plotting variables ----------
    
    x_var_plot <- NULL
    group_var_plot <- NULL
    facet_var_plot <- NULL
    
    if (has_x) {
      
      if (input$x_as_factor) {
        dat$.plot_x <- factor(dat[[x_var]])
        x_var_plot <- ".plot_x"
      } else {
        x_var_plot <- x_var
      }
    }
    
    if (has_group) {
      dat$.plot_group <- factor(dat[[group_var]])
      group_var_plot <- ".plot_group"
    }
    
    if (has_facet) {
      dat$.plot_facet <- factor(dat[[facet_var]])
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
        
        if (!has_group) {
          
          p <- ggplot(
            dat,
            aes(
              x = .data[[".single_box"]],
              y = .data[[y_var]]
            )
          ) +
            geom_boxplot()
          
        } else {
          
          p <- ggplot(
            dat,
            aes(
              x = .data[[".single_box"]],
              y = .data[[y_var]],
              color = .data[[group_var_plot]]
            )
          ) +
            geom_boxplot()
        }
        
      } else {
        
        if (!has_group) {
          
          p <- ggplot(
            dat,
            aes(
              x = .data[[x_var_plot]],
              y = .data[[y_var]]
            )
          ) +
            geom_boxplot()
          
        } else {
          
          p <- ggplot(
            dat,
            aes(
              x = .data[[x_var_plot]],
              y = .data[[y_var]],
              color = .data[[group_var_plot]]
            )
          ) +
            geom_boxplot() 
        }
      } 
      p <- p+ geom_point(position = position_jitterdodge(jitter.width = 0.08,dodge.width = 0.75),
                         size = 1.5,alpha = 0.3)
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
      p <- p +
        facet_wrap(
          vars(.data[[facet_var_plot]])
        )
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
      
      if (input$x_as_factor) {
        paste0(x_var, " (factor)")
      } else {
        x_var
      }
    }
    
    y_label <- if (input$plot_type %in% c("Histogram")) {
      "Count"
    } else {
      y_var
    }
    
    if (nzchar(input$x_label_custom)) {
      x_label <- input$x_label_custom
    }
    
    if (nzchar(input$y_label_custom)) {
      y_label <- input$y_label_custom
    }
    group_label <- if (has_group) group_var else NULL
    
    # ----- axis limits -----
    
    x_limits <- c(input$x_min, input$x_max)
    y_limits <- c(input$y_min, input$y_max)
    
    # Replace missing limits with NA_real_
    x_limits[is.na(x_limits)] <- NA_real_
    y_limits[is.na(y_limits)] <- NA_real_
    
    # Categorical X variables should not receive numeric X limits
    can_limit_x <- has_x &&
      !isTRUE(input$x_as_factor) &&
      is.numeric(dat[[x_var]])
    
    if (can_limit_x || any(!is.na(y_limits))) {
      p <- p +
        coord_cartesian(
          xlim = if (can_limit_x) x_limits else NULL,
          ylim = y_limits
        )
    }
    
    p +
      labs(
        title = input$plot_title,
        x = x_label,
        y = y_label,
        color = group_label,
        fill = group_label
      ) +
      theme_minimal(base_size = 14)
  })
  
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
          Message = "Select a numeric Y variable to show mean, median, and range."
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
    
    safe_min <- function(x) {
      if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
    }
    
    safe_max <- function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
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
        Min = safe_min(dat[[y_var]]),
        Max = safe_max(dat[[y_var]]),
        Range = safe_range(dat[[y_var]])
      )
      
    } else {
      
      result <- dat %>%
        group_by(across(all_of(group_columns))) %>%
        summarise(
          N = sum(!is.na(.data[[y_var]])),
          Mean = safe_mean(.data[[y_var]]),
          Median = safe_median(.data[[y_var]]),
          Min = safe_min(.data[[y_var]]),
          Max = safe_max(.data[[y_var]]),
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
      dat = uploaded_data()
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
      paste0("my_plot_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      ggsave(
        filename = file,
        plot = plot_object(),
        width = input$plot_width,
        height = input$plot_height,
        dpi = input$plot_dpi
      )
    }
  )
  
  ## download R codes for the plot
  output$plot_code <- renderText({
    plot_code()
  })
  
  observeEvent(input$copy_code, {
    
    session$sendCustomMessage(
      "copyToClipboard",
      list(code = plot_code())
    )
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