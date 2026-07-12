required_packages <- c(
  "bruceR",
  "bslib",
  "shiny",
  "ggplot2",
  "dplyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))


source("plot_code.R")

ui <- page_navbar(
  
  # ============================================================
  # App title and visual theme
  # ============================================================
  
  title = div(
    tags$strong("BEEP"),
    tags$span(
      " — Build Efficient and Elegant Plots",
      class = "d-none d-md-inline",
      style = "
        font-size: 0.82em;
        font-weight: normal;
      "
    )
  ),
  
  window_title = "BEEP",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  
  navbar_options = navbar_options(
    bg = "#343A40",
    inverse = TRUE,
    underline = FALSE
  ),
  
  # ============================================================
  # CSS and JavaScript
  # ============================================================
  
  header = tags$head(
    
    tags$style(
      HTML("
        
        /* Top navigation bar */
        .navbar {
          min-height: 54px;
          padding-top: 0.35rem;
          padding-bottom: 0.35rem;
        }
        
        .navbar-brand {
          font-size: 1.25rem;
          font-weight: 600;
        }
        
        .navbar-nav .nav-link {
          font-size: 1rem;
          padding-left: 0.85rem !important;
          padding-right: 0.85rem !important;
        }
        
        /* Main page spacing */
        .bslib-page-navbar > .container-fluid {
          padding-top: 1rem;
          padding-bottom: 1rem;
        }
        
        /* Plot preview width */
        .beep-plot-container {
          width: 100%;
          max-width: 950px;
          margin-left: auto;
          margin-right: auto;
        }
        
        /* Slightly softer card appearance */
        .card {
          border-radius: 0.45rem;
        }
        
        /* Full-width download button */
        .beep-download-button {
          width: 100%;
        }
        
        /* More compact accordion controls */
        .accordion-body {
          padding: 1rem;
        }
        
        /* Code output area */
        .beep-code-output pre {
          max-height: 650px;
          overflow-y: auto;
          white-space: pre-wrap;
        }
        
      ")
    ),
    
    tags$script(
      HTML("
        
        Shiny.addCustomMessageHandler(
          'copyToClipboard',
          function(message) {
            
            navigator.clipboard
              .writeText(message.code)
              .then(function() {
                
                $('#copy_code').text('Copied!');
                
                setTimeout(function() {
                  $('#copy_code').text('Copy code');
                }, 1500);
                
              });
          }
        );
        
      ")
    )
  ),
  
  # ============================================================
  # 1. Plot builder page
  # ============================================================
  
  nav_panel(
    "Plot builder",
    
    layout_sidebar(
      
      # --------------------------------------------------------
      # Control sidebar
      # --------------------------------------------------------
      
      sidebar = sidebar(
        width = 360,
        open = "always",
        
        accordion(
          id = "plot_controls",
          open = "plot_section",
          multiple = TRUE,
          
          # ====================================================
          # Data section
          # ====================================================
          
          accordion_panel(
            title = "1. Data",
            value = "data_section",
            
            fileInput(
              inputId = "file",
              label = "Upload data",
              accept = c(
                ".csv",
                ".xlsx",
                ".xls",
                ".rds"
              )
            ),
            
            
            tags$small(
              "Supported formats: csv, xlsx, xls and rds.",
              class = "text-muted"
            )
          ),
          
          # ====================================================
          # Plot-building section
          # ====================================================
          
          accordion_panel(
            title = "2. Plot building",
            value = "plot_section",
            
            selectInput(
              inputId = "plot_type",
              label = "Plot type",
              choices = c(
                "Scatterplot",
                "Boxplot",
                "Bar plot",
                "Histogram",
                "Line plot"
              ),
              selected = "Scatterplot"
            ),
            
            selectInput(
              inputId = "x_var",
              label = "X variable",
              choices = "None",
              selected = "None"
            ),
            
            checkboxInput(
              inputId = "x_as_factor",
              label = "Treat X variable as a factor",
              value = FALSE
            ),
            
            uiOutput("x_filter_ui"),
            
            selectInput(
              inputId = "y_var",
              label = "Y variable",
              choices = "None",
              selected = "None"
            ),
            
            selectInput(
              inputId = "group_var",
              label = "Color / group variable",
              choices = "None",
              selected = "None"
            ),
            
            uiOutput("group_filter_ui"),
            
            selectInput(
              inputId = "facet_var",
              label = "Facet variable",
              choices = "None",
              selected = "None"
            ),
            
            uiOutput("facet_filter_ui"),
            
            checkboxInput(
              inputId = "add_smooth",
              label = "Add regression line",
              value = FALSE
            )
          ),
          
          # ====================================================
          # Refinement section
          # ====================================================
          
          accordion_panel(
            title = "3. Refinement",
            value = "refinement_section",
            
            tags$strong("Text and labels"),
            
            textInput(
              inputId = "plot_title",
              label = "Plot title",
              value = ""
            ),
            
            textInput(
              inputId = "x_label_custom",
              label = "X-axis label",
              value = ""
            ),
            
            textInput(
              inputId = "y_label_custom",
              label = "Y-axis label",
              value = ""
            ),
            
            textInput(
              inputId = "legend_title",
              label = "Legend title",
              value = ""
            ),
            
            tags$hr(),
            
            tags$strong("Axis limits"),
            
            fluidRow(
              column(
                width = 6,
                
                numericInput(
                  inputId = "x_min",
                  label = "X minimum",
                  value = NA_real_
                )
              ),
              
              column(
                width = 6,
                
                numericInput(
                  inputId = "x_max",
                  label = "X maximum",
                  value = NA_real_
                )
              )
            ),
            
            fluidRow(
              column(
                width = 6,
                
                numericInput(
                  inputId = "y_min",
                  label = "Y minimum",
                  value = NA_real_
                )
              ),
              
              column(
                width = 6,
                
                numericInput(
                  inputId = "y_max",
                  label = "Y maximum",
                  value = NA_real_
                )
              )
            ),
            
            tags$hr(),
            
            tags$strong("Appearance"),
            
            selectInput(
              inputId = "plot_theme",
              label = "Plot theme",
              choices = c(
                "Minimal" = "minimal",
                "Classic" = "classic",
                "Black and white" = "bw",
                "Light" = "light"
              ),
              selected = "minimal"
            ),
            
            uiOutput("group_refinement_ui")
          ),
          
          # ====================================================
          # Download section
          # ====================================================
          
          accordion_panel(
            title = "4. Download",
            value = "download_section",
            
            selectInput(
              inputId = "plot_format",
              label = "Download format",
              choices = c(
                "PNG" = "png",
                "TIFF" = "tiff",
                "PDF" = "pdf",
                "SVG" = "svg",
                "JPEG" = "jpeg"
              ),
              selected = "png"
            ),
            
            tags$small(
              "PNG is the default format.",
              class = "text-muted"
            ),
            
            tags$br(),
            tags$br(),
            
            fluidRow(
              column(
                width = 6,
                
                numericInput(
                  inputId = "plot_width",
                  label = "Width (inches)",
                  value = 7,
                  min = 1,
                  step = 0.5
                )
              ),
              
              column(
                width = 6,
                
                numericInput(
                  inputId = "plot_height",
                  label = "Height (inches)",
                  value = 5,
                  min = 1,
                  step = 0.5
                )
              )
            ),
            
            numericInput(
              inputId = "plot_dpi",
              label = "Resolution (dpi)",
              value = 300,
              min = 72,
              step = 1
            ),
            
            tags$small(
              "DPI applies mainly to PNG, TIFF, and JPEG files.",
              class = "text-muted"
            ),
            
            tags$br(),
            tags$br(),
            
            downloadButton(
              outputId = "download_plot",
              label = "Download plot",
              class = "btn-primary beep-download-button"
            )
          )
        )
      ),
      
      # --------------------------------------------------------
      # Plot and summary area
      # --------------------------------------------------------
      
      div(
        class = "d-flex flex-column gap-3",
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Figure Preview"),
            )
          ),
          
          div(
            class = "beep-plot-container",
            
            plotOutput(
              outputId = "plot",
              height = "520px"
            )
          )
        ),
        
        card(
          card_header(
            tags$strong("Summary statistics")
          ),
          
          tags$p(
            "Statistics for the dependent variable shown in the plot.",
            class = "text-muted"
          ),
          
          tableOutput("summary_stats")
        )
      )
    )
  ),
  
  # ============================================================
  # 2. Data preview page
  # ============================================================
  
  nav_panel(
    "Data preview",
    
    card(
      full_screen = TRUE,
      
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          
          tags$strong("Imported data"),
          
          tags$small(
            "The first rows of the uploaded dataset are displayed.",
            class = "text-muted"
          )
        )
      ),
      
      tableOutput("data_preview")
    )
  ),
  
  # ============================================================
  # 3. R code page
  # ============================================================
  
  nav_panel(
    "R code",
    
    card(
      full_screen = TRUE,
      
      card_header(
        tags$strong("R code")
      ),
      
      tags$p(
        "The following code assumes your imported data are stored in the object", code("dat"),
            ". Change the variable name if needed.",
        class = "text-muted"
      ),
      
      div(
        class = "beep-code-output",
        verbatimTextOutput("plot_code")
      ),
      
      card_footer(
        div(
          class = "d-flex flex-wrap gap-2",
          
          actionButton(
            inputId = "copy_code",
            label = "Copy code"
          ),
          
          downloadButton(
            outputId = "download_code",
            label = "Download R code"
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
    
    head(uploaded_data(), 10)
  })
  
  # ---------- create plot ----------
  plot_data <- reactive({
    
    req(input$file)
    
    dat <- uploaded_data()
    
    x_var <- input$x_var %||% "None"
    group_var <- input$group_var %||% "None"
    facet_var <- input$facet_var %||% "None"
    
    has_x <- x_var != "None" && x_var %in% names(dat)
    has_group <- group_var != "None" && group_var %in% names(dat)
    has_facet <- facet_var != "None" && facet_var %in% names(dat)
    
    exclude_x_levels <- input$exclude_x_levels %||% character(0)
    exclude_groups <- input$exclude_groups %||% character(0)
    exclude_facets <- input$exclude_facets %||% character(0)
    
    # Remove selected X levels only when X is treated as categorical
    if (has_x && isTRUE(input$x_as_factor) && length(exclude_x_levels) > 0) {
      dat <- dat|> dplyr::filter(!as.character(.data[[x_var]]) %in% exclude_x_levels)
    }
    
    # Remove selected group levels
    if (has_group && length(exclude_groups) > 0) {
      dat <- dat|> dplyr::filter(!as.character(.data[[group_var]]) %in% exclude_groups)
    }

    # Exclude selected facet levels
    if (has_facet && length(exclude_facets) > 0) {
      dat <- dat|> dplyr::filter(!as.character(.data[[facet_var]]) %in% exclude_facets)
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

    # Remove selected facet levels
    exclude_facets <- input$exclude_facets %||% character(0)
    
    if (has_facet && length(exclude_facets) > 0) {
      
      dat <- dat[
        !as.character(dat[[facet_var]]) %in% exclude_facets,
        ,
        drop = FALSE
      ]
    }

    validate(need(nrow(dat) > 0, "No observations remain after applying the exclusions."))
    
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
