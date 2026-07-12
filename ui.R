#### this file stores the ui for BEEP ####
ui <- page_navbar(
  
  # ============================================================
  # App title and visual theme
  # ============================================================
  
  title = div(
    tags$strong("BEEP (Build Efficient and Elegant Plots)"),
    tags$span(
      " ",
      class = "d-none d-md-inline",
      style = "
        font-size: 0.5em;
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
      
      /* Add space between title and navigation tabs */
      .navbar .navbar-nav {
        margin-left: 5rem;
      }
      
      .navbar-nav .nav-link {
        font-size: 1rem;
        padding-left: 0.85rem !important;
        padding-right: 0.85rem !important;
      }
      
      @media (max-width: 992px) {
        .navbar .navbar-nav {
          margin-left: 0;
        }
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
    "Single plot builder",
    
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
