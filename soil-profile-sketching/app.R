# soil profile sketcher

# Load necessary packages
library(shiny)
library(aqp)
library(RColorBrewer)
library(latticeExtra)
library(plyr)
library(reshape)

# ---
# Define UI for soil profile sketcher app
ui <- fluidPage(
  titlePanel("Flexible Soil Profile Sketcher"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      # Sliders for controlling plot width and height
      sliderInput("plotWidth", "Plot Width (pixels):",
                  min = 400, max = 1600, value = 900, step = 50),
      sliderInput("plotHeight", "Plot Height (pixels):",
                  min = 300, max = 1200, value = 700, step = 50),
      tags$hr(),
      helpText(
        "Your CSV file must include columns named exactly:",
        tags$ul(
          tags$li("Profile"),
          tags$li("top"),
          tags$li("bottom"),
          tags$li("Hue"),
          tags$li("Value"),
          tags$li("Chroma"),
          tags$li("Horizon.Name")
        )
      )
    ),
    
    mainPanel(
      h3("Soil Profile Sketch with Depth Labeling"),
      fluidRow(
        column(12, align = "center",
               # Make plotOutput width and height reactive to slider inputs
               plotOutput("profilePlot",
                          width = "auto", # 'auto' or a percentage like '100%' is often good when renderPlot controls size
                          height = "auto") # 'auto' or a percentage like '100%'
        )
      )
    )
  )
)

# ---
# Define server logic to read file and plot
server <- function(input, output) {
  
  pedon_data <- reactive({
    req(input$file1) # Require a file to be uploaded
    
    inFile <- input$file1
    
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    # Basic validation for required columns
    required_cols <- c("Profile", "top", "bottom",
                       "Hue", "Value", "Chroma", "Horizon.Name")
    if (!all(required_cols %in% names(df))) {
      stop(paste("Missing required columns. Please ensure your CSV has:",
                 paste(required_cols, collapse = ", ")))
    }
    
    # Color convert munsell to rgb
    df$color <- munsell2rgb(df$Hue, df$Value, df$Chroma)
    
    # Add for label
    df <- within(df, label <- paste(Profile, Horizon.Name, sep=" , "))
    
    # Convert to SoilProfileCollection object using 'top' and 'bottom'
    depths(df) <- Profile ~ top + bottom
    return(df)
  })
  
  output$profilePlot <- renderPlot({
    pedon <- pedon_data()
    if (!is.null(pedon)) {
      # --- IMPORTANT: Adjust plot margins here ---
      # Save current par settings to restore them later (good practice)
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par)) # Ensure settings are restored when function exits
      
      # Adjust margins: c(bottom, left, top, right)
      # Reduce the left margin. You might need to experiment with values like 0, 1, 2
      par(mar = c(0.5, 0.5, 0.5, 0.5)) # Example: reduced left margin to 0.5 lines
      
      # The specific plot you requested
      plot(pedon,
           name = 'Horizon.Name',
           color = 'color',
           cex.names = .7,
           width = 0.3, # This 'width' controls the width of each profile in the plot
           depth.axis = FALSE,
           hz.depths = TRUE,
           fixLabelCollisions = TRUE,
           hz.depths.offset = 0.08)
    }
  },
  res = 96, # Added 'res' argument for better PNG resolution in Shiny
  width = function() input$plotWidth,  # Make width reactive to slider
  height = function() input$plotHeight # Make height reactive to slider
  )
}

# ---
# Run the application
shinyApp(ui = ui, server = server)