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
            # Use fluidRow and column for better layout control
            # column(12, ...) makes it take the full width of the mainPanel
            # offset = 0 means no offset from the left.
            # You can try using offset for manual centering if the below doesn't work perfectly.
            fluidRow(
                column(12, align = "center", # 'align = "center"' for column content
                       plotOutput("profilePlot", width = "90%") # Give plotOutput a specific width
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
    }, res = 96) # Added 'res' argument for better PNG resolution in Shiny
}

# ---
# Run the application
shinyApp(ui = ui, server = server)