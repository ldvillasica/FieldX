# app.R

# Load necessary packages
library(shiny)
library(aqp)
library(RColorBrewer) 
library(plyr) 
library(reshape) 
library(stringr)

# ---
# Custom function to wrap text manually based on character limit (width)
wrap_text_by_width <- function(text, width) {
    if (is.na(text) || text == "") return("")
    # Split the text into words
    words <- unlist(strsplit(text, " "))
    wrapped_lines <- c()
    current_line <- ""
    
    for (word in words) {
        # Check if adding the next word exceeds the width limit
        if (nchar(current_line) + nchar(word) + 1 > width) {
            # Start a new line
            wrapped_lines <- c(wrapped_lines, current_line)
            current_line <- word
        } else {
            # Append to current line
            if (current_line == "") {
                current_line <- word
            } else {
                current_line <- paste(current_line, word)
            }
        }
    }
    # Add the last line
    wrapped_lines <- c(wrapped_lines, current_line)
    
    # Combine lines with newline character
    return(paste(wrapped_lines, collapse = "\n"))
}
# ---

# ---
# Define UI for soil profile sketcher app
ui <- fluidPage(
    titlePanel("Soil Profile Sketcher: Adjustable Text Wrap"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            uiOutput("profile_selector"), 
            tags$hr(),
            
            # --- TEXT WRAPPING CONTROL ---
            numericInput("text_wrap_limit", 
                         "Text Wrap Width (Characters per Line):", 
                         value = 15, # Default value for a typical plot width
                         min = 5, max = 50),
            tags$hr(),
            
            # CSV parsing options
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                         selected = '"'),
            tags$hr(),
            
            # --- LABEL OFFSET CONTROL (Slider) ---
            sliderInput("label_offset", "Space Between Profile and Description (units):",
                        min = 0, max = 1.5, value = 0.5, step = 0.1),
            tags$hr(),
            
            # Plot sizing
            sliderInput("plotWidth", "Plot Width (pixels):",
                        min = 400, max = 1600, value = 900, step = 50),
            sliderInput("plotHeight", "Plot Height (pixels):",
                        min = 300, max = 1200, value = 700, step = 50),
            tags$hr(),
            helpText("Adjusting the wrap width and offset will affect the required plot width.")
        ),
        
        mainPanel(
            h3("Selected Profile Sketch with Clean Horizon Labels"),
            fluidRow(
                column(12, align = "center",
                       plotOutput("profilePlot", width = "auto", height = "auto")
                )
            )
        )
    )
)

# ---
# Define server logic to read file and plot
server <- function(input, output) {
    
    # 1. Load and Prepare Data (SPC Object)
    pedon_data <- reactive({
        req(input$file1)
        
        inFile <- input$file1
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
        
        required_cols <- c("Profile", "top", "bottom", "Hue", "Value", "Chroma", "Horizon.Name", "Description")
        if (!all(required_cols %in% names(df))) {
            stop(paste("Missing required columns. Ensure your CSV has:", paste(setdiff(required_cols, names(df)), collapse = ", ")))
        }
        
        # Apply wrapping to the Description column
        wrap_width <- input$text_wrap_limit
        df$WrappedDescription <- sapply(df$Description, wrap_text_by_width, width = wrap_width)
        
        # Concatenate the wrapped description with the Horizon Name
        df$CombinedLabel <- paste0(
            df$Horizon.Name, 
            "\n", 
            df$WrappedDescription
        )
        
        df$color <- munsell2rgb(df$Hue, df$Value, df$Chroma)
        depths(df) <- Profile ~ top + bottom
        
        return(df)
    })
    
    # 2. Dynamic Profile Selector UI
    output$profile_selector <- renderUI({
        pedon <- pedon_data()
        profile_names <- profile_id(pedon)
        selectInput("selectedProfile", "Select Soil Profile to View:", choices = profile_names, selected = profile_names[1])
    })
    
    # 3. Filter Data to Single Profile
    filtered_pedon <- reactive({
        pedon <- pedon_data()
        req(input$selectedProfile)
        return(pedon[profile_id(pedon) == input$selectedProfile, ])
    })
    
    # 4. Render Plot
    output$profilePlot <- renderPlot({
        pedon_single <- filtered_pedon()
        req(pedon_single)
        
        old_par <- par(no.readonly = TRUE)
        on.exit(par(old_par))
        
        # Use a standard margin
        par(mar = c(3, 1, 3, 1)) 
        
        # Calculate the required plot range (x-axis) to reserve space for the labels
        # min_xlim (start of plot) = 0.1 (standard)
        # Profile Width = 0.3 (fixed in plotSPC)
        # Required extra space = input$label_offset (user slider) + space for labels (1.5 units is ample)
        min_xlim <- 0.1
        max_xlim <- min_xlim + 0.3 + input$label_offset + 1.5
        
        plotSPC(
            pedon_single, 
            name = 'CombinedLabel',  # Use the new wrapped label column
            color = 'color',
            label = 'Profile',
            width = 0.3,
            cex.names = 0.7,
            
            # --- FIX: Set the plot's X-axis limits to reserve space ---
            plot.xlim = c(min_xlim, max_xlim), 
            
            # --- Use the slider value for the label offset ---
            label.x.offset = input$label_offset, 
            
            # --- FINAL PLOT ARGUMENTS ---
            depth.axis = FALSE,      
            hz.depths = TRUE,        
            name.style = 'center',   
            
            plot.class.stats = FALSE,
            fixLabelCollisions = TRUE
        )
        
        title(paste("Profile:", input$selectedProfile), line = 1.5, cex.main = 1.2)
    },
    res = 96,
    width = function() input$plotWidth,
    height = function() input$plotHeight
    )
}

# ---
# Run the application
shinyApp(ui = ui, server = server)