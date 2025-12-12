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
    words <- unlist(strsplit(text, " "))
    wrapped_lines <- c()
    current_line <- ""
    
    for (word in words) {
        if (nchar(current_line) + nchar(word) + 1 > width) {
            wrapped_lines <- c(wrapped_lines, current_line)
            current_line <- word
        } else {
            if (current_line == "") {
                current_line <- word
            } else {
                current_line <- paste(current_line, word)
            }
        }
    }
    wrapped_lines <- c(wrapped_lines, current_line)
    return(paste(wrapped_lines, collapse = "\n"))
}
# ---

# Define UI for soil profile sketcher app
ui <- fluidPage(
    titlePanel("Soil Profile Sketcher: Topography (Offset) + Distinctness (LTY) + Volume Fraction"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            
            # Select inputs for boundary features
            uiOutput("distinctness_column_selector"), 
            uiOutput("topography_column_selector"),
            
            # Select input for the new Volume Fraction feature
            uiOutput("volume_fraction_column_selector"),
            tags$hr(),
            
            uiOutput("profile_selector"), 
            tags$hr(),
            
            # --- BOUNDARY LEGEND INFO ---
            h4("Boundary Visualization"),
            helpText(
                "Distinctness (Line Type): A (Solid), C (Dashed), G (Dotted), D (Dot-Dash).", br(),
                "Topography (Offset): Draws a chevron pattern.", br(),
                "**Lines are colored white for maximum contrast.**"
            ),
            
            # --- VOLUME FRACTION INFO ---
            h4("Volume Fraction Visualization"),
            helpText(
                "The selected column's percentage (0-100) is displayed as **white dots** (mottles/fragments) inside the horizon.", br(),
                "Higher percentage = higher density of dots."
            ),
            tags$hr(),
            
            # --- TEXT WRAPPING CONTROL ---
            numericInput("text_wrap_limit", 
                         "Text Wrap Width (Characters per Line):", 
                         value = 15, 
                         min = 5, max = 50),
            tags$hr(),
            
            # --- LABEL FONT SIZE CONTROL ---
            sliderInput("label_font_size", "Horizon Label Font Size (cex):",
                        min = 0.5, max = 1.5, value = 0.7, step = 0.05),
            tags$hr(),
            
            # CSV parsing options
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            tags$hr(),
            
            # --- LABEL OFFSET CONTROL (Slider) ---
            sliderInput("label_offset", "Space Between Profile and Description (units):",
                        min = 0, max = 5.0, value = 0.5, step = 0.1),
            tags$hr(),
            
            # Plot sizing
            sliderInput("plotWidth", "Plot Width (pixels):",
                        min = 400, max = 1600, value = 900, step = 50),
            sliderInput("plotHeight", "Plot Height (pixels):",
                        min = 300, max = 1200, value = 700, step = 50)
        ),
        
        mainPanel(
            h3("Selected Profile Sketch with Encoded Features"),
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
server <- function(input, output, session) {
    
    # 1. Load and Prepare Data (SPC Object)
    pedon_data_raw <- reactive({
        req(input$file1)
        
        inFile <- input$file1
        # Added input$quote to the read.csv call for robustness
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep) 
        
        required_cols <- c("Profile", "top", "bottom", "Hue", "Value", "Chroma", "Horizon.Name", "Description")
        
        if (!all(required_cols %in% names(df))) {
            stop(paste("Missing required columns. Ensure your CSV has:", paste(setdiff(required_cols, names(df)), collapse = ", ")))
        }
        
        return(df)
    })
    
    # Dynamic Column Selector for Distinctness
    output$distinctness_column_selector <- renderUI({
        df <- pedon_data_raw()
        choices <- names(df)
        selected_col <- intersect(choices, c("Distinctness", "DistinctnessCode"))[1]
        
        selectInput("distinctnessCol", "Select Horizon Distinctness Column:", 
                    choices = choices, 
                    selected = if(is.na(selected_col)) choices[1] else selected_col)
    })
    
    # Dynamic Column Selector for Topography
    output$topography_column_selector <- renderUI({
        df <- pedon_data_raw()
        choices <- names(df)
        selected_col <- intersect(choices, c("Topography", "Boundary.Topography"))[1]
        
        selectInput("topographyCol", "Select Horizon Topography Column:", 
                    choices = choices, 
                    selected = if(is.na(selected_col)) choices[1] else selected_col)
    })
    
    # Dynamic Column Selector for Volume Fraction (NEW)
    output$volume_fraction_column_selector <- renderUI({
        df <- pedon_data_raw()
        choices <- names(df)
        # Suggest common names for mottles/fragments
        selected_col <- intersect(choices, c("VolumeFraction", "RockFragmentFraction", "MottleFraction"))[1]
        
        selectInput("volFractionCol", "Select Volume Fraction Column (%):", 
                    choices = choices, 
                    selected = if(is.na(selected_col)) choices[1] else selected_col)
    })
    
    pedon_data_prepared <- reactive({
        df <- pedon_data_raw()
        req(input$distinctnessCol, input$topographyCol, input$volFractionCol)
        
        # 1. Prepare DISTINCTNESS (Map codes to LTY)
        df$DistinctnessCode <- df[[input$distinctnessCol]]
        
        lty_map <- c(
            'A' = 1, # Abrupt -> Solid
            'C' = 2, # Clear -> Dashed
            'G' = 3, # Gradual -> Dotted
            'D' = 4  # Diffuse -> Dot-Dash
        )
        df$distinctness.lty <- lty_map[as.character(df$DistinctnessCode)]
        df$distinctness.lty <- as.numeric(df$distinctness.lty) 
        df$distinctness.lty[is.na(df$distinctness.lty)] <- 1 # Default to solid
        
        # 2. Prepare TOPOGRAPHY (Map codes to numeric offset for Chevrons)
        df$TopographyCode <- df[[input$topographyCol]]
        df$ht_offset <- hzTopographyCodeToOffset(df$TopographyCode)
        
        # 3. SET LINE COLOR TO WHITE FOR CONTRAST
        df$boundary_color <- 'white'
        
        # 4. Prepare VOLUME FRACTION (NEW)
        # Rename the selected column to a standard aqp name for addVolumeFraction()
        df$A.coord <- df[[input$volFractionCol]]
        df$A.coord <- pmax(0, pmin(100, df$A.coord)) # Clamp values between 0 and 100
        df$A.coord[is.na(df$A.coord)] <- 0 # Treat NA as 0% fraction
        
        # 5. Prepare Plot Labels and Colors
        wrap_width <- input$text_wrap_limit
        df$WrappedDescription <- sapply(df$Description, wrap_text_by_width, width = wrap_width)
        
        df$CombinedLabel <- paste0(df$Horizon.Name, "\n", df$WrappedDescription)
        df$color <- munsell2rgb(df$Hue, df$Value, df$Chroma)
        
        # Create SPC object
        depths(df) <- Profile ~ top + bottom
        
        return(df)
    })
    
    # 2. Dynamic Profile Selector UI
    output$profile_selector <- renderUI({
        pedon <- pedon_data_prepared()
        profile_names <- profile_id(pedon)
        selectInput("selectedProfile", "Select Soil Profile to View:", choices = profile_names, selected = profile_names[1])
    })
    
    # 3. Filter Data to Single Profile
    filtered_pedon <- reactive({
        pedon <- pedon_data_prepared()
        req(input$selectedProfile)
        return(pedon[profile_id(pedon) == input$selectedProfile, ])
    })
    
    # 4. Render Plot
    output$profilePlot <- renderPlot({
        pedon_single <- filtered_pedon()
        req(pedon_single)
        
        old_par <- par(no.readonly = TRUE)
        on.exit(par(old_par))
        
        par(mar = c(3, 1, 3, 1)) 
        
        text_start_x <- 0.1 + 0.3 + input$label_offset
        min_xlim <- 0.1
        max_xlim <- text_start_x + 2.0 
        
        # 1. Plot the base profile and boundaries
        plotSPC(
            pedon_single, 
            name = 'CombinedLabel', 
            color = 'color',
            label = 'Profile',
            width = 0.3,
            
            # --- TOPOGRAPHY: OFFSET ENCODING (Chevrons) ---
            hz.topography.offset = 'ht_offset', 
            
            # --- DISTINCTNESS: LTY ENCODING (Line Type) ---
            lty.hz = 1, 
            lwd.hz = 1, 
            hz.boundary.lty = 'distinctness.lty', 
            lwd.col = 'boundary_color', 
            
            # Set p.shape/p.interval to NULL to avoid conflict with addVolumeFraction
            p.shape = NULL,
            p.interval = NULL,
            
            cex.names = input$label_font_size, 
            label.x = text_start_x, 
            plot.xlim = c(min_xlim, max_xlim), 
            
            depth.axis = FALSE, 
            hz.depths = TRUE, 
            
            plot.class.stats = FALSE,
            fixLabelCollisions = TRUE
        )
        
        # 2. Add the volumetric fraction visualization (White dots/patterns)
        # 'A.coord' is the column containing the 0-100% fraction data.
        addVolumeFraction(pedon_single, colname = 'A.coord', col = 'white')
        
        # 3. Add Title and Legend
        title(paste("Profile:", input$selectedProfile), line = 1.5, cex.main = 1.2)
        
        # Legend for Distinctness (LTY)
        legend('topleft', 
               legend = c('Abrupt (A): Solid', 'Clear (C): Dashed', 'Gradual (G): Dotted', 'Diffuse (D): Dot-Dash'),
               lty = c(1, 2, 3, 4),
               lwd = 1, 
               col = 'black', 
               bty = 'n',
               cex = 0.8
        )
    },
    res = 96,
    width = function() input$plotWidth,
    height = function() input$plotHeight
    )
}

# ---
# Run the application
shinyApp(ui = ui, server = server)