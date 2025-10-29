# This is a complete, single-file R Shiny application (app.R).
# It visualizes the relative dominance of 16 detailed soil-forming processes 
# using a Radar Chart, expanding the original concept to a more comprehensive list 
# derived from fundamental pedology (e.g., Boul et al., 1997).
# To run this, you must have the 'shiny', 'fmsb', and 'scales' packages installed in R.

library(shiny)
library(fmsb) # Used for the radarchart function
library(scales) # Used for transparency in plot colors

# --- 1. Define Constants and Initial Data ---

# Define the 16 fundamental pedogenic processes (axes for the radar chart)
# These cover the four general processes: Additions, Removals, Translocations, and Transformations.
PROCESS_LABELS <- c(
    "1. Organic Matter Addition (Litter/Root)",
    "2. Mineral Addition (Dust/Ash)",
    "3. Leaching (Base/Cation Removal)",
    "4. Erosion/Removal (Physical Loss)",
    "5. Clay Illuviation (Bt Horizon)",
    "6. Clay Eluviation (E Horizon)",
    "7. Fe/Al Illuviation (Podzolization)",
    "8. Organic Translocation (Podzolization)",
    "9. Chemical Weathering (Primary Mineral Breakdown)",
    "10. Humification (Organic Matter Decomp.)",
    "11. Calcification (CaCO3 Accumulation)",
    "12. Decalcification (CaCO3 Removal)",
    "13. Salinization (Soluble Salt Accumulation)",
    "14. Desalinization (Soluble Salt Removal)",
    "15. Gleying (Reduction/Redox)",
    "16. Rubefaction (Fe Oxide Reddening)"
)

NUM_PROCESSES <- length(PROCESS_LABELS)

# Define the initial Process Intensity Index (PII) values (0-10 scale)
# Must contain exactly 16 values
DEFAULT_PII <- c(7.0, 3.0, 8.5, 4.0, 7.5, 2.0, 3.0, 4.0, 9.0, 9.5, 5.0, 6.0, 2.5, 7.0, 4.5, 6.5)

# Named list for reference (used for individual inputs)
names(DEFAULT_PII) <- paste0("proc_", seq_len(NUM_PROCESSES))
DEFAULT_PASTE_STRING <- paste(DEFAULT_PII, collapse = ", ")

# AXIS_COLORS constant has been removed as requested.


# --- 2. User Interface (UI) Definition ---

ui <- fluidPage(
    # Custom CSS for aesthetics
    tags$head(
        tags$style(HTML("
            body { font-family: 'Inter', sans-serif; background-color: #f7f7f7; }
            .title { color: #1f2937; font-weight: 800; }
            .card { background-color: white; padding: 20px; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.06); }
            
            /* Legend styling adjusted for placement next to the chart */
            .legend-key { 
                margin-top: 10px; 
                padding: 10px;
                height: 600px; /* Match plot height */
                overflow-y: auto; /* Allow scrolling */
                border-left: 1px solid #e2e8f0; /* Add a subtle separator */
            }
            .legend-item {
                width: 100%; /* Single column stack */
                margin-bottom: 5px;
                font-size: 0.9em;
                line-height: 1.2;
            }
            /* Styling for the bold number (index) in the legend */
            .legend-item b {
                display: inline-block;
                min-width: 25px;
                font-weight: 700;
                color: #1f2937; /* Ensure number is a dark color for contrast */
            }
        "))
    ),
    
    # Application Title
    headerPanel(title = tags$div(class = "title", "16-Axis Pedogenic Process Radar")),
    
    # Layout: Sidebar for inputs, Main panel for visualization
    sidebarLayout(
        
        # Sidebar Panel: Inputs
        sidebarPanel(
            class = "card",
            width = 4,
            h3("Process Intensity Indices (PII)"),
            p(paste0("Input PII values (0-10) for the ", NUM_PROCESSES, " processes.")),
            
            # --- Profile Label Input ---
            textInput("profile_label", 
                      "Profile/Horizon Name (Title)", 
                      value = "Example Pedogenic Profile"),
            
            # Input Selection Method
            radioButtons("input_method", "Select Input Method:",
                         c("Individual Inputs" = "individual",
                           "Bulk Paste (Comma Separated)" = "paste")),
            
            # --- Conditional Input Panels ---
            
            # 1. Individual Inputs Panel
            conditionalPanel(
                condition = "input.input_method == 'individual'",
                div(class = "input-group-container",
                    lapply(seq_len(NUM_PROCESSES), function(i) {
                        process_name <- PROCESS_LABELS[i]
                        numericInput(
                            inputId = paste0("proc_", i), # Use simple, sequential ID
                            label = process_name,
                            value = DEFAULT_PII[i],
                            min = 0,
                            max = 10,
                            step = 0.1
                        )
                    })
                )
            ),
            
            # 2. Bulk Paste Panel
            conditionalPanel(
                condition = "input.input_method == 'paste'",
                div(class = "input-group-container",
                    p(paste0("Enter the ", NUM_PROCESSES, " PII values separated by commas (0-10 scale).")),
                    p(tags$strong("Order:"), paste(PROCESS_LABELS, collapse = " | ")),
                    textAreaInput("pii_paste_data", 
                                  "Paste 16 PII Values:", 
                                  value = DEFAULT_PASTE_STRING, 
                                  rows = 4)
                )
            )
        ),
        
        # Main Panel: Outputs
        mainPanel(
            width = 8,
            
            # Radar Chart Output and Legend (inside the card)
            div(class = "card",
                h3("16-Axis Pedogenic Dominance Radar Chart"),
                
                # Use fluidRow to place plot and legend side-by-side
                fluidRow(
                    # Left Column: Plot (7/12 width)
                    column(width = 7,
                           plotOutput("radarPlot", height = 650)
                    ),
                    
                    # Right Column: Legend Key (5/12 width)
                    column(width = 5,
                           div(class = "legend-key",
                               h4("Process Index (PII) Legend"),
                               tagList(
                                   lapply(seq_len(NUM_PROCESSES), function(i) {
                                       tags$div(class = "legend-item", 
                                                tags$b(paste0(i, ". ")), 
                                                gsub(paste0(i, ". "), "", PROCESS_LABELS[i])) # Remove the number from the description
                                   })
                               )
                           )
                    )
                )
            )
        )
    )
)

# --- 3. Server Logic Definition ---

server <- function(input, output, session) {
    
    # 3.1. Reactive function to gather and process data
    processData <- reactive({
        
        input_values <- NULL
        error_message <- NULL
        
        if (input$input_method == "individual") {
            # Collect data from individual numeric inputs using the simple IDs
            input_values <- sapply(seq_len(NUM_PROCESSES), function(i) {
                input_name <- paste0("proc_", i)
                input[[input_name]]
            })
            
        } else { # input$input_method == 'paste'
            # Collect and parse data from the text area
            input_string <- gsub("\\s+", "", input$pii_paste_data) # Remove spaces
            input_list <- strsplit(input_string, ",")[[1]]
            
            # Convert to numeric
            input_values <- as.numeric(input_list)
            
            # Error handling: Check count and numeric validity
            if (length(input_values) != NUM_PROCESSES || any(is.na(input_values))) {
                error_message <- paste0("Error: Please enter exactly ", NUM_PROCESSES, " comma-separated numeric values (0-10 scale).")
                return(list(data_df = NULL, error_message = error_message))
            }
            
            # Enforce 0-10 bounds 
            input_values <- sapply(input_values, function(x) max(0, min(10, x)))
        }
        
        # --- Format data for fmsb::radarchart ---
        
        # 1. Create a 3x16 matrix (3 rows for Max, Min, Profile; 16 columns for processes)
        matrix_data <- matrix(
            c(rep(10, NUM_PROCESSES), # Row 1: Max values
              rep(0, NUM_PROCESSES),  # Row 2: Min values
              input_values),          # Row 3: Profile values
            nrow = 3,
            byrow = TRUE # Fill the matrix row-wise
        )
        
        # 2. Convert to data frame
        data_df <- as.data.frame(matrix_data)
        
        # 3. Assign names correctly
        rownames(data_df) <- c("Max", "Min", "Profile")
        colnames(data_df) <- PROCESS_LABELS
        
        # Return the data frame needed for plotting
        return(list(
            data_df = data_df,
            error_message = NULL
        ))
    })
    
    # 3.2. Render the Radar Chart
    output$radarPlot <- renderPlot({
        data_list <- processData()
        
        # Check for error message before plotting
        if (!is.null(data_list$error_message)) {
            # Display error instead of plot
            plot.new()
            text(0.5, 0.5, data_list$error_message, col = "#dc2626", cex = 1.2) # Red-700
            return()
        }
        
        # --- PLOT THE RADAR CHART ---
        
        # 1. Prepare data for plotting
        plot_df <- data_list$data_df
        
        # 2. Rename column names to empty strings so radarchart draws NO labels. 
        # The column order is still 1 to 16, which dictates the axis order.
        colnames(plot_df) <- rep("", NUM_PROCESSES)
        
        # 3. Draw the radar chart with HIDDEN labels (vlcex = 0)
        radarchart(
            plot_df, 
            pcol = "#38a169", # Line color (green-600)
            pfcol = scales::alpha("#38a169", 0.3), # Fill color
            plwd = 3, 
            cglcol = "gray", 
            cglty = 1, 
            axislabcol = "gray", 
            caxislabels = rep("", 6), # SUPPRESS the 0-10 axis labels
            cglwd = 0.8, 
            vlcex = 0 # HIDE the default numbered labels
        )
        
        # 4. Calculate coordinates for custom numbered axis labels
        N <- NUM_PROCESSES
        radius <- 1.15 # Increased distance for labels
        
        # Calculate angles: 
        # Starts at pi/2 (North, 12 o'clock) and proceeds clockwise.
        angles_rad <- pi/2 - (0:(N-1)) * (2 * pi / N)
        
        # Convert polar to cartesian coordinates (x = r*cos(a), y = r*sin(a))
        x_coords <- radius * cos(angles_rad)
        y_coords <- radius * sin(angles_rad)
        
        # 5. Use text() to draw the numbered labels (1 to 16). 
        # FLIP: Reverse the labels sequence while keeping '1' at the starting (12 o'clock) position.
        # Sequence: 1, 16, 15, 14, ..., 3, 2
        labels_flipped <- as.character(c(1, N:2))
        
        text(x = x_coords, 
             y = y_coords, 
             labels = labels_flipped, # Use the flipped labels
             col = "#1f2937", # Neutral dark gray color 
             cex = 0.85) # Reduced size for better visual isolation
        
        # 6. Add the user-defined title/label
        title(main = input$profile_label, 
              cex.main = 1.4, 
              font.main = 2, 
              col.main = "#1f2937") 
        
    }, height = 650) 
}

# --- 4. Run the Application ---
shinyApp(ui = ui, server = server)