library(shiny)
library(ggplot2)
library(dplyr)
library(scales) 

# --- 1. Define Constants, Data, and COLORS ---

# Define the labels and ensure the ID numbers are prepended to the process names
PROCESS_LABELS_FULL <- c(
    "1. Organic Matter Addition (Litter/Root)", "2. Mineral Addition (Dust/Ash)",
    "3. Leaching (Base/Cation Removal)", "4. Erosion/Removal (Physical Loss)",
    "5. Clay Illuviation (Bt Horizon)", "6. Clay Eluviation (E Horizon)",
    "7. Fe/Al Illuviation (Podzolization)", "8. Organic Translocation (Podzolization)",
    "9. Chemical Weathering (Primary Mineral Breakdown)", "10. Humification (Organic Matter Decomp.)",
    "11. Calcification (CaCO3 Accumulation)", "12. Decalcification (CaCO3 Removal)",
    "13. Salinization (Soluble Salt Accumulation)", "14. Desalinization (Soluble Salt Removal)",
    "15. Gleying (Reduction/Redox)", "16. Rubefaction (Fe Oxide Reddening)"
)

NUM_PROCESSES <- length(PROCESS_LABELS_FULL)
DEFAULT_PII <- c(7.0, 3.0, 8.5, 4.0, 7.5, 2.0, 3.0, 4.0, 9.0, 9.5, 5.0, 6.0, 2.5, 7.0, 4.5, 6.5)
names(DEFAULT_PII) <- paste0("proc_", seq_len(NUM_PROCESSES))
DEFAULT_PASTE_STRING <- paste(DEFAULT_PII, collapse = ", ")

# --- UNIQUE COLORS FOR 16 PROCESSES ---
PROCESS_COLORS <- c(
    "#E63946", "#F7B801", "#2A9D8F", "#1D3557",  
    "#8D99AE", "#FFC3A0", "#457B9D", "#A8DADC",  
    "#264653", "#FF6B6B", "#FFD166", "#06D6A0",  
    "#8338EC", "#FB5607", "#3D405B", "#90A878"   
)

# Create a named vector for color mapping (Name = Process Label, Value = Color Hex)
COLOR_MAP <- setNames(PROCESS_COLORS, PROCESS_LABELS_FULL)

MAX_BAR_Y <- 10.0 
# Position for the axis numbers, slightly outside MAX_BAR_Y
NUMBER_POSITION <- MAX_BAR_Y + 0.5 
PLOT_YMAX <- MAX_BAR_Y + 1.0 

# --- 2. User Interface (UI) Definition (No Change) ---

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            body { font-family: 'Inter', sans-serif; background-color: #f7f7f7; }
            .title { color: #1f2937; font-weight: 800; }
            .card { background-color: white; padding: 20px; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.06); }
            #radialBarPlot { margin-bottom: 20px; } 
        "))
    ),
    
    headerPanel(title = tags$div(class = "title", "16-Axis Pedogenic Process Radial Bar Plot")),
    
    sidebarLayout(
        
        # Sidebar Panel: Inputs
        sidebarPanel(
            class = "card",
            width = 4,
            h3("Process Intensity Indices (PII)"),
            p("Input PII values (0-10) for the 16 processes."),
            
            textInput("profile_label", 
                      "Profile/Horizon Name (Title)", 
                      value = "Example Pedogenic Profile"),
            
            radioButtons("input_method", "Select Input Method:",
                         c("Individual Inputs" = "individual",
                           "Bulk Paste (Comma Separated)" = "paste")),
            
            conditionalPanel(
                condition = "input.input_method == 'individual'",
                div(class = "input-group-container",
                    lapply(seq_len(NUM_PROCESSES), function(i) {
                        numericInput(
                            inputId = paste0("proc_", i), 
                            label = PROCESS_LABELS_FULL[i],
                            value = DEFAULT_PII[i],
                            min = 0, max = 10, step = 0.1
                        )
                    })
                )
            ),
            
            conditionalPanel(
                condition = "input.input_method == 'paste'",
                div(class = "input-group-container",
                    p(tags$strong("Order:"), paste(PROCESS_LABELS_FULL, collapse = " | ")),
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
            div(class = "card",
                h3("16-Axis Pedogenic Dominance Radial Bar Plot"),
                plotOutput("radialBarPlot", height = 700)
            )
        )
    )
)

# --- 3. Server Logic Definition (WITH AXIS NUMBERS) ---

server <- function(input, output, session) {
    
    # 3.1. Reactive function to gather and process data
    processData <- reactive({
        
        input_values <- NULL
        error_message <- NULL
        
        if (input$input_method == "individual") {
            input_values <- sapply(seq_len(NUM_PROCESSES), function(i) {
                input[[paste0("proc_", i)]]
            })
        } else {
            input_string <- gsub("\\s+", "", input$pii_paste_data)
            input_list <- strsplit(input_string, ",")[[1]]
            input_values <- as.numeric(input_list)
            
            if (length(input_values) != NUM_PROCESSES || any(is.na(input_values))) {
                error_message <- paste0("Error: Please enter exactly ", NUM_PROCESSES, " comma-separated numeric values (0-10 scale).")
                return(list(data_df = NULL, error_message = error_message))
            }
            
            input_values <- sapply(input_values, function(x) max(0, min(10, x)))
        }
        
        # --- Format data for ggplot2 Radial Bar Plot ---
        
        # 1. Create the data frame
        data_df <- data.frame(
            ID_Num = 1:NUM_PROCESSES,
            Process = PROCESS_LABELS_FULL,
            PII = input_values,
            Color = PROCESS_COLORS
        )
        
        # 2. Set factor levels for ID and Process to ensure correct order in plot and legend
        data_df <- data_df %>%
            mutate(
                ID = factor(ID_Num, levels = 1:NUM_PROCESSES),
                Process = factor(Process, levels = PROCESS_LABELS_FULL)
            )
        
        return(list(
            data_df = data_df,
            error_message = NULL
        ))
    })
    
    # 3.2. Render the Radial Bar Plot
    output$radialBarPlot <- renderPlot({
        data_list <- processData()
        
        if (!is.null(data_list$error_message)) {
            plot.new()
            text(0.5, 0.5, data_list$error_message, col = "#dc2626", cex = 1.2)
            return()
        }
        
        plot_df <- data_list$data_df
        
        # Map the factor ID for the X-axis for geom_bar
        ggplot(plot_df, aes(x = ID, y = PII)) +
            
            # 1. Background full-circle bar (Max PII = 10)
            geom_bar(
                aes(y = MAX_BAR_Y), 
                stat = "identity", 
                fill = alpha("gray90", 0.6), 
                width = 0.8, 
                color = "transparent"
            ) +
            
            # 2. Actual PII value bar, mapped to the Process factor for color and legend
            geom_bar(
                aes(fill = Process), 
                stat = "identity", 
                width = 0.8, 
                color = "transparent"
            ) +
            
            # 3. Add the Process Numbers (1-16)
            geom_text(
                aes(x = ID, y = NUMBER_POSITION, label = ID),
                color = "#1f2937", 
                size = 3.5, 
                # Use hjust=0.5 (center) and angle=0 (perpendicular) for consistent, clean placement
                angle = 0, 
                hjust = 0.5 
            ) +
            
            # 4. Apply the custom 16-color palette
            scale_fill_manual(
                values = COLOR_MAP,
                name = "Pedogenic Process Intensity Index (PII)"
            ) +
            
            # 5. Convert to Polar Coordinates (Counter-clockwise)
            coord_polar(start = 0, direction = -1) + 
            
            # 6. Customize the look
            ylim(c(0, PLOT_YMAX)) + 
            theme_minimal() + 
            theme(
                # Remove axis labels and text that previously held the labels
                axis.title = element_blank(),
                axis.text = element_blank(), 
                
                # Keep the radial grid lines
                panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
                panel.grid.minor = element_blank(),
                
                # Plot title customization
                plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#1f2937"),
                panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA),
                
                # Legend customization
                legend.position = "right", 
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 10),
                legend.key.size = unit(0.5, "cm")
            ) +
            
            # 7. Add the user-defined title
            labs(title = input$profile_label)
        
    }, height = 700)
}

# --- 8. Run the Application ---
shinyApp(ui = ui, server = server)