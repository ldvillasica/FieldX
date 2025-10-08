# Load required libraries
library(shiny)
library(ggplot2) # Kept for potential future 2D needs or basic styling, but primarily use plotly
library(plotly) # New library for 3D visualization

# Define UI for application
ui <- fluidPage(
    
    # Set the title and basic styling
    titlePanel("Custom Data Trajectory Plotter (3D)"),
    
    # Custom CSS for aesthetics
    tags$head(
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f8f9fa;
        color: #343a40;
      }
      .panel-title {
        color: #007bff;
        font-weight: 700;
        text-align: center;
        margin-bottom: 20px;
      }
      .well {
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 8px rgba(0,0,0,.1);
        padding: 20px;
      }
      .slider-label {
        font-weight: 600;
        margin-top: 10px;
        color: #212529;
      }
      /* Changed to accommodate plotly output */
      .shiny-html-output { 
        border: 1px solid #dee2e6;
        border-radius: 12px;
        padding: 10px;
        background-color: #ffffff;
      }
    "))
    ),
    
    # Layout with sidebar for controls and main area for the plot
    sidebarLayout(
        
        # Sidebar panel for user input (file upload and column mapping)
        sidebarPanel(
            div(class = "well", 
                h4("Upload Your Data", class = "slider-label"),
                p("Upload a CSV file containing the data for your three axes (X, Y, Z) plus an optional color/grouping column."),
                
                # File input for CSV
                fileInput("datafile", "Choose CSV File",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Placeholder for dynamically generated column selection UI
                h4("Map Data Columns to 3D Axes", class = "slider-label"),
                uiOutput("column_selectors") 
            )
        ),
        
        # Main panel for the output plot
        mainPanel(
            h3("Data-Driven Trajectory Plot (3D)", style = "text-align: center; color: #1557ad;"),
            # Changed from plotOutput to plotlyOutput
            plotlyOutput("phasePlot") 
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # 1. Reactive function to read the uploaded data
    user_data <- reactive({
        req(input$datafile)
        tryCatch({
            # Read CSV (assuming header is present)
            read.csv(input$datafile$datapath, header = TRUE)
        }, error = function(e) {
            # Handle potential file reading errors
            print(paste("Error reading file:", e$message))
            NULL
        })
    })
    
    # 2. Dynamic UI for column selection once data is uploaded
    output$column_selectors <- renderUI({
        df <- user_data()
        req(df)
        
        col_names <- names(df)
        
        list(
            # Select component for X-Axis
            selectInput("col_x", "Component 1 (X-Axis):", choices = col_names, selected = col_names[1]),
            # Select component for Y-Axis
            selectInput("col_y", "Component 2 (Y-Axis):", choices = col_names, selected = col_names[2]),
            # Select component for Z-Axis
            selectInput("col_z", "Component 3 (Z-Axis):", choices = col_names, selected = col_names[3]),
            # Optional: Select a column to use for coloring/grouping points
            selectInput("col_label", "Color/Group Points By (Optional):", choices = c("None", col_names), selected = "None")
        )
    })
    
    # 3. Reactive function to prepare the data for 3D plotting
    plot_data <- reactive({
        df <- user_data()
        # Require the selected columns
        req(df, input$col_x, input$col_y, input$col_z)
        
        # Ensure the required columns exist
        if (!all(c(input$col_x, input$col_y, input$col_z) %in% names(df))) {
            return(NULL) # Guard against errors
        }
        
        # Create the data frame for plotting (no conversion needed)
        data.frame(
            x = as.numeric(df[[input$col_x]]), # Ensure numeric for 3D axes
            y = as.numeric(df[[input$col_y]]),
            z = as.numeric(df[[input$col_z]]),
            label_col = if (input$col_label != "None") df[[input$col_label]] else factor(1:nrow(df)),
            stringsAsFactors = FALSE
        )
    })
    
    # 4. Generate the 3D Trajectory Plot using Plotly
    output$phasePlot <- renderPlotly({
        
        plot_df <- plot_data()
        req(plot_df)
        
        # Determine the color/grouping variable
        color_var <- if (input$col_label != "None") as.formula(~label_col) else NULL
        
        # Create the 3D plot
        p <- plot_ly(plot_df, 
                     x = ~x, 
                     y = ~y, 
                     z = ~z, 
                     color = color_var, # Color points based on selection
                     type = 'scatter3d', 
                     mode = 'lines+markers', # Show both the trajectory (lines) and individual points (markers)
                     marker = list(size = 5, opacity = 0.8),
                     line = list(width = 3)) %>%
            
            layout(scene = list(
                # Set axis titles dynamically
                xaxis = list(title = input$col_x, showgrid = TRUE),
                yaxis = list(title = input$col_y, showgrid = TRUE),
                zaxis = list(title = input$col_z, showgrid = TRUE),
                # Add interactive camera controls
                aspectmode = 'cube' # Ensures a visually balanced cube view
            ),
            # Set plot title
            title = "Soil State Trajectory in 3D Space",
            # Set plot font
            font = list(family = 'Inter'))
        
        # Print the plotly object
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)