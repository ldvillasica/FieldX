# app.R

# Load the shiny package
library(shiny)
library(ggplot2) # For plotting

# Define the UI (User Interface) for the application
ui <- fluidPage(
    
    # Application title
    titlePanel("Soil Water Flow Modeling (Darcy's Law) - Layered Soil Profile"),
    
    # Sidebar with input widgets
    sidebarLayout(
        sidebarPanel(
            h3("Overall System Parameters"),
            
            # Input: Pressure Head Difference (ΔH) for the entire system
            numericInput("head_difference",
                         "Total Pressure Head Difference (ΔH, cm):",
                         value = 50,
                         min = 1,
                         max = 500,
                         step = 1),
            
            # Input: Cross-sectional Area (A) for the entire system
            numericInput("cross_sectional_area",
                         "Cross-sectional Area (A, cm²):",
                         value = 100,
                         min = 1,
                         max = 2000,
                         step = 10),
            
            hr(), # Horizontal rule for separation
            
            h3("Layer Parameters"),
            # Input: Number of soil layers
            numericInput("num_layers",
                         "Number of Soil Layers:",
                         value = 1,
                         min = 1,
                         max = 5,
                         step = 1),
            
            # Dynamic UI for layer-specific inputs
            uiOutput("layer_inputs")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            h3("Results for Layered System"),
            
            h4("Equivalent Hydraulic Conductivity (K_eq):"),
            verbatimTextOutput("equivalent_K"),
            p("The equivalent hydraulic conductivity for flow through layers in series is calculated as:"),
            tags$b("K_eq = (Σ L_i) / (Σ (L_i / K_i))"),
            br(),
            
            # Output: Display Darcy's Velocity for the entire system
            h4("Overall Darcy's Velocity (q):"),
            verbatimTextOutput("overall_darcy_velocity"),
            p("Overall Darcy's Velocity through the layered system:"),
            tags$b("q = K_eq * (ΔH / L_total)"),
            br(),
            
            # Output: Display Volumetric Flow Rate for the entire system
            h4("Overall Volumetric Flow Rate (Q):"),
            verbatimTextOutput("overall_volumetric_flow_rate"),
            p("Overall Volumetric Flow Rate through the layered system:"),
            tags$b("Q = q * A"),
            br(),
            
            h3("Layer-Specific Volumetric Water Content Considerations"),
            p("This section helps conceptualize the potential water storage within each layer based on its porosity and dimensions."),
            tableOutput("layer_water_storage"),
            br(),
            
            # Output: A simple plot illustrating the concept
            h4("Conceptual Flow Representation (Layered)"),
            plotOutput("flow_plot")
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    
    # Reactive expression to dynamically generate layer input fields
    output$layer_inputs <- renderUI({
        num_layers <- input$num_layers
        if (is.null(num_layers) || num_layers < 1) return(NULL)
        
        lapply(1:num_layers, function(i) {
            wellPanel( # wellPanel adds a subtle border and background
                h4(paste("Layer", i, "Properties")),
                numericInput(paste0("K_", i),
                             paste0("Hydraulic Conductivity (K", i, ", cm/day):"),
                             value = ifelse(i == 1, 50, ifelse(i == 2, 20, 100)), # Default values
                             min = 0.01, max = 1000, step = 0.01),
                numericInput(paste0("L_", i),
                             paste0("Length (L", i, ", cm):"),
                             value = 50, # Default length for each layer
                             min = 1, max = 200, step = 1),
                numericInput(paste0("Porosity_", i),
                             paste0("Porosity (φ", i, ", %):"),
                             value = 40, # Default porosity (e.g., 40%)
                             min = 1, max = 100, step = 1)
            )
        })
    })
    
    # Reactive expressions to collect all K, L, and Porosity values
    Ks <- reactive({
        num_layers <- input$num_layers
        sapply(1:num_layers, function(i) input[[paste0("K_", i)]])
    })
    
    Ls <- reactive({
        num_layers <- input$num_layers
        sapply(1:num_layers, function(i) input[[paste0("L_", i)]])
    })
    
    Porosities <- reactive({
        num_layers <- input$num_layers
        sapply(1:num_layers, function(i) input[[paste0("Porosity_", i)]] / 100) # Convert % to fraction
    })
    
    # Reactive expression to calculate Total Length (L_total)
    total_length_calc <- reactive({
        sum(Ls(), na.rm = TRUE)
    })
    
    # Reactive expression to calculate Equivalent Hydraulic Conductivity (K_eq)
    equivalent_K_calc <- reactive({
        Ks_val <- Ks()
        Ls_val <- Ls()
        
        # Handle potential division by zero or NA values
        if (any(Ls_val == 0, na.rm = TRUE) || any(is.na(Ks_val)) || any(Ks_val == 0, na.rm = TRUE)) {
            return(0) # Or handle as an error/message
        }
        
        sum_Li <- sum(Ls_val, na.rm = TRUE)
        sum_Li_Ki <- sum(Ls_val / Ks_val, na.rm = TRUE)
        
        if (sum_Li_Ki == 0) return(0) # Avoid division by zero
        
        K_eq <- sum_Li / sum_Li_Ki
        return(K_eq)
    })
    
    # Output Equivalent Hydraulic Conductivity
    output$equivalent_K <- renderText({
        paste0(round(equivalent_K_calc(), 4), " cm/day")
    })
    
    # Reactive expression to calculate Overall Darcy's Velocity (q)
    overall_darcy_velocity_calc <- reactive({
        K_eq <- equivalent_K_calc()
        delta_H <- input$head_difference
        L_total <- total_length_calc()
        
        if (L_total == 0) return(0)
        
        q <- K_eq * (delta_H / L_total)
        return(q)
    })
    
    # Output Overall Darcy's Velocity
    output$overall_darcy_velocity <- renderText({
        paste0(round(overall_darcy_velocity_calc(), 4), " cm/day")
    })
    
    # Reactive expression to calculate Overall Volumetric Flow Rate (Q)
    overall_volumetric_flow_rate_calc <- reactive({
        q <- overall_darcy_velocity_calc()
        A <- input$cross_sectional_area
        Q <- q * A
        return(Q)
    })
    
    # Output Overall Volumetric Flow Rate
    output$overall_volumetric_flow_rate <- renderText({
        paste0(round(overall_volumetric_flow_rate_calc(), 4), " cm³/day")
    })
    
    # Reactive expression for Layer Water Storage Table
    output$layer_water_storage <- renderTable({
        num_layers <- input$num_layers
        if (is.null(num_layers) || num_layers < 1) return(NULL)
        
        Ls_val <- Ls()
        Porosities_val <- Porosities()
        A_val <- input$cross_sectional_area
        
        if (any(is.na(Ls_val)) || any(is.na(Porosities_val))) return(NULL)
        
        # Calculate bulk volume and pore volume for each layer
        layer_data <- data.frame(
            Layer = 1:num_layers,
            Length_cm = Ls_val,
            Porosity_percent = Porosities() * 100, # Display as percentage
            Bulk_Volume_cm3 = Ls_val * A_val
        )
        layer_data$Pore_Volume_cm3 = layer_data$Bulk_Volume_cm3 * Porosities_val
        layer_data$Saturated_Water_Content_cm3_cm3 = Porosities_val # Volumetric water content at saturation
        
        return(layer_data)
    }, caption = "Layer Properties and Potential Water Storage (assuming saturation)", caption.placement = "top")
    
    
    # Render a simple plot illustrating the layered concept and head loss
    output$flow_plot <- renderPlot({
        Ls_val <- Ls()
        K_eq_val <- equivalent_K_calc()
        delta_H_val <- input$head_difference
        L_total_val <- total_length_calc()
        
        if (L_total_val == 0 || K_eq_val == 0) {
            # Provide a message if inputs are not valid for plotting
            ggplot() +
                geom_text(aes(x=0.5, y=0.5, label="Invalid inputs for plot generation.")) +
                theme_void() +
                theme(plot.title = element_text(hjust = 0.5))
        } else {
            # Calculate head loss across each layer
            q_overall <- overall_darcy_velocity_calc()
            head_losses <- numeric(length(Ls_val))
            current_head <- delta_H_val
            
            plot_points_x <- c(0)
            plot_points_y <- c(delta_H_val)
            
            # Calculate head drop for each layer and accumulate points for the plot
            for (i in seq_along(Ls_val)) {
                if (Ls_val[i] > 0 && Ks()[i] > 0) {
                    delta_h_i <- (q_overall * Ls_val[i]) / Ks()[i]
                    current_head <- current_head - delta_h_i
                } else {
                    delta_h_i <- 0 # No head loss if length or K is zero
                }
                head_losses[i] <- delta_h_i
                plot_points_x <- c(plot_points_x, sum(Ls_val[1:i]))
                plot_points_y <- c(plot_points_y, current_head)
            }
            
            plot_data <- data.frame(
                x = plot_points_x,
                y = plot_points_y
            )
            
            # Add layer boundaries to the plot
            layer_boundaries_x <- cumsum(Ls_val[-length(Ls_val)]) # Exclude total length
            layer_boundaries_y_top <- max(plot_data$y) * 1.1 # Extend line above plot
            layer_boundaries_y_bottom <- min(plot_data$y) * 0.9 # Extend line below plot
            
            ggplot(plot_data, aes(x = x, y = y)) +
                geom_line(color = "blue", size = 1.5) +
                geom_point(color = "red", size = 3) +
                # Add vertical lines for layer boundaries
                {if(length(layer_boundaries_x) > 0) geom_vline(xintercept = layer_boundaries_x, linetype = "dashed", color = "gray")} +
                labs(title = "Conceptual Head Loss and Flow Through Layers",
                     x = paste0("Flow Path Length (Total L = ", round(L_total_val, 2), " cm)"),
                     y = paste0("Head (cm)\n(Total ΔH = ", round(delta_H_val, 2), " cm)")) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10)) +
                xlim(0, L_total_val * 1.2) + # Adjust x-axis limits
                ylim(min(plot_points_y, 0) * 0.9, max(plot_points_y) * 1.1) # Adjust y-axis limits dynamically
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)