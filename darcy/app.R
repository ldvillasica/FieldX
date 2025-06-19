# app.R

# Load the shiny package
library(shiny)
library(ggplot2) # For plotting (optional, but good for visualization)

# Define the UI (User Interface) for the application
ui <- fluidPage(
    
    # Application title
    titlePanel("Soil Water Flow Modeling (Darcy's Law)"),
    
    # Sidebar with input widgets
    sidebarLayout(
        sidebarPanel(
            h3("Input Parameters"),
            
            # Input: Hydraulic Conductivity (K)
            # K is typically in units of length/time (e.g., cm/day)
            sliderInput("hydraulic_conductivity",
                        "Hydraulic Conductivity (K, cm/day):",
                        min = 0.1,
                        max = 1000,
                        value = 50, # A typical value for some soils
                        step = 0.1),
            
            # Input: Pressure Head Difference (ΔH)
            # ΔH is dimensionless when calculated from height difference, or in length units if directly given
            # Here, we'll assume it's a difference in height (cm)
            sliderInput("head_difference",
                        "Pressure Head Difference (ΔH, cm):",
                        min = 1,
                        max = 200,
                        value = 50,
                        step = 1),
            
            # Input: Length of Flow Path (L)
            # L is in length units (cm)
            sliderInput("flow_path_length",
                        "Length of Flow Path (L, cm):",
                        min = 1,
                        max = 500,
                        value = 100,
                        step = 1),
            
            # Area of flow (A) - not part of Darcy's velocity law, but needed for volumetric flow
            # If we want volumetric flow rate (Q), we need area. Darcy's Law often calculates velocity (q)
            # Let's add an area input for volumetric flow.
            sliderInput("cross_sectional_area",
                        "Cross-sectional Area (A, cm²):",
                        min = 1,
                        max = 1000,
                        value = 100,
                        step = 10)
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            h3("Results"),
            
            # Output: Display Darcy's Velocity
            h4("Darcy's Velocity (q):"),
            verbatimTextOutput("darcy_velocity"),
            p("Darcy's Velocity (q) represents the apparent flow velocity through the porous medium, calculated as:"),
            tags$b("q = K * (ΔH / L)"), # Using HTML tags for bold text
            p("where:"),
            tags$ul(
                tags$li("K = Hydraulic Conductivity"),
                tags$li("ΔH = Pressure Head Difference"),
                tags$li("L = Length of Flow Path")
            ),
            br(), # Line break for spacing
            
            # Output: Display Volumetric Flow Rate
            h4("Volumetric Flow Rate (Q):"),
            verbatimTextOutput("volumetric_flow_rate"),
            p("The Volumetric Flow Rate (Q) is the actual volume of water flowing per unit time, calculated as:"),
            tags$b("Q = q * A"),
            p("where:"),
            tags$ul(
                tags$li("q = Darcy's Velocity"),
                tags$li("A = Cross-sectional Area")
            ),
            br(),
            
            # Output: A simple plot illustrating the concept
            h4("Conceptual Flow Representation"),
            plotOutput("flow_plot")
        )
    )
)

# Define the server logic
server <- function(input, output) {
    
    # Reactive expression to calculate Darcy's Velocity (q)
    # q = K * (ΔH / L)
    darcy_velocity_calc <- reactive({
        K <- input$hydraulic_conductivity
        delta_H <- input$head_difference
        L <- input$flow_path_length
        
        # Avoid division by zero if L is 0 (though slider min is 1)
        if (L == 0) return(0)
        
        q <- K * (delta_H / L)
        return(q)
    })
    
    # Reactive expression to calculate Volumetric Flow Rate (Q)
    # Q = q * A
    volumetric_flow_rate_calc <- reactive({
        q <- darcy_velocity_calc()
        A <- input$cross_sectional_area
        Q <- q * A
        return(Q)
    })
    
    # Output Darcy's Velocity
    output$darcy_velocity <- renderText({
        paste0(round(darcy_velocity_calc(), 4), " cm/day")
    })
    
    # Output Volumetric Flow Rate
    output$volumetric_flow_rate <- renderText({
        paste0(round(volumetric_flow_rate_calc(), 4), " cm³/day")
    })
    
    # Render a simple plot
    output$flow_plot <- renderPlot({
        q_val <- darcy_velocity_calc()
        K_val <- input$hydraulic_conductivity
        delta_H_val <- input$head_difference
        L_val <- input$flow_path_length
        
        # Create a simple data frame for plotting
        plot_data <- data.frame(
            x = c(0, L_val),
            y = c(delta_H_val, 0)
        )
        
        ggplot(plot_data, aes(x = x, y = y)) +
            geom_line(color = "blue", size = 1.5) +
            geom_point(color = "red", size = 3) +
            annotate("text", x = L_val / 2, y = delta_H_val / 2,
                     label = paste("Flow Direction -->"),
                     color = "darkgreen", size = 5, vjust = -1) +
            labs(title = "Conceptual Head Loss and Flow",
                 x = paste0("Flow Path Length (L = ", L_val, " cm)"),
                 y = paste0("Head (cm)\n(ΔH = ", delta_H_val, " cm)")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 10)) +
            xlim(0, L_val * 1.2) + # Adjust x-axis limits
            ylim(0, delta_H_val * 1.2) # Adjust y-axis limits
    })
}

# Run the application
shinyApp(ui = ui, server = server)