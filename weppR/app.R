library(shiny)
library(ggplot2)
library(shinythemes)

# --- 1. THE PHYSICS ENGINE (WEPP LOGIC) ---
# Ki = Interrill Erodibility, Kr = Rill Erodibility
calc_wepp_erosion <- function(rain_int, duration, slope, k_inter, k_rill, vwc, cover) {
    
    # A. HYDROLOGY: Simplified Green-Ampt
    # If VWC is high, infiltration capacity is lower
    inf_capacity <- 15 * (1 - vwc/100) 
    runoff_rate <- max(0, rain_int - inf_capacity)
    total_runoff_vol <- runoff_rate * (duration / 60) # mm
    
    # B. INTERRILL EROSION (Raindrop Impact)
    # Di = Ki * I * q (where I is intensity, q is runoff)
    interrill_loss <- k_inter * rain_int * runoff_rate * (1 - cover/100)
    
    # C. RILL EROSION (Flowing Water)
    # Dr = Kr * (Shear Stress - Critical Shear)
    shear_stress <- 9800 * (total_runoff_vol/1000) * (slope/100)
    rill_loss <- max(0, k_rill * (shear_stress - 1)) * (1 - cover/100)
    
    # Total Sediment Yield
    return(list(
        total = round(interrill_loss + rill_loss, 4),
        runoff = round(total_runoff_vol, 2)
    ))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("R-WEPP: Process-Based Erosion Simulator"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Climate & Topography"),
            sliderInput("rain_int", "Rain Intensity (mm/hr):", 10, 150, 60),
            sliderInput("duration", "Storm Duration (min):", 10, 120, 30),
            sliderInput("slope", "Slope Gradient (%):", 0, 45, 10),
            
            hr(),
            h4("Soil Erodibility (WEPP Params)"),
            numericInput("k_inter", "Interrill Erodibility (Ki):", 0.002, step = 0.001),
            numericInput("k_rill", "Rill Erodibility (Kr):", 0.005, step = 0.001),
            sliderInput("vwc", "Initial Soil Moisture (%):", 0, 100, 20),
            
            hr(),
            h4("Management"),
            sliderInput("cover", "Surface Cover/Residue (%):", 0, 100, 20)
        ),
        
        mainPanel(
            fluidRow(
                column(6, wellPanel(h4("Total Runoff"), h3(textOutput("runoff_out"), "mm"))),
                column(6, wellPanel(h4("Sediment Yield"), h3(textOutput("sed_out"), "kg/m²")))
            ),
            plotOutput("erosion_dist_plot"),
            helpText("This model simulates the split between Interrill (splash) and Rill (flow) erosion, similar to the USDA-WEPP logic.")
        )
    )
)

# --- 3. THE SERVER ---
server <- function(input, output) {
    
    results <- reactive({
        calc_wepp_erosion(input$rain_int, input$duration, input$slope, 
                          input$k_inter, input$k_rill, input$vwc, input$cover)
    })
    
    output$runoff_out <- renderText({ results()$runoff })
    output$sed_out <- renderText({ results()$total })
    
    output$erosion_dist_plot <- renderPlot({
        # Visualize how erosion changes with slope length (Conceptual)
        distance <- seq(0, 100, by = 5)
        # WEPP typically shows increasing erosion as you move down a slope
        loss_over_dist <- results()$total * (distance / 50)^1.5 
        
        ggplot(data.frame(x=distance, y=loss_over_dist), aes(x, y)) +
            geom_area(fill = "brown", alpha = 0.6) +
            labs(title = "Sediment Accumulation Down-Slope",
                 x = "Distance from Top of Hill (m)", y = "Cumulative Sediment (kg)") +
            theme_minimal()
    })
}

shinyApp(ui, server)