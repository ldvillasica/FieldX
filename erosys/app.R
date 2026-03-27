library(shiny)
library(ggplot2)

# --- 1. THE PHYSICS ENGINE ---
# This is where you will refine your "Better WEPP" math
predict_erosion <- function(rain_intensity, aggregate_stability, dispersible_clay) {
    # Hypothetical Model: High stability reduces erosion, high clay dispersion increases it
    # We use a non-linear decay for stability to simulate 'pore clogging' resistance
    base_erodibility <- (dispersible_clay / 100) * 10
    resistance_factor <- exp(-aggregate_stability / 50)
    
    total_loss <- rain_intensity * base_erodibility * resistance_factor
    return(total_loss)
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    titlePanel("WEPP 2.0: Soil Erosion Resistance Lab"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Adjust your experimental soil parameters below:"),
            
            sliderInput("rain", "Rainfall Intensity (mm/hr):", min = 0, max = 150, value = 50),
            hr(),
            sliderInput("stability", "Wet Aggregate Stability (%):", min = 0, max = 100, value = 60),
            sliderInput("wd_clay", "Water Dispersible Clay (%):", min = 0, max = 50, value = 10),
            
            br(),
            actionButton("save", "Save Result to Local Database", class = "btn-success")
        ),
        
        mainPanel(
            plotOutput("erosionPlot"),
            wellPanel(
                h4("Predicted Soil Loss Index:"),
                textOutput("lossValue")
            )
        )
    )
)

# --- 3. THE SERVER LOGIC ---
server <- function(input, output) {
    
    # Calculate prediction reactively
    results <- reactive({
        predict_erosion(input$rain, input$stability, input$wd_clay)
    })
    
    output$lossValue <- renderText({
        paste(round(results(), 2), "tons/ha (Estimated)")
    })
    
    output$erosionPlot <- renderPlot({
        # Generating a sensitivity curve
        stability_range <- seq(0, 100, by = 1)
        plot_data <- data.frame(
            Stability = stability_range,
            Loss = sapply(stability_range, function(s) predict_erosion(input$rain, s, input$wd_clay))
        )
        
        ggplot(plot_data, aes(x = Stability, y = Loss)) +
            geom_line(color = "blue", size = 1.2) +
            geom_vline(xintercept = input$stability, linetype = "dashed", color = "red") +
            labs(title = "Impact of Aggregate Stability on Soil Loss",
                 x = "Wet Aggregate Stability (%)",
                 y = "Predicted Erosion") +
            theme_minimal()
    })
}

shinyApp(ui = ui, server = server)