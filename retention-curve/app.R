# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# --- Data Extraction ---
# Manually extract the data from the provided image table.
# These parameters are for the van Genuchten model (theta_r, theta_s, alpha, n).
# Although psi_e, lambda, and b are also present, we will primarily use van Genuchten parameters for this app.
soil_data <- tibble::tribble(
    ~Textural_class, ~theta_r, ~theta_s, ~psi_e, ~lambda, ~alpha, ~n, ~b,
    "Clay",          0.098,    0.459,   -37.3,  0.131,   0.0150, 1.25, 7.6,
    "C loam",        0.079,    0.442,   -25.9,  0.194,   0.0158, 1.42, 5.2,
    "Loam",          0.061,    0.399,   -11.2,  0.220,   0.0111, 1.47, 4.5,
    "L Sand",        0.049,    0.390,   -8.69,  0.474,   0.0348, 1.75, 2.1,
    "Sand",          0.053,    0.375,   -7.26,  0.592,   0.0352, 3.18, 1.7,
    "S Clay",        0.117,    0.385,   -29.2,  0.168,   0.0334, 1.21, 6.0,
    "S C L",         0.063,    0.384,   -28.1,  0.250,   0.0211, 1.33, 4.0,
    "S loam",        0.039,    0.387,   -14.7,  0.322,   0.0267, 1.45, 4.7,
    "Silt",          0.050,    0.489,   NA,     NA,      0.0066, 1.68, NA, # Missing values as per table
    "Si Clay",       0.111,    0.481,   -34.2,  0.127,   0.0162, 1.32, 7.9,
    "Si C L",        0.090,    0.482,   -32.6,  0.151,   0.0084, 1.52, 6.6,
    "Si Loam",       0.065,    0.439,   -20.8,  0.211,   0.0051, 1.66, 4.7
)

# --- Van Genuchten Model Function ---
# Calculate volumetric water content (theta) given matric potential (h)
# and van Genuchten parameters (theta_r, theta_s, alpha, n).
# h is typically negative for unsaturated conditions, so we use its absolute value.
# Note: h should be in cm for consistency with the 'alpha' parameter (cm^-1).
calculate_theta_vg <- function(h, theta_r, theta_s, alpha, n) {
    # Ensure h is treated as positive for the equation's power term
    h_abs <- abs(h)
    
    # Check for valid parameters to avoid NaN or Inf in calculation
    if (is.na(theta_r) || is.na(theta_s) || is.na(alpha) || is.na(n) || n <= 1) {
        return(NA) # Return NA if parameters are invalid or missing
    }
    
    m <- 1 - (1 / n)
    theta <- theta_r + (theta_s - theta_r) * (1 + (alpha * h_abs)^n)^(-m)
    return(theta)
}

# --- Standard Potential Values ---
# Common matric potential points converted to mm of water head (negative values for suction)
# FC: Field Capacity, approximately -33 kPa = -330 cm = -3300 mm
# PWP: Permanent Wilting Point, approximately -1500 kPa = -15000 cm = -150000 mm
FC_mm <- -3300
PWP_mm <- -150000

# --- Shiny UI ---
ui <- fluidPage(
    # Application title
    titlePanel("Soil Water Retention Curve Analyzer (Van Genuchten Model)"),
    
    # Sidebar layout with input and output sections
    sidebarLayout(
        sidebarPanel(
            # Dropdown to select textural class
            selectInput("textural_class",
                        "Select Textural Class:",
                        choices = soil_data$Textural_class,
                        selected = "Loam"),
            
            hr(), # Horizontal rule for separation
            
            # Radio buttons for selecting potential input source
            radioButtons("potential_source", "Select Potential Input:",
                         choices = c("Custom Potential (mm)",
                                     "Field Capacity (-33 kPa)",
                                     "Permanent Wilting Point (-1500 kPa)"),
                         selected = "Custom Potential (mm)"),
            
            # Conditional numeric input for custom matric potential in mm
            conditionalPanel(
                condition = "input.potential_source == 'Custom Potential (mm)'",
                numericInput("custom_potential_mm", "Enter Custom Matric Potential (mm):",
                             value = -1000, min = PWP_mm, max = -1, step = 1)
            ),
            
            hr(), # Horizontal rule for separation
            
            # Display selected parameters
            h4("Selected Parameters:"),
            tableOutput("selected_params_table")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            # Display calculated volumetric water content
            h3("Calculated Volumetric Water Content (θ):"),
            verbatimTextOutput("water_content_output"),
            
            # Plot of the soil water retention curve
            h3("Soil Water Retention Curve:"),
            plotOutput("water_retention_plot")
        )
    )
)

# --- Shiny Server ---
server <- function(input, output) {
    
    # Reactive expression to filter data based on selected textural class
    selected_soil_params <- reactive({
        soil_data %>% filter(Textural_class == input$textural_class)
    })
    
    # Render the table of selected parameters
    output$selected_params_table <- renderTable({
        selected_soil_params() %>%
            select(Textural_class, theta_r, theta_s, alpha, n) # Show only relevant VG parameters
    })
    
    # Reactive expression to determine the matric potential in mm based on user selection
    current_matric_potential_mm <- reactive({
        if (input$potential_source == "Custom Potential (mm)") {
            input$custom_potential_mm
        } else if (input$potential_source == "Field Capacity (-33 kPa)") {
            FC_mm
        } else if (input$potential_source == "Permanent Wilting Point (-1500 kPa)") {
            PWP_mm
        } else {
            # Default or fallback value if none selected (shouldn't happen with radioButtons)
            -1000 # Default to -100 cm = -1000 mm
        }
    })
    
    # Convert the current matric potential from mm to cm for calculation
    current_matric_potential_cm <- reactive({
        current_matric_potential_mm() / 10
    })
    
    # Calculate and display the volumetric water content for the given matric potential
    output$water_content_output <- renderPrint({
        params <- selected_soil_params()
        h_input_mm <- current_matric_potential_mm()
        h_input_cm <- current_matric_potential_cm()
        
        if (nrow(params) == 0 || is.na(params$theta_r) || is.na(params$theta_s) || is.na(params$alpha) || is.na(params$n)) {
            "Please select a valid textural class with complete parameters."
        } else {
            theta_calculated <- calculate_theta_vg(
                h = h_input_cm, # Pass the value in cm to the function
                theta_r = params$theta_r,
                theta_s = params$theta_s,
                alpha = params$alpha,
                n = params$n
            )
            if (is.na(theta_calculated)) {
                "Error: Could not calculate water content due to missing or invalid parameters."
            } else {
                paste("Volumetric Water Content (θ) at", h_input_mm, "mm =", round(theta_calculated, 4), "cm³/cm³")
            }
        }
    })
    
    # Generate and render the soil water retention curve plot
    output$water_retention_plot <- renderPlot({
        params <- selected_soil_params()
        
        if (nrow(params) == 0 || is.na(params$theta_r) || is.na(params$theta_s) || is.na(params$alpha) || is.na(params$n)) {
            return(NULL) # Don't plot if parameters are invalid
        }
        
        # Define a range of matric potentials in mm for plotting the curve
        # Convert this range to cm for the calculate_theta_vg function
        h_values_mm <- seq(from = -1, to = PWP_mm, length.out = 500) # From very wet to very dry
        h_values_cm <- h_values_mm / 10 # Convert to cm
        
        # Calculate theta for each matric potential in the range
        curve_data <- data.frame(
            Matric_Potential_cm = h_values_cm, # Store in cm for calculation
            Volumetric_Water_Content = sapply(h_values_cm, function(h_cm) {
                calculate_theta_vg(
                    h = h_cm, # Pass the value in cm to the function
                    theta_r = params$theta_r,
                    theta_s = params$theta_s,
                    alpha = params$alpha,
                    n = params$n
                )
            })
        )
        
        # Plotting the curve
        ggplot(curve_data, aes(x = abs(Matric_Potential_cm), y = Volumetric_Water_Content)) +
            geom_line(color = "darkblue", size = 1.2) +
            # Highlight the current input point (converted to cm for plotting)
            geom_point(aes(x = abs(current_matric_potential_cm()),
                           y = calculate_theta_vg(current_matric_potential_cm(), params$theta_r, params$theta_s, params$alpha, params$n)),
                       color = "red", size = 4, shape = 18) +
            scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000), labels = c("1", "10", "100", "1000", "10000", "100000")) + # Log scale for better visualization, adjusted for larger range
            labs(title = paste("Soil Water Retention Curve for", input$textural_class),
                 x = "Absolute Matric Potential (cm, log scale)",
                 y = "Volumetric Water Content (cm³/cm³)") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 10))
    })
}

# --- Run the application ---
shinyApp(ui = ui, server = server)