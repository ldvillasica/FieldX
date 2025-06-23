# app.R
library(shiny)
library(dplyr) # For data manipulation

ui <- fluidPage(
    titlePanel("Fertilizer Calculator"),
    
    sidebarLayout(
        sidebarPanel(
            h3("1. Target Nutrient Requirements (kg/hectare)"),
            numericInput("req_N", "Required Nitrogen (N):", value = 100, min = 0),
            numericInput("req_P", "Required Phosphorus (P2O5):", value = 50, min = 0),
            numericInput("req_K", "Required Potassium (K2O):", value = 60, min = 0),
            hr(),
            
            h3("2. Area of Application"),
            numericInput("area_value", "Area Value:", value = 1, min = 0.01),
            selectInput("area_unit", "Area Unit:",
                        choices = c("Hectares" = "ha", "Square Meters" = "sqm"),
                        selected = "ha"),
            hr(),
            
            h3("3. Define Your Fertilizers (50 kg/bag)"),
            actionButton("add_fertilizer", "Add Another Fertilizer"),
            div(id = "fertilizer_inputs",
                # Initial 3 fertilizer inputs
                wellPanel(
                    h5("Fertilizer 1"),
                    textInput("fert_name_1", "Name:", value = "Urea"),
                    numericInput("fert_N_1", "N %:", value = 46, min = 0, max = 100),
                    numericInput("fert_P_1", "P2O5 %:", value = 0, min = 0, max = 100),
                    numericInput("fert_K_1", "K2O %:", value = 0, min = 0, max = 100),
                    numericInput("fert_price_1", "Price (PHP/50kg bag):", value = 1500, min = 0)
                ),
                wellPanel(
                    h5("Fertilizer 2"),
                    textInput("fert_name_2", "Name:", value = "DAP"),
                    numericInput("fert_N_2", "N %:", value = 18, min = 0, max = 100),
                    numericInput("fert_P_2", "P2O5 %:", value = 46, min = 0, max = 100),
                    numericInput("fert_K_2", "K2O %:", value = 0, min = 0, max = 100),
                    numericInput("fert_price_2", "Price (PHP/50kg bag):", value = 2000, min = 0)
                ),
                wellPanel(
                    h5("Fertilizer 3"),
                    textInput("fert_name_3", "Name:", value = "MOP"),
                    numericInput("fert_N_3", "N %:", value = 0, min = 0, max = 100),
                    numericInput("fert_P_3", "P2O5 %:", value = 0, min = 0, max = 100),
                    numericInput("fert_K_3", "K2O %:", value = 60, min = 0, max = 100),
                    numericInput("fert_price_3", "Price (PHP/50kg bag):", value = 1800, min = 0)
                )
            ),
            hr(),
            
            h3("4. User-Defined Fertilizer Mix"),
            p("Enter the number of 50kg bags you plan to use for each fertilizer below."),
            div(id = "mix_inputs",
                uiOutput("mix_inputs_ui") # Use uiOutput to render dynamically
            )
        ),
        
        mainPanel(
            h3("Calculation Results"),
            h4("Total Nutrients Required for Defined Area:"),
            tableOutput("total_nutrient_req_table"),
            hr(),
            
            h4("A. Individual Fertilizer Calculation (If used to supply a primary nutrient alone):"),
            p("This table shows how much of each fertilizer you'd need if you used it to meet the N, P2O5, or K2O requirement for which it's primarily suited (e.g., Urea for N, DAP for P2O5, MOP for K2O). This is for comparison only, not a balanced recommendation."),
            tableOutput("individual_fert_calc_table"),
            strong("Total Cost for Individual Fertilizers:"),
            textOutput("individual_total_cost"), # Now correctly calculates total
            hr(),
            
            h4("B. Your Defined Fertilizer Mix:"),
            tableOutput("mix_summary_table"),
            tableOutput("mix_npk_supplied_table"),
            h4("Estimated Total Cost of Your Mix:"),
            textOutput("mix_total_cost")
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive value to keep track of the number of fertilizers
    rv <- reactiveValues(num_fert = 3) # Start with 3
    
    # Observe button click to add more fertilizers
    observeEvent(input$add_fertilizer, {
        rv$num_fert <- rv$num_fert + 1
        new_fert_num <- rv$num_fert
        
        insertUI(
            selector = "#fertilizer_inputs",
            where = "beforeEnd",
            ui = wellPanel(
                id = paste0("fert_panel_", new_fert_num),
                h5(paste0("Fertilizer ", new_fert_num)),
                textInput(paste0("fert_name_", new_fert_num), "Name:", value = ""),
                numericInput(paste0("fert_N_", new_fert_num), "N %:", value = 0, min = 0, max = 100),
                numericInput(paste0("fert_P_", new_fert_num), "P2O5 %:", value = 0, min = 0, max = 100),
                numericInput(paste0("fert_K_", new_fert_num), "K2O %:", value = 0, min = 0, max = 100),
                numericInput(paste0("fert_price_", new_fert_num), "Price (PHP/50kg bag):", value = 0, min = 0)
            )
        )
    })
    
    
    # --- Reactive data for all fertilizers ---
    fertilizers_data <- reactive({
        all_fert <- lapply(1:rv$num_fert, function(i) {
            fert_name_val <- input[[paste0("fert_name_", i)]]
            
            if (!is.null(fert_name_val) && fert_name_val != "") {
                # Retrieve numeric inputs and convert to numeric, replacing NA with 0
                n_val <- as.numeric(input[[paste0("fert_N_", i)]])
                p_val <- as.numeric(input[[paste0("fert_P_", i)]])
                k_val <- as.numeric(input[[paste0("fert_K_", i)]])
                price_val <- as.numeric(input[[paste0("fert_price_", i)]])
                
                data.frame(
                    Name = fert_name_val,
                    N_perc = ifelse(is.na(n_val), 0, n_val) / 100,
                    P_perc = ifelse(is.na(p_val), 0, p_val) / 100,
                    K_perc = ifelse(is.na(k_val), 0, k_val) / 100,
                    Price_per_bag = ifelse(is.na(price_val), 0, price_val),
                    Bag_Weight_kg = 50
                )
            } else {
                NULL # Return NULL for empty or non-existent inputs
            }
        })
        # Filter out NULLs and bind rows to create a single data frame
        bind_rows(all_fert[!sapply(all_fert, is.null)])
    })
    
    
    # --- Dynamic UI for User-Defined Mix inputs ---
    output$mix_inputs_ui <- renderUI({
        fert_data <- fertilizers_data()
        req(nrow(fert_data) > 0) # Require that at least one fertilizer is defined
        
        lapply(1:nrow(fert_data), function(i) {
            numericInput(
                paste0("mix_bags_", i),
                paste0("Bags of ", fert_data$Name[i], " (50kg):"),
                value = 0, min = 0
            )
        })
    })
    
    
    # --- Reactive: Total Nutrient Requirements for the Area ---
    total_nutrient_req <- reactive({
        # Ensure these critical inputs are present
        req(input$req_N, input$req_P, input$req_K, input$area_value)
        
        # Handle NULL/NA values for main requirements if user clears them
        req_N_val <- as.numeric(input$req_N)
        req_P_val <- as.numeric(input$req_P)
        req_K_val <- as.numeric(input$req_K)
        area_value_val <- as.numeric(input$area_value)
        
        req_N_val <- ifelse(is.na(req_N_val), 0, req_N_val)
        req_P_val <- ifelse(is.na(req_P_val), 0, req_P_val)
        req_K_val <- ifelse(is.na(req_K_val), 0, req_K_val)
        area_value_val <- ifelse(is.na(area_value_val), 0, area_value_val)
        
        area_factor <- if (input$area_unit == "ha") area_value_val else area_value_val / 10000 # 1 ha = 10,000 sqm
        
        df <- data.frame(
            Nutrient = c("N", "P2O5", "K2O"),
            Requirement_kg_per_ha = c(req_N_val, req_P_val, req_K_val)
        ) %>%
            mutate(Total_Requirement_kg = Requirement_kg_per_ha * area_factor)
        
        df
    })
    
    output$total_nutrient_req_table <- renderTable({
        # Ensure the reactive producing this data is ready
        req(total_nutrient_req())
        total_nutrient_req() %>%
            rename(
                `Nutient (kg/ha)` = Requirement_kg_per_ha,
                `Total Requirement (kg) for Area` = Total_Requirement_kg
            )
    }, striped = TRUE, bordered = TRUE, digits = 2)
    
    
    # --- A. Individual Fertilizer Calculation (for comparison) ---
    individual_fert_calculation <- reactive({
        # Ensure fert_data and req_nutrients are valid before proceeding
        fert_data <- fertilizers_data()
        req(nrow(fert_data) > 0) # Require at least one fertilizer defined
        
        req_nutrients <- total_nutrient_req()
        req(req_nutrients) # Ensure nutrient requirements are calculated
        
        results <- lapply(1:nrow(fert_data), function(i) {
            fert <- fert_data[i,]
            output_rows <- list()
            
            # Explicitly ensure these values are not NA before using in if conditions
            fert_N_perc_safe <- ifelse(is.na(fert$N_perc), 0, fert$N_perc)
            fert_P_perc_safe <- ifelse(is.na(fert$P_perc), 0, fert$P_perc)
            fert_K_perc_safe <- ifelse(is.na(fert$K_perc), 0, fert$K_perc)
            
            # Helper function to safely get requirement values from total_nutrient_req
            get_req_val <- function(nutrient_name) {
                val_idx <- which(req_nutrients$Nutrient == nutrient_name)
                if(length(val_idx) > 0) {
                    return(ifelse(is.na(req_nutrients$Total_Requirement_kg[val_idx]), 0, req_nutrients$Total_Requirement_kg[val_idx]))
                } else {
                    return(0) # Should not happen if total_nutrient_req is well-formed
                }
            }
            
            req_N_kg_safe <- get_req_val("N")
            req_P_kg_safe <- get_req_val("P2O5")
            req_K_kg_safe <- get_req_val("K2O")
            
            # Calculate for N
            if (fert_N_perc_safe > 0 && req_N_kg_safe > 0) {
                # Check for division by zero before calculation
                divisor <- (fert_N_perc_safe * fert$Bag_Weight_kg)
                if (divisor > 0) {
                    bags_needed <- req_N_kg_safe / divisor
                    cost <- bags_needed * fert$Price_per_bag
                    output_rows[[length(output_rows) + 1]] <- data.frame(
                        Fertilizer = fert$Name,
                        `Nutrient Supplied` = "N",
                        `Bags Needed (50kg)` = bags_needed,
                        `Total Cost (PHP)` = cost,
                        stringsAsFactors = FALSE # Important for consistent column types
                    )
                }
            }
            
            # Calculate for P2O5
            if (fert_P_perc_safe > 0 && req_P_kg_safe > 0) {
                divisor <- (fert_P_perc_safe * fert$Bag_Weight_kg)
                if (divisor > 0) {
                    bags_needed <- req_P_kg_safe / divisor
                    cost <- bags_needed * fert$Price_per_bag
                    output_rows[[length(output_rows) + 1]] <- data.frame(
                        Fertilizer = fert$Name,
                        `Nutrient Supplied` = "P2O5",
                        `Bags Needed (50kg)` = bags_needed,
                        `Total Cost (PHP)` = cost,
                        stringsAsFactors = FALSE
                    )
                }
            }
            
            # Calculate for K2O
            if (fert_K_perc_safe > 0 && req_K_kg_safe > 0) {
                divisor <- (fert_K_perc_safe * fert$Bag_Weight_kg)
                if (divisor > 0) {
                    bags_needed <- req_K_kg_safe / divisor
                    cost <- bags_needed * fert$Price_per_bag
                    output_rows[[length(output_rows) + 1]] <- data.frame(
                        Fertilizer = fert$Name,
                        `Nutrient Supplied` = "K2O",
                        `Bags Needed (50kg)` = bags_needed,
                        `Total Cost (PHP)` = cost,
                        stringsAsFactors = FALSE
                    )
                }
            }
            
            if(length(output_rows) == 0) {
                # Return an empty data frame with expected columns if no calculations were made for this fertilizer
                # Ensure column types match what bind_rows expects
                data.frame(
                    Fertilizer = character(0),
                    `Nutrient Supplied` = character(0),
                    `Bags Needed (50kg)` = numeric(0),
                    `Total Cost (PHP)` = numeric(0),
                    stringsAsFactors = FALSE
                )
            } else {
                bind_rows(output_rows)
            }
        })
        final_df <- bind_rows(results)
        final_df
    })
    
    output$individual_fert_calc_table <- renderTable({
        req(individual_fert_calculation()) # Ensure data is available
        individual_fert_calculation()
    }, striped = TRUE, bordered = TRUE, digits = 2)
    
    output$individual_total_cost <- renderText({
        individual_data <- individual_fert_calculation()
        req(individual_data) # Ensure data is available
        
        if (nrow(individual_data) > 0) {
            # FIX: R/Shiny automatically sanitizes column names with spaces/parentheses to dots.
            # The table shows 'Total.Cost..PHP.', so we access it using that name.
            total_cost <- sum(individual_data$Total.Cost..PHP., na.rm = TRUE)
            
            paste("PHP", format(total_cost, big.mark = ",", scientific = FALSE, digits = 2, nsmall = 2))
        } else {
            "PHP 0.00"
        }
    })
    
    
    # --- B. User-Defined Fertilizer Mix Calculation ---
    mix_calculation <- reactive({
        fert_data <- fertilizers_data()
        req(nrow(fert_data) > 0) # Ensure fertilizers are defined
        
        mix_bags_values <- sapply(1:nrow(fert_data), function(i) {
            input_id <- paste0("mix_bags_", i)
            val <- input[[input_id]]
            as.numeric(ifelse(is.null(val) || is.na(val), 0, val))
        })
        
        if (sum(mix_bags_values, na.rm = TRUE) == 0) {
            return(list(
                summary = data.frame(), # Return empty data frame
                npk_supplied = data.frame(
                    Nutrient = c("N", "P2O5", "K2O"),
                    `Supplied (kg)` = c(0, 0, 0)
                ),
                total_cost = 0,
                total_bags = 0
            ))
        }
        
        total_N_supplied <- 0
        total_P_supplied <- 0
        total_K_supplied <- 0
        total_mix_cost <- 0
        total_bags_used <- 0
        
        mix_summary_list <- list() # Initialize the list here
        
        for (i in 1:nrow(fert_data)) {
            num_bags <- mix_bags_values[i] # Use the pre-validated value
            
            fert <- fert_data[i,]
            
            if (num_bags > 0) { # Only calculate if bags are actually entered
                # Ensure fert percentages and price are not NA before multiplication
                N_from_fert <- num_bags * ifelse(is.na(fert$N_perc), 0, fert$N_perc) * fert$Bag_Weight_kg
                P_from_fert <- num_bags * ifelse(is.na(fert$P_perc), 0, fert$P_perc) * fert$Bag_Weight_kg
                K_from_fert <- num_bags * ifelse(is.na(fert$K_perc), 0, fert$K_perc) * fert$Bag_Weight_kg
                cost_from_fert <- num_bags * ifelse(is.na(fert$Price_per_bag), 0, fert$Price_per_bag)
                
                total_N_supplied <- total_N_supplied + N_from_fert
                total_P_supplied <- total_P_supplied + P_from_fert
                total_K_supplied <- total_K_supplied + K_from_fert
                total_mix_cost <- total_mix_cost + cost_from_fert
                total_bags_used <- total_bags_used + num_bags
                
                mix_summary_list[[length(mix_summary_list) + 1]] <- data.frame(
                    Fertilizer = fert$Name,
                    `Bags (50kg) Used` = num_bags,
                    `Cost from this Fert (PHP)` = cost_from_fert,
                    stringsAsFactors = FALSE # Important for consistent column types
                )
            }
        }
        list(
            summary = bind_rows(mix_summary_list),
            npk_supplied = data.frame(
                Nutrient = c("N", "P2O5", "K2O"),
                `Supplied (kg)` = c(total_N_supplied, total_P_supplied, total_K_supplied)
            ),
            total_cost = total_mix_cost,
            total_bags = total_bags_used
        )
    })
    
    output$mix_summary_table <- renderTable({
        mix_data <- mix_calculation()
        req(mix_data$summary) # Ensure summary data exists from mix_calculation
        if(nrow(mix_data$summary) == 0) return(data.frame(Note = "No fertilizer bags specified in the mix."))
        mix_data$summary
    }, striped = TRUE, bordered = TRUE, digits = 2)
    
    output$mix_npk_supplied_table <- renderTable({
        mix_data <- mix_calculation()
        req(mix_data$npk_supplied) # Ensure npk_supplied data exists from mix_calculation
        if(nrow(mix_data$npk_supplied) == 0 || mix_data$total_bags == 0) return(NULL) # Only show if some bags were specified
        mix_data$npk_supplied
    }, striped = TRUE, bordered = TRUE, digits = 2)
    
    output$mix_total_cost <- renderText({
        mix_data <- mix_calculation()
        req(mix_data$total_cost) # Ensure total_cost exists from mix_calculation
        if(mix_data$total_bags == 0) return("PHP 0.00") # Show 0 if no bags specified
        paste("PHP", format(mix_data$total_cost, big.mark = ",", scientific = FALSE))
    })
    
}

shinyApp(ui = ui, server = server)