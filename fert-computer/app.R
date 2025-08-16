library(shiny)
library(dplyr)
library(ggplot2) # For potential future visualizations, though not strictly needed for current outputs

# Define the UI for the Shiny app
ui <- fluidPage(
    titlePanel("Fertilizer Computation Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Recommended Nutrient Rates (kg/ha)"),
            numericInput("rr_n", "Recommended N (kg/ha):", value = 120, min = 0),
            numericInput("rr_p", "Recommended P (kg/ha):", value = 90, min = 0),
            numericInput("rr_k", "Recommended K (kg/ha):", value = 80, min = 0),
            hr(),
            h3("Problem 2 Inputs"),
            numericInput("area_m2", "Area to be planted (m²):", value = 750, min = 1),
            numericInput("plant_spacing_row_cm", "Plant Spacing (Row to Row, cm):", value = 50, min = 1),
            numericInput("plant_spacing_plant_cm", "Plant Spacing (Plant to Plant, cm):", value = 75, min = 1),
            hr(),
            h3("Customize Fertilizer Data"),
            # Urea
            h4("Urea (46-0-0)"),
            numericInput("urea_n_perc", "N (%):", value = 46, min = 0, max = 100),
            numericInput("urea_p_perc", "P (%):", value = 0, min = 0, max = 100),
            numericInput("urea_k_perc", "K (%):", value = 0, min = 0, max = 100),
            numericInput("urea_price", "Price / 50kg (PHP):", value = 1593.00, min = 0),
            hr(),
            # Solophos
            h4("Solophos (0-20-0)"),
            numericInput("solophos_n_perc", "N (%):", value = 0, min = 0, max = 100),
            numericInput("solophos_p_perc", "P (%):", value = 20, min = 0, max = 100),
            numericInput("solophos_k_perc", "K (%):", value = 0, min = 0, max = 100),
            numericInput("solophos_price", "Price / 50kg (PHP):", value = 1300.00, min = 0),
            hr(),
            # Muriate of Potash (MOP)
            h4("MOP (0-0-60)"),
            numericInput("mop_n_perc", "N (%):", value = 0, min = 0, max = 100),
            numericInput("mop_p_perc", "P (%):", value = 0, min = 0, max = 100),
            numericInput("mop_k_perc", "K (%):", value = 60, min = 0, max = 100),
            numericInput("mop_price", "Price / 50kg (PHP):", value = 2102.55, min = 0),
            hr(),
            # Complete
            h4("Complete (14-14-14)"),
            numericInput("complete_n_perc", "N (%):", value = 14, min = 0, max = 100),
            numericInput("complete_p_perc", "P (%):", value = 14, min = 0, max = 100),
            numericInput("complete_k_perc", "K (%):", value = 14, min = 0, max = 100),
            numericInput("complete_price", "Price / 50kg (PHP):", value = 1582.27, min = 0),
            hr(),
            # Ammophos
            h4("Ammophos (16-20-0)"),
            numericInput("ammophos_n_perc", "N (%):", value = 16, min = 0, max = 100),
            numericInput("ammophos_p_perc", "P (%):", value = 20, min = 0, max = 100),
            numericInput("ammophos_k_perc", "K (%):", value = 0, min = 0, max = 100),
            numericInput("ammophos_price", "Price / 50kg (PHP):", value = 1503.50, min = 0),
            hr(),
            h3("Current Fertilizer Information"),
            tableOutput("fertilizer_info_table")
        ),
        
        mainPanel(
            tabsetPanel(
                id = "dashboard_tabs",
                tabPanel("Problem 1: Fertilizer Needed",
                         h3("Calculate Fertilizer Needed"),
                         p("This section calculates the amount of Urea, Solophos, and Muriate of Potash needed to meet the recommended N, P, K rates."),
                         tableOutput("prob1_output")
                ),
                tabPanel("Problem 2: Application Rate per Plant",
                         h3("Application Rate per Plant"),
                         p("This section calculates the application rate of each fertilizer per plant based on the amounts from Problem 1, planting area, and spacing."),
                         tableOutput("prob2_output")
                ),
                tabPanel("Problem 3: Fertilizer Combination Savings",
                         h3("Fertilizer Combination Savings"),
                         p("This section compares the cost of three different fertilizer combinations to meet the recommended N, P, K rates and identifies the most cost-effective option."),
                         tableOutput("prob3_output"),
                         h4("Most Economical Combination:"),
                         textOutput("prob3_cheapest_combo")
                ),
                tabPanel("Problem 4: Nutrient Applied Comparison",
                         h3("Nutrient Applied Comparison"),
                         p("This section compares the total amount of N, P, and K applied by each combination from Problem 3, highlighting the highest and lowest nutrient inputs."),
                         tableOutput("prob4_output"),
                         h4("Combination with Highest Total Nutrients:"),
                         textOutput("prob4_highest_nutrient"),
                         h4("Combination with Lowest Total Nutrients:"),
                         textOutput("prob4_lowest_nutrient")
                )
            )
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    
    # Reactive expression to create fertilizers_data based on user inputs
    custom_fertilizers_data <- reactive({
        tribble(
            ~Fertilizer, ~N_perc, ~P_perc, ~K_perc, ~Price_per_50kg_bag,
            "Urea",      input$urea_n_perc / 100,    input$urea_p_perc / 100,    input$urea_k_perc / 100,    input$urea_price,
            "Solophos",  input$solophos_n_perc / 100,    input$solophos_p_perc / 100,    input$solophos_k_perc / 100,    input$solophos_price,
            "MOP",       input$mop_n_perc / 100,    input$mop_p_perc / 100,    input$mop_k_perc / 100,    input$mop_price,
            "Complete",  input$complete_n_perc / 100,    input$complete_p_perc / 100,    input$complete_k_perc / 100,    input$complete_price,
            "Ammophos",  input$ammophos_n_perc / 100,    input$ammophos_p_perc / 100,    input$ammophos_k_perc / 100,    input$ammophos_price
        ) %>%
            mutate(Price_per_kg = Price_per_50kg_bag / 50)
    })
    
    # Display fertilizer info table
    output$fertilizer_info_table <- renderTable({
        # Use the reactive custom_fertilizers_data
        custom_fertilizers_data() %>%
            select(Fertilizer, N_perc, P_perc, K_perc, Price_per_50kg_bag) %>%
            mutate(N_perc = paste0(N_perc * 100, "%"),
                   P_perc = paste0(P_perc * 100, "%"),
                   K_perc = paste0(K_perc * 100, "%")) %>%
            rename("N (%)" = N_perc, "P (%)" = P_perc, "K (%)" = K_perc, "Price/50kg Bag (PHP)" = Price_per_50kg_bag)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    
    # Reactive values for recommended rates
    rr_values <- reactive({
        list(
            N = input$rr_n,
            P = input$rr_p,
            K = input$rr_k
        )
    })
    
    # Problem 1: Calculate Fertilizer Needed
    prob1_results <- reactive({
        req(rr_values()$N, rr_values()$P, rr_values()$K)
        # Use the reactive custom_fertilizers_data
        fert_data <- custom_fertilizers_data()
        
        urea_data <- fert_data %>% filter(Fertilizer == "Urea")
        solophos_data <- fert_data %>% filter(Fertilizer == "Solophos")
        mop_data <- fert_data %>% filter(Fertilizer == "MOP")
        
        # Validate that fertilizers can supply the required nutrients
        validate(
            # If N is required but Urea has 0% N
            need(!(rr_values()$N > 0 && urea_data$N_perc == 0),
                 "Urea has 0% N, but Nitrogen is required. Please adjust Urea N% or N requirement."),
            # If P is required but Solophos has 0% P
            need(!(rr_values()$P > 0 && solophos_data$P_perc == 0),
                 "Solophos has 0% P, but Phosphorus is required. Please adjust Solophos P% or P requirement."),
            # If K is required but MOP has 0% K
            need(!(rr_values()$K > 0 && mop_data$K_perc == 0),
                 "MOP has 0% K, but Potassium is required. Please adjust MOP K% or K requirement.")
        )
        
        # Calculate fertilizer amounts (now safe from division by zero for required nutrients)
        mop_needed_kg <- ifelse(mop_data$K_perc > 0, rr_values()$K / mop_data$K_perc, 0)
        solophos_needed_kg <- ifelse(solophos_data$P_perc > 0, rr_values()$P / solophos_data$P_perc, 0)
        urea_needed_kg <- ifelse(urea_data$N_perc > 0, rr_values()$N / urea_data$N_perc, 0)
        
        # Create data frame with a syntactic name first, then mutate and rename
        df_temp <- data.frame(
            Fertilizer = c("Urea", "Solophos", "MOP"),
            AmountNeeded_kg = c(urea_needed_kg, solophos_needed_kg, mop_needed_kg)
        ) %>%
            mutate(AmountNeeded_kg = round(AmountNeeded_kg, 2))
        
        # Rename the column for display
        df_temp %>%
            rename(`Amount Needed (kg)` = AmountNeeded_kg)
    })
    
    output$prob1_output <- renderTable({
        prob1_results()
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # Problem 2: Application Rate per Plant
    prob2_results <- reactive({
        req(input$area_m2, input$plant_spacing_row_cm, input$plant_spacing_plant_cm)
        req(prob1_results()) # Ensure Problem 1 results are available
        
        area_m2 <- input$area_m2
        plant_spacing_row_m <- input$plant_spacing_row_cm / 100
        plant_spacing_plant_m <- input$plant_spacing_plant_cm / 100
        
        # Calculate number of plants
        plants_per_m2 <- 1 / (plant_spacing_row_m * plant_spacing_plant_m)
        total_plants <- plants_per_m2 * area_m2
        
        # Get fertilizer amounts from Problem 1
        # Note: prob1_results() now returns a data frame with `Amount Needed (kg)`
        fert_amounts_kg <- prob1_results() %>%
            rename(Amount_kg = `Amount Needed (kg)`)
        
        # Calculate application rate per plant in grams
        fert_amounts_g_per_plant <- fert_amounts_kg %>%
            mutate(Amount_g_per_plant = (Amount_kg * 1000) / total_plants) %>%
            select(Fertilizer, Amount_g_per_plant) %>%
            mutate(Amount_g_per_plant = round(Amount_g_per_plant, 3)) %>%
            rename(`Amount per Plant (g)` = Amount_g_per_plant)
        
        fert_amounts_g_per_plant
    })
    
    output$prob2_output <- renderTable({
        prob2_results()
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # Problem 3: Fertilizer Combination Savings
    prob3_results <- reactive({
        req(rr_values()$N, rr_values()$P, rr_values()$K)
        
        N_req <- rr_values()$N
        P_req <- rr_values()$P
        K_req <- rr_values()$K
        b <- c(N_req, P_req, K_req) # Required N, P, K vector
        
        # Use the reactive custom_fertilizers_data
        fert_data_all <- custom_fertilizers_data()
        
        results <- list()
        
        # --- Combination 1: Urea, Solophos, MOP ---
        fert_names_1 <- c("Urea", "Solophos", "MOP")
        fert_data_1 <- fert_data_all %>% filter(Fertilizer %in% fert_names_1) %>%
            arrange(match(Fertilizer, fert_names_1)) # Ensure order
        
        A1 <- matrix(c(
            fert_data_1$N_perc[1], fert_data_1$N_perc[2], fert_data_1$N_perc[3],
            fert_data_1$P_perc[1], fert_data_1$P_perc[2], fert_data_1$P_perc[3],
            fert_data_1$K_perc[1], fert_data_1$K_perc[2], fert_data_1$K_perc[3]
        ), nrow = 3, byrow = TRUE)
        
        costs_1 <- fert_data_1$Price_per_kg
        
        combo1_calc <- tryCatch({
            amounts <- solve(A1, b)
            if (any(amounts < -1e-9)) { # Check for negative amounts
                list(amounts = setNames(rep(NA, 3), fert_names_1), total_cost = NA, status = "Infeasible (Negative amounts)")
            } else {
                list(amounts = setNames(amounts, fert_names_1), total_cost = sum(amounts * costs_1), status = "Success")
            }
        }, error = function(e) {
            list(amounts = setNames(rep(NA, 3), fert_names_1), total_cost = NA, status = "Infeasible (No unique solution)")
        })
        results[["Combination 1"]] <- combo1_calc
        
        # --- Combination 2: Complete, Ammophos, MOP ---
        fert_names_2 <- c("Complete", "Ammophos", "MOP")
        fert_data_2 <- fert_data_all %>% filter(Fertilizer %in% fert_names_2) %>%
            arrange(match(Fertilizer, fert_names_2))
        
        A2 <- matrix(c(
            fert_data_2$N_perc[1], fert_data_2$N_perc[2], fert_data_2$N_perc[3],
            fert_data_2$P_perc[1], fert_data_2$P_perc[2], fert_data_2$P_perc[3],
            fert_data_2$K_perc[1], fert_data_2$K_perc[2], fert_data_2$K_perc[3]
        ), nrow = 3, byrow = TRUE)
        
        costs_2 <- fert_data_2$Price_per_kg
        
        combo2_calc <- tryCatch({
            amounts <- solve(A2, b)
            if (any(amounts < -1e-9)) {
                list(amounts = setNames(rep(NA, 3), fert_names_2), total_cost = NA, status = "Infeasible (Negative amounts)")
            } else {
                list(amounts = setNames(amounts, fert_names_2), total_cost = sum(amounts * costs_2), status = "Success")
            }
        }, error = function(e) {
            list(amounts = setNames(rep(NA, 3), fert_names_2), total_cost = NA, status = "Infeasible (No unique solution)")
        })
        results[["Combination 2"]] <- combo2_calc
        
        # --- Combination 3: Complete, Ammophos, Solophos ---
        # NOTE: This combination might be infeasible for K if Complete alone cannot supply it.
        fert_names_3 <- c("Complete", "Ammophos", "Solophos")
        fert_data_3 <- fert_data_all %>% filter(Fertilizer %in% fert_names_3) %>%
            arrange(match(Fertilizer, fert_names_3))
        
        A3 <- matrix(c(
            fert_data_3$N_perc[1], fert_data_3$N_perc[2], fert_data_3$N_perc[3],
            fert_data_3$P_perc[1], fert_data_3$P_perc[2], fert_data_3$P_perc[3],
            fert_data_3$K_perc[1], fert_data_3$K_perc[2], fert_data_3$K_perc[3]
        ), nrow = 3, byrow = TRUE)
        
        costs_3 <- fert_data_3$Price_per_kg
        
        combo3_calc <- tryCatch({
            amounts <- solve(A3, b)
            if (any(amounts < -1e-9)) {
                list(amounts = setNames(rep(NA, 3), fert_names_3), total_cost = NA, status = "Infeasible (Negative amounts)")
            } else {
                list(amounts = setNames(amounts, fert_names_3), total_cost = sum(amounts * costs_3), status = "Success")
            }
        }, error = function(e) {
            list(amounts = setNames(rep(NA, 3), fert_names_3), total_cost = NA, status = "Infeasible (No unique solution)")
        })
        results[["Combination 3"]] <- combo3_calc
        
        # Format results for display
        display_data <- data.frame(
            Combination = character(),
            Fertilizer1 = character(),
            Amount1_kg = numeric(),
            Fertilizer2 = character(),
            Amount2_kg = numeric(),
            Fertilizer3 = character(),
            Amount3_kg = numeric(),
            Total_Cost_PHP = numeric(),
            Status = character(),
            stringsAsFactors = FALSE
        )
        
        for (i in seq_along(results)) {
            combo_name <- names(results)[i]
            res <- results[[i]]
            amounts_vec <- res$amounts
            names(amounts_vec) <- c(paste0(names(amounts_vec)[1]), paste0(names(amounts_vec)[2]), paste0(names(amounts_vec)[3]))
            
            display_data <- rbind(display_data, data.frame(
                Combination = combo_name,
                Fertilizer1 = names(amounts_vec)[1],
                Amount1_kg = round(amounts_vec[1], 2),
                Fertilizer2 = names(amounts_vec)[2],
                Amount2_kg = round(amounts_vec[2], 2),
                Fertilizer3 = names(amounts_vec)[3],
                Amount3_kg = round(amounts_vec[3], 2),
                Total_Cost_PHP = round(res$total_cost, 2),
                Status = res$status,
                stringsAsFactors = FALSE
            ))
        }
        display_data
    })
    
    output$prob3_output <- renderTable({
        prob3_results()
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$prob3_cheapest_combo <- renderText({
        results_df <- prob3_results() %>% filter(Status == "Success")
        if (nrow(results_df) > 0) {
            cheapest_combo <- results_df %>%
                filter(Total_Cost_PHP == min(Total_Cost_PHP, na.rm = TRUE)) %>%
                pull(Combination) %>%
                unique()
            paste("The most economical combination is:", paste(cheapest_combo, collapse = ", "), "with a total cost of PHP", min(results_df$Total_Cost_PHP, na.rm = TRUE))
        } else {
            "No feasible combinations found."
        }
    })
    
    # Problem 4: Nutrient Applied Comparison
    prob4_results <- reactive({
        req(prob3_results())
        
        prob3_df <- prob3_results() %>% filter(Status == "Success")
        if (nrow(prob3_df) == 0) {
            return(NULL) # No successful combinations to compare
        }
        
        # Use the reactive custom_fertilizers_data
        fert_data_all <- custom_fertilizers_data()
        
        nutrient_comparison <- data.frame(
            Combination = character(),
            N_applied_kg = numeric(),
            P_applied_kg = numeric(),
            K_applied_kg = numeric(),
            Total_Nutrients_kg = numeric(),
            stringsAsFactors = FALSE
        )
        
        for (i in 1:nrow(prob3_df)) {
            combo_row <- prob3_df[i, ]
            combo_name <- combo_row$Combination
            
            # Extract amounts for the current combination
            fert1_name <- combo_row$Fertilizer1
            fert1_amount <- combo_row$Amount1_kg
            fert2_name <- combo_row$Fertilizer2
            fert2_amount <- combo_row$Amount2_kg
            fert3_name <- combo_row$Fertilizer3
            fert3_amount <- combo_row$Amount3_kg
            
            # Get NPK percentages for these fertilizers from the reactive data
            fert1_data <- fert_data_all %>% filter(Fertilizer == fert1_name)
            fert2_data <- fert_data_all %>% filter(Fertilizer == fert2_name)
            fert3_data <- fert_data_all %>% filter(Fertilizer == fert3_name)
            
            # Calculate total N, P, K applied
            n_applied <- (fert1_amount * fert1_data$N_perc) +
                (fert2_amount * fert2_data$N_perc) +
                (fert3_amount * fert3_data$N_perc)
            
            p_applied <- (fert1_amount * fert1_data$P_perc) +
                (fert2_amount * fert2_data$P_perc) +
                (fert3_amount * fert3_data$P_perc)
            
            k_applied <- (fert1_amount * fert1_data$K_perc) +
                (fert2_amount * fert2_data$K_perc) +
                (fert3_amount * fert3_data$K_perc)
            
            total_nutrients <- n_applied + p_applied + k_applied
            
            nutrient_comparison <- rbind(nutrient_comparison, data.frame(
                Combination = combo_name,
                N_applied_kg = round(n_applied, 2),
                P_applied_kg = round(p_applied, 2),
                K_applied_kg = round(k_applied, 2),
                Total_Nutrients_kg = round(total_nutrients, 2),
                stringsAsFactors = FALSE
            ))
        }
        nutrient_comparison
    })
    
    output$prob4_output <- renderTable({
        prob4_results()
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$prob4_highest_nutrient <- renderText({
        results_df <- prob4_results()
        if (!is.null(results_df) && nrow(results_df) > 0) {
            highest_combo <- results_df %>%
                filter(Total_Nutrients_kg == max(Total_Nutrients_kg, na.rm = TRUE)) %>%
                pull(Combination) %>%
                unique()
            paste("The combination with the highest total nutrients applied is:", paste(highest_combo, collapse = ", "), "with", max(results_df$Total_Nutrients_kg, na.rm = TRUE), "kg.")
        } else {
            "No feasible combinations for comparison."
        }
    })
    
    output$prob4_lowest_nutrient <- renderText({
        results_df <- prob4_results()
        if (!is.null(results_df) && nrow(results_df) > 0) {
            lowest_combo <- results_df %>%
                filter(Total_Nutrients_kg == min(Total_Nutrients_kg, na.rm = TRUE)) %>%
                pull(Combination) %>%
                unique()
            paste("The combination with the lowest total nutrients applied is:", paste(lowest_combo, collapse = ", "), "with", min(results_df$Total_Nutrients_kg, na.rm = TRUE), "kg.")
        } else {
            "No feasible combinations for comparison."
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)