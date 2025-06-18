# app.R

# Load necessary packages
library(shiny)
library(agricolae)         # For experimental designs
library(agricolaeplotr)    # For visualization of designs
library(dplyr)             # For data manipulation
library(ggplot2)           # For further customization
library(stringr)           # For string manipulation

# Define the User Interface (UI)
ui <- fluidPage(
    titlePanel("Field Layout Generator (Experimental Designs)"),
    
    sidebarLayout(
        sidebarPanel(
            h3("1. Generate Field Layout"),
            selectInput("designType", "Select Design Type:",
                        choices = c("Completely Randomized Design",
                                    "Randomized Complete Block Design",
                                    "Split-Plot Design")),
            
            # Inputs for CRD and RCBD (Treatments and Replications)
            conditionalPanel(
                condition = "input.designType == 'Completely Randomized Design' || input.designType == 'Randomized Complete Block Design'",
                numericInput("numTreatments", "Number of Treatments (Genotypes):", value = 10, min = 2),
                textAreaInput("treatmentLabels", "Enter Treatment Labels (comma-separated, e.g., T1, T2, Variety A):", rows = 3,
                              placeholder = "e.g., T1, T2, T3, Control, Treatment A"),
                numericInput("numReplications", "Number of Replications:", value = 3, min = 1)
            ),
            
            # Add input for plot arrangement for CRD (ncols and nrows) - specific to CRD
            conditionalPanel(
                condition = "input.designType == 'Completely Randomized Design'",
                numericInput("crd_ncols", "Columns for CRD Plot:", value = 5, min = 1),
                numericInput("crd_nrows", "Rows for CRD Plot:", value = 0, min = 0) # 0 means auto-calculate
            ),
            
            # Specific inputs for Split-Plot - specific to Split-Plot
            conditionalPanel(
                condition = "input.designType == 'Split-Plot Design'",
                numericInput("wpLevels", "Number of Whole Plot Levels (A):", value = 3, min = 2),
                textAreaInput("wpLabels", "Enter Whole Plot Labels (comma-separated, e.g., Water Level 1, Water Level 2):", rows = 2,
                              placeholder = "e.g., A1, A2, A3"),
                numericInput("spLevels", "Number of Subplot Levels (B):", value = 4, min = 2),
                textAreaInput("spLabels", "Enter Subplot Labels (comma-separated, e.g., Fertilizer 1, Fertilizer 2):", rows = 2,
                              placeholder = "e.g., B1, B2, B3, B4")
            ),
            
            hr(), # Separator
            
            # Optional Seed Input (applies to all designs)
            numericInput("randomSeed", "Optional: Set Random Seed (for reproducibility):", value = NULL, min = 1),
            helpText("Leave blank for a truly random layout each time."),
            
            actionButton("generateLayoutBtn", "Generate Layout"),
            hr(),
            
            h3("2. Download Logbook"),
            downloadButton("downloadLogbook", "Download Logbook (CSV)")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Field Layout",
                         h4("Generated Field Layout (Logbook):"),
                         tableOutput("layoutTable"),
                         h4("Graphical Layout:"),
                         plotOutput("generatedLayoutPlot", height = "600px")),
                tabPanel("Dummy ANOVA Table",
                         h4("Expected Degrees of Freedom:"),
                         tableOutput("anovaTable"))
            )
        )
    )
)

# Define the Server Logic
server <- function(input, output, session) {
    
    # Reactive value to store the generated design output (including $book)
    rv <- reactiveValues(design_output = NULL, logbook = NULL, anova_df = NULL, plot_coords = NULL)
    
    # Helper function to find a column by its unique values (for treatments/factors)
    # This is more robust as it doesn't rely on column names being 'trt', 'trt1', 'trt2'
    find_factor_column <- function(df, expected_unique_values) {
        # Ensure expected_unique_values are sorted for consistent comparison
        expected_unique_values_sorted <- sort(unique(expected_unique_values))
        
        for (col_name in colnames(df)) {
            current_col_values_sorted <- sort(unique(as.character(df[[col_name]])))
            if (length(current_col_values_sorted) == length(expected_unique_values_sorted) &&
                all(current_col_values_sorted == expected_unique_values_sorted)) {
                return(col_name)
            }
        }
        return(NULL) # Column not found
    }
    
    
    # 1. Generate Field Layout
    observeEvent(input$generateLayoutBtn, {
        req(input$designType, input$numReplications)
        
        design_output_agricolae <- NULL
        logbook_df_raw <- NULL
        design_type_local <- input$designType
        
        # --- Handle Random Seed ---
        if (!is.null(input$randomSeed) && !is.na(input$randomSeed)) {
            set.seed(input$randomSeed)
            showNotification(paste("Using random seed:", input$randomSeed), type = "message", duration = 3)
        } else {
            # If no seed is provided, ensure a truly random generation
            rm(.Random.seed, envir = globalenv()) # Removes the current seed state
            showNotification("Generating truly random layout.", type = "message", duration = 3)
        }
        # --- End Handle Random Seed ---
        
        tryCatch({
            if (design_type_local == "Completely Randomized Design") {
                req(input$numTreatments, input$treatmentLabels)
                user_trt_labels <- unlist(str_split(input$treatmentLabels, ",")) %>% str_trim()
                user_trt_labels <- user_trt_labels[user_trt_labels != ""] # Remove empty strings
                
                if (length(user_trt_labels) == 0) {
                    stop("Please enter valid treatment labels.")
                }
                if (length(user_trt_labels) != input$numTreatments) {
                    stop(paste0("Number of treatment labels (", length(user_trt_labels), ") does not match 'Number of Treatments' (", input$numTreatments, ")."))
                }
                
                # CRITICAL FIX: Pass the vector 'user_trt_labels' directly
                design_output_agricolae <- agricolae::design.crd(
                    trt = user_trt_labels,
                    r = input$numReplications
                )
                logbook_df_raw <- design_output_agricolae$book
                
                # FIND THE ACTUAL TREATMENT COLUMN NAME IN THE GENERATED BOOK
                actual_trt_col_name <- find_factor_column(logbook_df_raw, user_trt_labels)
                
                if (is.null(actual_trt_col_name)) {
                    stop("Could not find the treatment column in CRD design output. Columns found: ",
                         paste(colnames(logbook_df_raw), collapse = ", "), ". Expected labels: ",
                         paste(user_trt_labels, collapse = ", "))
                }
                
                # Rename the identified treatment column to 'trt' for consistency AFTER generation
                if (actual_trt_col_name != "trt") {
                    logbook_df_raw <- logbook_df_raw %>%
                        rename(trt = !!sym(actual_trt_col_name)) # Use !!sym() for dynamic renaming
                }
                
                logbook_df_raw <- logbook_df_raw %>%
                    mutate(Plot_Label = paste0(plots, "\n", trt))
                
                design_output_agricolae$book <- logbook_df_raw
                design_output_agricolae$parameters$trt <- "trt" # Confirming the parameter name
                
                # Calculate DF for CRD
                t <- input$numTreatments
                r <- input$numReplications
                total_plots <- t * r
                
                df_trt <- t - 1
                df_error <- total_plots - t
                df_total <- total_plots - 1
                
                rv$anova_df <- tibble(
                    `Source of Variation` = c("Treatment", "Error", "Total"),
                    `Degrees of Freedom (df)` = c(df_trt, df_error, df_total)
                )
                
                # --- Calculate Row and Column Coordinates for CRD ---
                plot_ncols <- input$crd_ncols
                plot_nrows <- input$crd_nrows
                
                if (plot_nrows == 0) {
                    plot_nrows <- ceiling(total_plots / plot_ncols)
                }
                
                col_seq <- rep(1:plot_ncols, length.out = total_plots)
                row_seq <- ceiling((1:total_plots) / plot_ncols)
                
                crd_coords <- tibble(
                    plots = 1:total_plots,
                    row_coord = row_seq,
                    col_coord = col_seq
                )
                rv$plot_coords <- crd_coords
                # --- End CRD Coordinates ---
                
            } else if (design_type_local == "Randomized Complete Block Design") {
                req(input$numTreatments, input$treatmentLabels)
                user_trt_labels <- unlist(str_split(input$treatmentLabels, ",")) %>% str_trim()
                user_trt_labels <- user_trt_labels[user_trt_labels != ""]
                
                if (length(user_trt_labels) == 0) {
                    stop("Please enter valid treatment labels.")
                }
                if (length(user_trt_labels) != input$numTreatments) {
                    stop(paste0("Number of treatment labels (", length(user_trt_labels), ") does not match 'Number of Treatments' (", input$numTreatments, ")."))
                }
                
                # CRITICAL FIX: Pass the vector 'user_trt_labels' directly
                design_output_agricolae <- agricolae::design.rcbd(
                    trt = user_trt_labels,
                    r = input$numReplications
                )
                logbook_df_raw <- design_output_agricolae$book
                
                # FIND THE ACTUAL TREATMENT COLUMN NAME IN THE GENERATED BOOK
                actual_trt_col_name <- find_factor_column(logbook_df_raw, user_trt_labels)
                
                if (is.null(actual_trt_col_name)) {
                    stop("Could not find the treatment column in RCBD design output. Columns found: ",
                         paste(colnames(logbook_df_raw), collapse = ", "), ". Expected labels: ",
                         paste(user_trt_labels, collapse = ", "))
                }
                
                # Rename the identified treatment column to 'trt' for consistency AFTER generation
                if (actual_trt_col_name != "trt") {
                    logbook_df_raw <- logbook_df_raw %>%
                        rename(trt = !!sym(actual_trt_col_name))
                }
                
                logbook_df_raw <- logbook_df_raw %>%
                    mutate(Plot_Label = paste0(plots, "\n", trt)) %>%
                    group_by(block) %>%
                    mutate(Plot_Position_In_Block = row_number()) %>%
                    ungroup()
                
                design_output_agricolae$book <- logbook_df_raw
                design_output_agricolae$parameters$trt <- "trt"
                design_output_agricolae$parameters$block <- "block"
                
                # Calculate DF for RCBD
                t <- input$numTreatments
                b <- input$numReplications # In RCBD, replications are blocks
                total_plots <- t * b
                
                df_block <- b - 1
                df_trt <- t - 1
                df_error <- (b - 1) * (t - 1)
                df_total <- total_plots - 1
                
                rv$anova_df <- tibble(
                    `Source of Variation` = c("Block", "Treatment", "Error", "Total"),
                    `Degrees of Freedom (df)` = c(df_block, df_trt, df_error, df_total)
                )
                
                # --- Calculate Row and Column Coordinates for RCBD ---
                rcbd_coords <- logbook_df_raw %>%
                    arrange(block, plots) %>% # Ensure consistent ordering
                    group_by(block) %>%
                    mutate(col_coord = row_number()) %>%
                    ungroup() %>%
                    mutate(row_coord = as.integer(as.factor(block))) %>% # Each block is a row
                    select(plots, row_coord, col_coord)
                
                rv$plot_coords <- rcbd_coords
                # --- End RCBD Coordinates ---
                
            } else if (design_type_local == "Split-Plot Design") {
                req(input$wpLevels, input$spLevels, input$wpLabels, input$spLabels)
                
                user_wp_labels <- unlist(str_split(input$wpLabels, ",")) %>% str_trim()
                user_wp_labels <- user_wp_labels[user_wp_labels != ""]
                user_sp_labels <- unlist(str_split(input$spLabels, ",")) %>% str_trim()
                user_sp_labels <- user_sp_labels[user_sp_labels != ""]
                
                if (length(user_wp_labels) == 0) {
                    stop("Please enter valid Whole Plot labels.")
                }
                if (length(user_sp_labels) == 0) {
                    stop("Please enter valid Subplot labels.")
                }
                if (length(user_wp_labels) != input$wpLevels) {
                    stop(paste0("Number of Whole Plot labels (", length(user_wp_labels), ") does not match 'Number of Whole Plot Levels' (", input$wpLevels, ")."))
                }
                if (length(user_sp_labels) != input$spLevels) {
                    stop(paste0("Number of Subplot labels (", length(user_sp_labels), ") does not match 'Number of Subplot Levels' (", input$spLevels, ")."))
                }
                
                # CRITICAL FIX: Pass the vectors 'user_wp_labels' and 'user_sp_labels' directly
                design_output_agricolae <- agricolae::design.split(
                    trt1 = user_wp_labels,
                    trt2 = user_sp_labels,
                    r = input$numReplications,
                    design = "rcbd"
                )
                logbook_df_raw <- design_output_agricolae$book
                
                # --- FIND THE ACTUAL WHOLE PLOT AND SUBPLOT FACTOR COLUMN NAMES IN THE GENERATED BOOK ---
                actual_trt1_col_name <- find_factor_column(logbook_df_raw, user_wp_labels)
                actual_trt2_col_name <- find_factor_column(logbook_df_raw, user_sp_labels)
                
                # FIX: Corrected "is.is.null" to "is.null"
                if (is.null(actual_trt1_col_name) || is.null(actual_trt2_col_name)) {
                    stop("Could not find Whole Plot (trt1) or Subplot (trt2) columns in Split-Plot design output. ",
                         "Columns found: ", paste(colnames(logbook_df_raw), collapse = ", "),
                         ". Expected Whole Plot labels: ", paste(user_wp_labels, collapse = ", "),
                         ". Expected Subplot labels: ", paste(user_sp_labels, collapse = ", "))
                }
                
                # Rename to 'trt1' and 'trt2' for consistency if they are named differently AFTER generation
                if (actual_trt1_col_name != "trt1") {
                    logbook_df_raw <- logbook_df_raw %>%
                        rename(trt1 = !!sym(actual_trt1_col_name))
                }
                if (actual_trt2_col_name != "trt2") {
                    logbook_df_raw <- logbook_df_raw %>%
                        rename(trt2 = !!sym(actual_trt2_col_name))
                }
                
                logbook_df_raw <- logbook_df_raw %>%
                    dplyr::mutate(TRT_COMB = paste0(trt1, "-", trt2))
                
                logbook_df_raw <- logbook_df_raw %>%
                    mutate(Plot_Label = paste0(plots, "-", splots, "\n", TRT_COMB))
                
                design_output_agricolae$parameters$trt1 <- "trt1"
                design_output_agricolae$parameters$trt2 <- "trt2"
                design_output_agricolae$parameters$block <- "block"
                design_output_agricolae$book <- logbook_df_raw
                
                # Calculate DF for Split-Plot (RCBD for whole plots)
                r <- input$numReplications
                A <- input$wpLevels
                B_sp <- input$spLevels
                total_plots <- r * A * B_sp
                
                df_block <- r - 1
                df_A <- A - 1
                df_error_a <- (r - 1) * (A - 1)
                df_B <- B_sp - 1
                df_AB <- (A - 1) * (B_sp - 1)
                df_error_b <- A * (r - 1) * (B_sp - 1)
                df_total <- total_plots - 1
                
                rv$anova_df <- tibble(
                    `Source of Variation` = c("Block", "Whole Plot Factor (A)", "Error(a)",
                                              "Subplot Factor (B)", "A x B Interaction", "Error(b)", "Total"),
                    `Degrees of Freedom (df)` = c(df_block, df_A, df_error_a,
                                                  df_B, df_AB, df_error_b, df_total)
                )
                
                # --- Calculate Row and Column Coordinates for Split-Plot ---
                split_plot_coords <- logbook_df_raw %>%
                    mutate(
                        block_int = as.integer(as.character(block)),
                        plots_int = as.integer(plots),
                        splots_int = as.integer(splots)
                    ) %>%
                    arrange(block_int, plots_int, splots_int) %>%
                    group_by(block_int) %>%
                    mutate(
                        row_coord = block_int,
                        col_coord = (plots_int - 1) * B_sp + splots_int
                    ) %>%
                    ungroup() %>%
                    select(plots = plots_int, splots = splots_int, block = block_int, row_coord, col_coord)
                
                rv$plot_coords <- split_plot_coords
                # --- End Split-Plot Coordinates ---
            }
            
            req(logbook_df_raw)
            
            # --- Identify and prepare columns for the standardized logbook table ---
            standard_plot <- logbook_df_raw$plots
            standard_block <- NA_character_
            standard_treatment <- NA_character_
            standard_wp_factor <- NA_character_
            standard_sp_factor <- NA_character_
            
            if ("block" %in% colnames(logbook_df_raw)) {
                standard_block <- as.character(logbook_df_raw$block)
            }
            
            if (design_type_local == "Completely Randomized Design" || design_type_local == "Randomized Complete Block Design") {
                if ("trt" %in% colnames(logbook_df_raw)) { # 'trt' should be consistent due to renaming
                    standard_treatment <- as.character(logbook_df_raw$trt)
                }
            } else if (design_type_local == "Split-Plot Design") {
                if ("TRT_COMB" %in% colnames(logbook_df_raw)) {
                    standard_treatment <- as.character(logbook_df_raw$TRT_COMB)
                }
                if ("trt1" %in% colnames(logbook_df_raw)) { # 'trt1' should be consistent
                    standard_wp_factor <- as.character(logbook_df_raw$trt1)
                }
                if ("trt2" %in% colnames(logbook_df_raw)) { # 'trt2' should be consistent
                    standard_sp_factor <- as.character(logbook_df_raw$trt2)
                }
            }
            
            # Create the standardized dataframe for table and download
            standardized_df <- NULL
            if (design_type_local == "Split-Plot Design") {
                standardized_df <- tibble(
                    Plot = as.integer(logbook_df_raw$plots),
                    Subplot = as.integer(logbook_df_raw$splots),
                    Block = as.integer(as.character(logbook_df_raw$block)),
                    Treatment = as.factor(standard_treatment),
                    WholePlotFactor = as.factor(standard_wp_factor),
                    SubplotFactor = as.factor(standard_sp_factor)
                ) %>%
                    select_if(~!all(is.na(.))) %>%
                    left_join(rv$plot_coords, by = c("Plot" = "plots", "Subplot" = "splots", "Block" = "block"))
            } else {
                standardized_df <- tibble(
                    Plot = as.integer(standard_plot),
                    Block = as.factor(standard_block),
                    Treatment = as.factor(standard_treatment),
                    WholePlotFactor = as.factor(standard_wp_factor),
                    SubplotFactor = as.factor(standard_sp_factor)
                ) %>%
                    select_if(~!all(is.na(.))) %>%
                    left_join(rv$plot_coords, by = c("Plot" = "plots"))
            }
            
            rv$logbook <- standardized_df
            rv$design_output <- design_output_agricolae
            showNotification(paste(design_type_local, "Layout generated successfully!"), type = "message", duration = 3)
            
        }, error = function(e) {
            message("Design generation failed for ", design_type_local, ": ", e$message)
            showNotification(paste("Error generating layout for", design_type_local, ": ", e$message,
                                   "Please ensure valid combinations for treatments/factors and replications, and correct number of labels."),
                             type = "error", duration = 8)
            rv$logbook <- NULL
            rv$design_output <- NULL
            rv$anova_df <- NULL
            rv$plot_coords <- NULL
            output$layoutTable <- renderTable(NULL)
            output$generatedLayoutPlot <- renderPlot(NULL)
            output$anovaTable <- renderTable(NULL)
        })
    })
    
    # Display the generated layout table
    output$layoutTable <- renderTable({
        req(rv$logbook)
        rv$logbook
    })
    
    # Display the dummy ANOVA table
    output$anovaTable <- renderTable({
        req(rv$anova_df)
        rv$anova_df
    })
    
    # Plot the generated layout using agricolaeplotr or ggplot2
    output$generatedLayoutPlot <- renderPlot({
        req(rv$design_output)
        
        design_type_local <- input$designType
        plot_data_book <- rv$design_output$book
        
        p <- NULL
        plot_title <- paste(design_type_local, "Layout")
        
        if (design_type_local == "Completely Randomized Design") {
            labels_crd <- "Plot_Label"
            factor_name_crd <- "trt"
            
            total_plots <- nrow(plot_data_book)
            plot_ncols <- input$crd_ncols
            plot_nrows <- input$crd_nrows
            
            if (plot_nrows == 0) {
                plot_nrows <- ceiling(total_plots / plot_ncols)
            }
            
            if (plot_nrows == 0 || plot_ncols == 0) {
                return(NULL)
            }
            
            if (!(labels_crd %in% colnames(plot_data_book)) || !(factor_name_crd %in% colnames(plot_data_book))) {
                return(NULL)
            }
            
            p <- agricolaeplotr::plot_design_crd(
                design = rv$design_output,
                labels = labels_crd,
                factor_name = factor_name_crd,
                ncols = plot_ncols,
                nrows = plot_nrows
            ) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
            
        } else if (design_type_local == "Randomized Complete Block Design") {
            required_cols <- c("plots", "block", "trt", "Plot_Label", "Plot_Position_In_Block")
            if (!all(required_cols %in% colnames(plot_data_book))) {
                return(NULL)
            }
            
            plot_data_book$block <- factor(plot_data_book$block)
            plot_data_book$trt <- factor(plot_data_book$trt)
            
            p <- ggplot(plot_data_book, aes(x = Plot_Position_In_Block, y = 1)) +
                geom_tile(aes(fill = trt), color = "black", linewidth = 0.5) +
                geom_text(aes(label = Plot_Label), color = "black", size = 4) +
                facet_wrap(~ block, scales = "fixed", ncol = 1, strip.position = "left") +
                scale_fill_discrete(name = "Treatment") +
                labs(x = "Plot Position (within Block)", y = "Block") +
                theme_void() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                    strip.text.y.left = element_text(angle = 90, face = "bold"),
                    strip.background = element_rect(fill = "lightgray", color = "black"),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing.y = unit(0.5, "cm"),
                    legend.position = "right",
                    legend.title = element_text(size = 12),
                    legend.text = element_text(size = 10),
                    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                    plot.margin = unit(c(1, 1, 1, 1), "cm")
                )
            
        } else if (design_type_local == "Split-Plot Design") {
            factor_name_1_split <- "trt1"
            factor_name_2_split <- "trt2"
            block_split <- "block"
            labels_split <- "Plot_Label"
            
            if (!(factor_name_1_split %in% colnames(plot_data_book)) ||
                !(factor_name_2_split %in% colnames(plot_data_book)) ||
                !(labels_split %in% colnames(plot_data_book)) ||
                !(block_split %in% colnames(plot_data_book))) {
                return(NULL)
            }
            
            p <- agricolaeplotr::plot_split_rcbd(
                design = rv$design_output,
                factor_name_1 = factor_name_1_split,
                factor_name_2 = factor_name_2_split,
                labels = labels_split,
                y = block_split
            ) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
        }
        
        if (!is.null(p)) {
            p <- p +
                labs(title = plot_title) +
                theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                      legend.position = "right",
                      legend.title = element_text(size = 12),
                      legend.text = element_text(size = 10))
            print(p)
        } else {
            NULL
        }
    })
    
    # 2. Download Logbook
    output$downloadLogbook <- downloadHandler(
        filename = function() {
            design_abbr <- switch(input$designType,
                                  "Completely Randomized Design" = "CRD",
                                  "Randomized Complete Block Design" = "RCBD",
                                  "Split-Plot Design" = "SPLIT",
                                  "layout")
            paste0("field_logbook_", design_abbr, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            req(rv$logbook)
            logbook_to_download <- rv$logbook %>%
                mutate(
                    `Date Collected` = NA,
                    `Observed Trait 1` = NA,
                    `Observed Trait 2` = NA,
                    `Notes` = NA
                )
            write.csv(logbook_to_download, file, row.names = FALSE)
            showNotification("Logbook prepared for download.", type = "message", duration = 3)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)