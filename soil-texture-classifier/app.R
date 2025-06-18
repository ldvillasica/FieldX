# app.R

# --- Required R Packages ---
# If you don't have these installed, run these commands in your R console:
# install.packages(c("shiny", "ggplot2", "ggtern", "dplyr", "DT"))

library(shiny)
library(ggplot2)
library(ggtern)
library(dplyr) # For data manipulation (e.g., mutate, group_by, summarise)
library(DT)    # For interactive data tables

# --- Load USDA data for the ternary diagram background ---
# This dataset is built into the 'ggtern' package and defines the regions
# and labels for the USDA soil texture triangle.
data(USDA)

# Pre-process USDA data for text labels on the plot (mean position of each class)
# This helps in placing the class labels centrally within their regions on the plot.
USDA_text <- USDA %>%
    group_by(Label) %>%
    summarise(
        Clay = mean(Clay, na.rm = TRUE),
        Sand = mean(Sand, na.rm = TRUE),
        Silt = mean(Silt, na.rm = TRUE),
        .groups = 'drop' # Drop the grouping after summarising
    )

# --- Manual Soil Texture Classification Function (USDA System) ---
# This function implements the USDA soil texture triangle rules manually.
# It is robust against common data issues like non-100% sums and floating-point errors.
classify_soil_texture <- function(Sand, Silt, Clay) {
    # 1. Normalize to 100%:
    # Ensures that the sum of Sand, Silt, and Clay is exactly 100%. This is crucial
    # for accurate placement on the ternary diagram and adherence to USDA rules.
    total <- Sand + Silt + Clay
    Sand <- (Sand / total) * 100
    Silt <- (Silt / total) * 100
    Clay <- (Clay / total) * 100
    
    # 2. Rounding to mitigate floating-point issues:
    # Small inaccuracies from floating-point arithmetic can cause a point to fall
    # just outside a defined boundary, leading to incorrect or 'undefined' classification.
    # Rounding helps to align values with discrete boundaries.
    Sand <- round(Sand, 1)
    Silt <- round(Silt, 1)
    Clay <- round(Clay, 1)
    
    # 3. Adjust for sum deviation after rounding:
    # If rounding causes the sum to be slightly off from 100 (e.g., 99.9 or 100.1),
    # this step adjusts one component to bring the sum exactly to 100.
    # The adjustment is applied to the largest component to minimize its relative impact.
    current_sum <- Sand + Silt + Clay
    if (abs(current_sum - 100) > 0.01) { # Check for a deviation larger than a very small tolerance
        diff <- 100 - current_sum
        if (Sand >= Silt && Sand >= Clay) Sand <- Sand + diff
        else if (Silt >= Sand && Silt >= Clay) Silt <- Silt + diff
        else Clay <- Clay + diff
        
        # Re-round after adjustment to maintain consistency
        Sand <- round(Sand, 1)
        Silt <- round(Silt, 1)
        Clay <- round(Clay, 1)
    }
    
    # --- Start Classification Rules (Ordered for accuracy) ---
    # The order of these 'if' statements is critical. More specific or narrow categories
    # are checked first to ensure they take precedence over broader categories they might overlap.
    
    # 1. Sand: Characterized by very high sand content.
    if (Silt + 1.5 * Clay < 15) return("Sand")
    
    # 2. Loamy Sand: High sand, but with enough silt/clay to move it from pure sand.
    if (Sand >= 70 && Clay < 15) return("Loamy Sand")
    if (Sand > 52 && Clay < 20 && Silt + 2 * Clay < 30) return("Loamy Sand")
    
    # 3. High Clay Classes (Clay >= 40%): These are at the top of the triangle.
    if (Clay >= 40) {
        if (Silt >= 40) return("Silty Clay") # High Clay AND High Silt
        if (Sand >= 45) return("Sandy Clay") # High Clay AND High Sand
        return("Clay") # High Clay, but not fitting Silty Clay or Sandy Clay
    }
    
    # 4. High Silt Classes (Silt >= 50%): These are on the left side of the triangle.
    if (Silt >= 50) {
        if (Clay >= 20) return("Silty Clay Loam") # High Silt AND moderate Clay (20-39%)
        return("Silt Loam") # High Silt AND low Clay (<20%)
    }
    
    # 5. Silt: Extremely high silt content, very specific region.
    if (Silt >= 80 && Clay < 12) return("Silt")
    
    # 6. Sandy Loam: Moderate to high sand, low clay.
    # Differentiated from Loamy Sand by a specific line.
    if (Sand > 45 && Clay < 20) {
        if (Silt + 2 * Clay >= 30) return("Sandy Loam")
    }
    
    # ***** CRITICAL AREA: LOAM, CLAY LOAM, SANDY CLAY LOAM FIXES *****
    # These are central categories where boundaries can be nuanced.
    # Their order is crucial after ruling out more extreme classes.
    
    # 7. Sandy Clay Loam: Moderate clay, significant sand, lower silt.
    if (Clay >= 20 && Clay < 35 && Sand >= 45 && Silt < 28) return("Sandy Clay Loam")
    
    # 8. Clay Loam: Moderate to higher clay content.
    # This rule applies if the point hasn't been classified as Sandy Clay Loam (e.g., lower sand or higher silt).
    if (Clay >= 27 && Clay < 40) { # Clay 27-39% (since Pure Clay >=40 already handled)
        return("Clay Loam")
    }
    
    # 9. Loam (The Final Catch-All):
    # Any point not classified by the more specific rules above must fall into the 'Loam' region.
    # This makes the function exhaustive, ensuring all valid soil compositions get a class.
    return("Loam")
}


# --- Shiny UI (User Interface) ---
ui <- fluidPage(
    titlePanel("Soil Texture Classifier and Plotter"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Upload Your CSV File",
                      accept = c(".csv")),
            tags$hr(), # Horizontal line separator for visual organization
            checkboxInput("header", "Header", TRUE), # Option for CSV header row
            uiOutput("grouping_variable_selector") # Dynamic UI for selecting a grouping variable from the uploaded data
        ),
        
        mainPanel(
            tabsetPanel( # Tabbed interface for organizing outputs (plot and table)
                tabPanel("Ternary Plot", plotOutput("ternaryPlot", height = "600px")), # Tab for the ternary plot
                tabPanel("Soil Data Table",
                         DT::dataTableOutput("soilDataTable"), # Tab for the interactive data table
                         downloadButton("downloadTable", "Download Table (.csv)") # Download button for table
                )
            )
        )
    )
)

# --- Shiny Server Logic ---
server <- function(input, output, session) {
    
    # Reactive expression to read and process the uploaded user data
    userData <- reactive({
        req(input$file1) # 'req' ensures this reactive only runs if a file is uploaded
        
        inFile <- input$file1
        df <- read.csv(inFile$datapath, header = input$header)
        
        # Standardize column names to uppercase for consistent validation and processing.
        # This makes the app less sensitive to case variations in user's CSV headers.
        names(df) <- toupper(names(df))
        
        # Validate that essential columns (SAND, CLAY, SILT) exist in the uploaded file.
        if (!("SAND" %in% names(df)) ||
            !("CLAY" %in% names(df)) ||
            !("SILT" %in% names(df))) {
            validate(
                "Error: Your CSV file must contain 'SAND', 'CLAY', and 'SILT' columns (case-insensitive)."
            )
        }
        
        # Ensure Sand, Silt, Clay sum to 100% (or close to it) before classification.
        # This is crucial for accurate soil texture classification on the ternary diagram.
        # It also handles cases where the sum might be slightly off due to other components or rounding.
        df <- df %>%
            mutate(SUM_SSC = SAND + SILT + CLAY) %>% # Calculate the sum of Sand, Silt, Clay
            rowwise() %>% # Apply the normalization row by row
            mutate(
                SAND = (SAND / SUM_SSC) * 100, # Normalize Sand
                SILT = (SILT / SUM_SSC) * 100, # Normalize Silt
                CLAY = (CLAY / SUM_SSC) * 100  # Normalize Clay
            ) %>%
            ungroup() %>% # Remove row-wise grouping
            dplyr::select(-SUM_SSC) # Remove the temporary sum column using dplyr::select
        
        # --- MANUAL SOIL TEXTURE CLASSIFICATION ---
        # Apply the custom 'classify_soil_texture' function to each row of the dataframe.
        # 'mapply' is used here to apply the function element-wise across the Sand, Silt, Clay columns.
        df$TextureClass <- mapply(classify_soil_texture, df$SAND, df$SILT, df$CLAY)
        # --- END MANUAL SOIL TEXTURE CLASSIFICATION ---
        
        return(df)
    })
    
    # Render dynamic UI for selecting a grouping variable
    output$grouping_variable_selector <- renderUI({
        df <- userData() # Access the processed user data
        if (is.null(df)) {
            return(NULL) # If no data uploaded, don't show selector
        }
        
        # Identify potential grouping columns: character/factor columns, or numeric columns
        # with a limited number of unique values (e.g., <= 15 for meaningful color coding).
        grouping_cols <- names(df)[sapply(df, function(col) {
            if (is.character(col) || is.factor(col)) {
                TRUE # Always consider character/factor columns
            } else if (is.numeric(col)) {
                length(unique(col)) <= 15 # Numeric columns with few unique values
            } else {
                FALSE # Exclude other types (e.g., logical, dates)
            }
        })]
        
        # Exclude the generated 'TEXTURECLASS' as a grouping option, as it's an output.
        grouping_cols <- setdiff(grouping_cols, "TEXTURECLASS")
        
        if (length(grouping_cols) > 0) {
            # Create a named list for the selectInput choices (label = value)
            choices_for_selectInput <- setNames(as.list(grouping_cols), grouping_cols)
            # Add a "None" option to allow users to not group points
            choices_for_selectInput <- c(list("None" = ""), choices_for_selectInput)
            
            selectInput("group_by", "Select Grouping Variable (for color/table summary):",
                        choices = choices_for_selectInput,
                        selected = "") # Default selection is "None"
        } else {
            p("No suitable grouping variables found in your data.") # Message if no grouping columns
        }
    })
    
    # Render the Ternary Plot using ggtern
    # suppressWarnings is used here because ggtern can sometimes throw minor warnings
    # about data transformations, which don't affect the final plot.
    output$ternaryPlot <- suppressWarnings(renderPlot({
        df_user <- userData() # Access the processed user data
        
        # 1. Create the base ternary plot with USDA background
        p <- ggplot(data = USDA, aes(
            y = Clay, # Clay on the top axis (T)
            x = Sand, # Sand on the left axis (L)
            z = Silt  # Silt on the right axis (R)
        )) +
            # Use coord_tern for the ternary coordinate system
            coord_tern(L = "x", T = "y", R = "z") +
            # Add colored polygons for each soil texture class
            geom_polygon(
                aes(fill = Label), # Fill by USDA Label (soil texture class)
                alpha = 0.1,        # Transparency of the polygons
                linewidth = 0.5,    # Line thickness of boundaries
                color = "black"     # Color of boundary lines
            ) +
            # Add text labels for each soil texture class in the center of its region
            geom_text(data = USDA_text,
                      aes(label = Label),
                      color = 'black',
                      size = 3) + # Size of the text labels
            # Customize plot theme and labels
            theme_showarrows() + # Show arrows on the axes
            labs(yarrow = "Clay (%)",
                 zarrow = "Silt (%)",
                 xarrow = "Sand (%)") + # Axis labels
            theme_clockwise() + # Arrange axes clockwise
            theme(text = element_text(family = "Helvetica")) + # Font family
            guides(fill = FALSE) # Hide the fill legend for the USDA polygons (redundant with labels)
        
        # 2. Add user data points to the plot only if data is uploaded
        if (!is.null(df_user)) {
            # Check if a grouping variable is selected by the user
            if (!is.null(input$group_by) && input$group_by != "") {
                selected_group_col_upper <- toupper(input$group_by) # Get the uppercase column name
                
                # Add geom_point with color aesthetic if the selected grouping column exists in data
                if (selected_group_col_upper %in% names(df_user)) {
                    p <- p + geom_point(
                        data = df_user,
                        # Use aes_string for dynamic aesthetic mapping with string names
                        aes_string(
                            x = "SAND",
                            y = "CLAY",
                            z = "SILT",
                            color = selected_group_col_upper # Map color to the selected grouping variable
                        ),
                        size = 3, # Size of the data points
                        alpha = 0.8 # Transparency of data points
                    ) +
                        labs(color = input$group_by) # Set the legend title to the user-friendly grouping variable name
                } else {
                    # Fallback: Plot points without color if the selected column is not found (shouldn't happen with dynamic UI)
                    warning(paste("Selected grouping variable", input$group_by, "not found in data. Plotting without color."))
                    p <- p + geom_point(
                        data = df_user,
                        aes(x = SAND, y = CLAY, z = SILT),
                        size = 3,
                        alpha = 0.8
                    )
                }
            } else {
                # Plot points without color if no grouping variable is selected
                p <- p + geom_point(
                    data = df_user,
                    aes(x = SAND, y = CLAY, z = SILT), # Map points without a color aesthetic
                    size = 3,
                    alpha = 0.8
                )
            }
        }
        print(p) # Render the final plot
    }))
    
    # Reactive expression for the table data (full or summarized).
    outputTableData <- reactive({
        df_user <- userData() # Access the processed user data
        if (is.null(df_user)) {
            return(NULL) # If no data uploaded, return NULL for the table
        }
        
        df_to_display <- df_user # Start with the full processed data
        
        # Check if a grouping variable is selected for summarizing the table
        if (!is.null(input$group_by) && input$group_by != "") {
            selected_group_col_upper <- toupper(input$group_by) # Get the uppercase column name
            
            if (selected_group_col_upper %in% names(df_user)) {
                # Identify numeric columns for summarization (Sand, Silt, Clay, and any other numerics)
                numeric_cols_to_summarize <- names(df_user)[sapply(df_user, is.numeric)]
                # Exclude the grouping column itself if it's numeric and already used for grouping
                numeric_cols_to_summarize <- setdiff(numeric_cols_to_summarize, selected_group_col_upper)
                
                # Create the summary table
                summary_df <- df_user %>%
                    group_by(across(all_of(selected_group_col_upper))) %>% # Group by the selected variable
                    summarise(
                        # Calculate the mean for all specified numeric columns
                        across(all_of(numeric_cols_to_summarize), ~round(mean(.x, na.rm = TRUE), 2), .names = "Mean_{.col}"),
                        # List all unique TextureClasses found within each group
                        Unique_TextureClasses = paste(sort(unique(TextureClass)), collapse = ", "),
                        .groups = 'drop' # Remove grouping after summarising
                    )
                
                # Optionally rename the grouping column in the summary for better display (e.g., from "LOCATION" to "Location")
                if (input$group_by != selected_group_col_upper) {
                    names(summary_df)[names(summary_df) == selected_group_col_upper] <- input$group_by
                }
                
                df_to_display <- summary_df # Use the summarized data for display
                
            } else {
                # Fallback: If grouping column somehow not found, display full data with a warning
                warning(paste("Selected grouping variable", input$group_by, "not found for summary. Displaying full data."))
            }
        }
        
        return(df_to_display) # Return the data to be displayed in the table
    })
    
    # Render the interactive data table
    output$soilDataTable <- DT::renderDataTable({
        DT::datatable(outputTableData(),
                      options = list(pageLength = 10, scrollX = TRUE), # Set default page length and enable horizontal scrolling
                      rownames = FALSE) # Do not display R row names
    })
    
    # Download Handler for the Table
    output$downloadTable <- downloadHandler(
        filename = function() {
            paste("soil_data_summary_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(outputTableData(), file, row.names = FALSE)
        }
    )
}

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)