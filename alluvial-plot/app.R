# app.R

library(shiny)
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(RColorBrewer)
library(rlang) # CRITICAL for sym() and !! 

# --- 1. User Interface (UI) ---
ui <- fluidPage(
    
    # Application title
    titlePanel(
        div(
            span(icon("stream"), "Robust ggalluvial Plot Generator"),
            style = "color: #1F78B4;"
        )
    ),
    
    # Layout
    sidebarLayout(
        sidebarPanel(
            
            h3(span(icon("upload"), "1. Upload Data")),
            fileInput("file1", "Choose CSV/TSV File:",
                      accept = c(".csv", ".tsv", ".txt")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator:",
                         c("Comma" = ",",
                           "Semicolon" = ";",
                           "Tab" = "\t"),
                         selected = ","),
            
            hr(), 
            
            h3(span(icon("sliders"), "2. Plot Configuration")),
            
            # Dynamic UI output for column selection
            uiOutput("column_selectors"),
            
            # Plot customization controls
            hr(),
            textInput("plot_title", "Plot Title:", 
                      placeholder = "Enter a title (e.g., Stage Flow)"),
            
            selectInput("palette", "Color Palette:",
                        choices = rownames(RColorBrewer::brewer.pal.info),
                        selected = "Set1")
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            h3("Generated Alluvial Plot"),
            plotOutput("alluvialPlot", height = "550px"),
            
            hr(),
            h4(span(icon("table"), "Data Preview")),
            tableOutput("dataPreview")
            
        )
    )
)

# --- 2. Server Logic ---
server <- function(input, output, session) {
    
    # --- Reactive Data Loading and Cleaning ---
    dataInput <- reactive({
        req(input$file1)
        
        tryCatch({
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           stringsAsFactors = FALSE)
            
            # Convert all character columns to factor for proper strata handling
            # Add an explicit unique row ID *before* transformation for joining later
            df <- df %>% 
                mutate_if(is.character, factor) %>%
                mutate(unique_id = seq_len(nrow(.))) # CRITICAL: Add explicit ID
            
            return(df)
            
        }, error = function(e) {
            showNotification(paste("Error reading file. Check separator/header:", e$message), type = "error")
            return(NULL)
        })
    })
    
    # Data Preview
    output$dataPreview <- renderTable({
        req(dataInput())
        head(dataInput() %>% select(-unique_id), 6) # Hide the internal ID
    })
    
    
    # --- Dynamic UI Generation for Column Selectors ---
    
    output$column_selectors <- renderUI({
        df <- dataInput()
        req(df)
        
        col_names <- names(df) %>% setdiff("unique_id")
        strata_cols <- col_names[sapply(df[, col_names], function(col) is.factor(col) || is.character(col))]
        numeric_cols <- col_names[sapply(df[, col_names], is.numeric)]
        
        tagList(
            selectizeInput(
                "axis_cols",
                "Select Flow/Axis Columns (Stages, Order Matters):",
                choices = strata_cols,
                multiple = TRUE, 
                options = list(maxItems = 10),
                selected = NULL
            ),
            selectInput(
                "fill_col",
                "Select Fill/Color Column:",
                choices = c("None" = "None", strata_cols),
                selected = "None"
            ),
            selectInput(
                "weight_col",
                "Select Weight/Frequency Column (Optional):",
                choices = c("Count Observations (Default)" = "None", numeric_cols),
                selected = "None"
            )
        )
    })
    
    
    # --- Alluvial Plot Generation ---
    
    output$alluvialPlot <- renderPlot({
        
        df <- dataInput()
        req(df, input$axis_cols)
        
        if (length(input$axis_cols) < 2) {
            return(ggplot() + 
                       annotate("text", x = 0, y = 0, size=5, 
                                label = "Please select at least two columns for the alluvial stages (axes).") +
                       theme_void())
        }
        
        # --- Prepare Data and Aesthetics ---
        
        strata_vars <- input$axis_cols
        
        # 1. Data Transformation: Convert Wide Data to Long Data
        df_long <- ggalluvial::to_lodes_form(df, key = "stage", value = "stratum", axes = strata_vars, id = "unique_id")
        
        # 2. Add Alluvium ID (The unique flow line identifier)
        df_long$alluvium <- factor(df_long$unique_id)
        
        # 3. Define the Fill and Weight Variables
        fill_var_name <- if (input$fill_col != "None") input$fill_col else strata_vars[1]
        weight_var_name <- if (input$weight_col != "None") input$weight_col else NULL # Use NULL if not selected
        
        # --- Prepare Columns for Joining ---
        
        # Collect all columns needed in the long format, excluding the strata
        cols_to_keep <- c("unique_id")
        
        if (input$fill_col != "None") {
            cols_to_keep <- c(cols_to_keep, input$fill_col)
        }
        # Only include weight in join_df if it is NOT one of the axis columns (it's already in df_long if it is)
        if (input$weight_col != "None" && !(input$weight_col %in% strata_vars)) {
            cols_to_keep <- c(cols_to_keep, input$weight_col)
        }
        
        # Create a reduced data frame with only the necessary columns + ID
        join_df <- df %>% 
            select(all_of(cols_to_keep)) 
        
        # 4. Join back the required fill/weight columns
        df_long <- left_join(df_long, join_df, by = "unique_id")
        
        # 5. Construct the base aesthetic list (x, alluvium, stratum)
        final_aes <- list(
            x = quote(stage), 
            alluvium = quote(alluvium), 
            stratum = quote(stratum)
        )
        
        # 6. Adjust for Weight (If selected)
        if (input$weight_col != "None") {
            y_label <- input$weight_col
            # CRITICAL: Use sym() to turn the character string into a quoted name
            final_aes$weight <- sym(input$weight_col) 
        } else {
            y_label <- "Count of Observations"
            final_aes$weight <- quote(1) 
        }
        
        # --- Generate the Plot ---
        
        p <- ggplot(data = df_long,
                    mapping = do.call(aes, final_aes)
        ) +
            
            # FINAL CRITICAL FIX: Use sym() and !! for the fill aesthetic in geom layers
            geom_flow(aes(fill = !!sym(fill_var_name)), 
                      color = "darkgray", 
                      alpha = 0.6) +
            
            geom_stratum(aes(fill = !!sym(fill_var_name))) +
            
            geom_text(stat = "stratum", 
                      aes(label = after_stat(stratum)), 
                      color = "black", size = 3) +
            
            scale_fill_brewer(type = "qual", palette = input$palette) +
            labs(x = "Stages", 
                 y = y_label, 
                 fill = gsub("_", " ", fill_var_name)) +
            
            ggtitle(input$plot_title) +
            theme_minimal() +
            theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5, face = "bold"))
        
        print(p)
    }, res = 96) 
}

# --- 3. App Call ---
shinyApp(ui = ui, server = server)