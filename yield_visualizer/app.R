# app.R

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(DT)
library(ggfx) # For text outlines/glow effects

# --- UI (User Interface) ---
ui <- dashboardPage(
    dashboardHeader(title = "Yield Heatmap Visualizer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
            menuItem("Visualize Heatmap", tabName = "heatmap_viz", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # Tab 1: Data Upload
            tabItem(tabName = "upload_data",
                    h2("Upload Your Yield Data"),
                    fluidRow(
                        box(
                            title = "Data Input", status = "primary", solidHeader = TRUE,
                            width = 6,
                            fileInput("file1", "Choose CSV File",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            tags$hr(),
                            checkboxInput("header", "Header", TRUE),
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"')
                        ),
                        box(
                            title = "Data Preview", status = "info", solidHeader = TRUE,
                            width = 6,
                            DTOutput("contents")
                        )
                    )
            ),
            
            # Tab 2: Heatmap Visualization
            tabItem(tabName = "heatmap_viz",
                    h2("Yield Heatmap Visualization"),
                    fluidRow(
                        box(
                            title = "Heatmap Controls", status = "primary", solidHeader = TRUE,
                            width = 3,
                            uiOutput("grouping_variable_select"),
                            uiOutput("replication_select"),
                            actionButton("update_heatmap", "Generate Heatmap")
                        ),
                        box(
                            title = "Field Layout Heatmap", status = "success", solidHeader = TRUE,
                            width = 9,
                            plotOutput("yield_heatmap", height = "600px")
                        )
                    )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive expression to read uploaded data
    data_input <- reactive({
        req(input$file1) # Ensure a file is uploaded
        
        inFile <- input$file1
        df <- read_delim(inFile$datapath,
                         col_names = input$header,
                         delim = input$sep,
                         quote = input$quote,
                         show_col_types = FALSE)
        
        
        # Basic validation for required columns (adjust as needed)
        required_cols <- c("row_coord", "col_coord", "yield_kg_ha")
        if (!all(required_cols %in% names(df))) {
            stop(paste("Missing required columns. Please ensure your data has at least:",
                       paste(required_cols, collapse = ", ")))
        }
        df
    })
    
    # Output: Data preview table
    output$contents <- renderDT({
        datatable(data_input(), options = list(pageLength = 10))
    })
    
    # Dynamic UI for grouping variable selection
    output$grouping_variable_select <- renderUI({
        df <- data_input()
        exclude_cols <- c("row_coord", "col_coord", "yield_kg_ha", "rep", "block", "plot_id")
        group_cols <- names(df)[!names(df) %in% exclude_cols & sapply(df, function(x) !is.numeric(x))]
        if (length(group_cols) == 0) {
            helpText("No suitable grouping variables found. Ensure you have non-numeric columns like 'variety' or 'treatment'.")
        } else {
            selectInput("group_var", "Select Grouping Variable to Display on Tiles (Optional)",
                        choices = c("None" = "", group_cols))
        }
    })
    
    # Dynamic UI for replication selection
    output$replication_select <- renderUI({
        df <- data_input()
        req(df$rep)
        replications <- sort(unique(df$rep))
        selectInput("selected_rep", "Select Replication(s)",
                    choices = replications,
                    selected = replications[1],
                    multiple = TRUE)
    })
    
    
    # Reactive expression for filtered data for heatmap
    filtered_data <- eventReactive(input$update_heatmap, {
        df <- data_input()
        req(input$selected_rep)
        
        df_filtered <- df %>%
            filter(rep %in% input$selected_rep)
        
        df_filtered
    })
    
    
    # Output: Heatmap plot
    output$yield_heatmap <- renderPlot({
        data_for_plot <- filtered_data()
        req(data_for_plot)
        
        if (nrow(data_for_plot) == 0) {
            return(ggplot() + annotate("text", x = 0, y = 0, label = "No data for selected replication(s).", size = 6))
        }
        
        plot_title <- paste0("Yield Heatmap for Replication(s): ", paste(input$selected_rep, collapse = ", "))
        
        p <- ggplot(data_for_plot, aes(x = as.factor(col_coord), y = as.factor(row_coord), fill = yield_kg_ha)) +
            geom_tile(color = "black") +
            scale_fill_viridis_c(option = "plasma", name = "Yield (kg/ha)") +
            labs(title = plot_title,
                 x = "Field Column",
                 y = "Field Row",
                 caption = "Color represents Yield (kg/ha)") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "right",
                  plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "gray95", colour = NA)
            ) +
            # --- START OF CHANGE: Increased font size ---
            # Yield value text (top label)
            with_outer_glow(
                geom_text(aes(label = round(yield_kg_ha, 1)),
                          color = "white",
                          size = 4, # <--- INCREASED SIZE HERE (was 3)
                          vjust = 0.3),
                colour = "black",
                expand = 1
            )
        
        
        # If a grouping variable is selected, add it as a secondary label on tiles
        if (!is.null(input$group_var) && input$group_var != "" && input$group_var %in% names(data_for_plot)) {
            p <- p + with_outer_glow(
                geom_text(aes(label = .data[[input$group_var]]),
                          color = "black",
                          size = 3.5, # <--- INCREASED SIZE HERE (was 2.5)
                          vjust = 1.7),
                colour = "white",
                expand = 1
            )
        }
        # --- END OF CHANGE ---
        
        # Adjust facet_wrap to stack replications vertically
        if ("block" %in% names(data_for_plot) && length(unique(data_for_plot$block)) > 1) {
            p <- p + facet_wrap(~ rep + block, scales = "free", ncol = 1)
        } else if ("rep" %in% names(data_for_plot) && length(unique(data_for_plot$rep)) > 1) {
            p <- p + facet_wrap(~ rep, scales = "free", ncol = 1)
        }
        
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)