# Soil Texture Plotter

library(shiny)
library(ggplot2)
library(ggtern)
library(dplyr)

# Load USDA data (assuming it's available with ggtern)
data(USDA)

# Pre-process USDA data for text labels
USDA_text <- USDA %>%
    group_by(Label) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

ui <- fluidPage(
    titlePanel("Soil Texture Plotter"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Upload Your CSV File",
                      accept = c(".csv")),
            tags$hr(),
            # --- Added Note on Data Requirements ---
            wellPanel(
                p(strong("Your CSV file should include:")),
                tags$ul(
                    tags$li("Columns named ", code("SAND"), ", ", code("CLAY"), ", and ", code("SILT"), " (case-insensitive)."),
                    tags$li("These columns should contain numerical values representing percentages.")
                ),
                p("You can also include other columns for grouping your data points (e.g., location, sample ID).")
            ),
            tags$hr(),
            # --- End of Added Note ---
            checkboxInput("header", "Header", TRUE),
            uiOutput("grouping_variable_selector") # Dynamic UI for selecting grouping variable
        ),
        
        mainPanel(
            plotOutput("ternaryPlot")
        )
    )
)

server <- function(input, output, session) {
    
    userData <- reactive({
        req(input$file1) # Require a file to be uploaded
        
        inFile <- input$file1
        df <- read.csv(inFile$datapath, header = input$header)
        return(df)
    })
    
    output$grouping_variable_selector <- renderUI({
        df <- userData()
        if (is.null(df)) {
            return(NULL)
        }
        
        grouping_cols <- names(df)[sapply(df, function(col) {
            if (is.character(col) || is.factor(col)) {
                TRUE
            } else if (is.numeric(col)) {
                length(unique(col)) <= 15
            } else {
                FALSE
            }
        })]
        
        if (length(grouping_cols) > 0) {
            choices_for_selectInput <- setNames(as.list(grouping_cols), grouping_cols)
            choices_for_selectInput <- c(list("None" = ""), choices_for_selectInput)
            
            selectInput("group_by", "Select Grouping Variable (for color):",
                        choices = choices_for_selectInput,
                        selected = "")
        } else {
            p("No suitable grouping variables found in your data.")
        }
    })
    
    output$ternaryPlot <- suppressWarnings(renderPlot({
        df_user <- userData()
        
        if (!("SAND" %in% toupper(names(df_user))) ||
            !("CLAY" %in% toupper(names(df_user))) ||
            !("SILT" %in% toupper(names(df_user)))) {
            validate(
                "Error: Your CSV file must contain 'SAND', 'CLAY', and 'SILT' columns (case-insensitive)."
            )
        }
        
        names(df_user) <- toupper(names(df_user))
        
        p <- ggplot(data = USDA, aes(
            y = Clay,
            x = Sand,
            z = Silt
        )) +
            coord_tern(L = "x", T = "y", R = "z") +
            geom_polygon(
                aes(fill = Label),
                alpha = 0.1,
                linewidth = 0.5,
                color = "black"
            ) +
            geom_text(data = USDA_text,
                      aes(label = Label),
                      color = 'black',
                      size = 2) +
            theme_showarrows() +
            labs(yarrow = "Clay (%)",
                 zarrow = "Silt (%)",
                 xarrow = "Sand (%)") +
            theme_clockwise() +
            theme(text = element_text(family = "Helvetica")) +
            guides(fill = FALSE)
        
        # Add user data points ONLY when a file is uploaded
        if (!is.null(df_user)) {
            
            # *** REVISED CONDITIONAL geom_point ADDITION ***
            if (!is.null(input$group_by) && input$group_by != "") {
                selected_group_col_upper <- toupper(input$group_by)
                
                if (selected_group_col_upper %in% names(df_user)) {
                    # Use aes_string directly for the conditional color aesthetic
                    p <- p + geom_point(
                        data = df_user,
                        aes_string(
                            x = "SAND",
                            y = "CLAY",
                            z = "SILT",
                            color = selected_group_col_upper # Pass the string directly
                        ),
                        size = 3
                    ) +
                        labs(color = input$group_by)
                } else {
                    warning(paste("Selected grouping variable", input$group_by, "not found in data. Plotting without color."))
                    # Fallback to no color if column not found
                    p <- p + geom_point(
                        data = df_user,
                        aes(
                            x = SAND,
                            y = CLAY,
                            z = SILT
                        ),
                        size = 3
                    )
                }
            } else {
                # No grouping variable selected, plot without color
                p <- p + geom_point(
                    data = df_user,
                    aes(
                        x = SAND,
                        y = CLAY,
                        z = SILT
                    ),
                    size = 3
                )
            }
        }
        print(p)
    }))
}

shinyApp(ui = ui, server = server)