# Interactive PCA and Correlation Analysis with Biplot Grouping

# Load necessary libraries
library(shiny)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(corrplot) # For correlation matrix visualization

# Define UI for the Shiny app
ui <- fluidPage(
    # Application title
    titlePanel("Interactive PCA and Correlation Analysis with Biplot Grouping"),
    
    # Sidebar layout with a sidebar and main panel
    sidebarLayout(
        sidebarPanel(
            h4("1. Upload Data"),
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
                         selected = '"'),
            tags$hr(),
            
            h4("2. Select Variables for PCA"),
            uiOutput("pca_vars_selector"),
            uiOutput("group_var_selector"),
            tags$hr(),
            
            h4("3. Biplot Options"),
            radioButtons("ellipse_type", "Ellipse Type",
                         choices = c("Convex" = "convex",
                                     "Confidence" = "confidence",
                                     "Normal" = "norm"),
                         selected = "convex"),
            sliderInput("point_size", "Point Size", min = 1, max = 5, value = 2, step = 0.5),
            sliderInput("arrow_size", "Arrow Size", min = 0.1, max = 2, value = 0.8, step = 0.1),
            checkboxInput("repel_labels", "Repel Variable Labels", TRUE),
            tags$hr(),
            
            h4("4. Download Results"),
            downloadButton("downloadPcaVarCoords", "Download PCA Variable Coordinates"),
            downloadButton("downloadPcaIndCoords", "Download PCA Individual Coordinates"),
            downloadButton("downloadBiplot", "Download Biplot")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview",
                         h4("Uploaded Data (First 6 Rows)"),
                         tableOutput("headTable")),
                tabPanel("Correlation Analysis",
                         h4("Correlation Matrix of Selected PCA Variables"),
                         plotOutput("correlationPlot", height = "500px"),
                         verbatimTextOutput("correlationSummary")),
                tabPanel("PCA Summary",
                         h4("PCA Eigenvalues (Variance Explained)"),
                         plotOutput("screePlot"),
                         h4("PCA Summary Statistics"),
                         verbatimTextOutput("pcaSummary")),
                tabPanel("PCA Biplot",
                         h4("PCA Biplot with Grouping"),
                         plotOutput("biplotPlot", height = "600px"))
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive expression to read the uploaded data
    data <- reactive({
        req(input$file1) # Require a file to be uploaded
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # Output for data preview
    output$headTable <- renderTable({
        head(data())
    })
    
    # UI for selecting PCA variables
    output$pca_vars_selector <- renderUI({
        df <- data()
        if (!is.null(df)) {
            # Exclude non-numeric columns from default selection
            numeric_cols <- names(df)[sapply(df, is.numeric)]
            selectInput("pca_vars", "Select Variables for PCA",
                        choices = names(df),
                        selected = numeric_cols, # Select all numeric columns by default
                        multiple = TRUE)
        }
    })
    
    # UI for selecting the grouping variable
    output$group_var_selector <- renderUI({
        df <- data()
        if (!is.null(df)) {
            selectInput("group_var", "Select Grouping Variable",
                        choices = c("None", names(df)),
                        selected = "None")
        }
    })
    
    # Reactive expression for PCA data
    pca_data_filtered <- reactive({
        req(input$pca_vars) # Ensure pca_vars is selected
        df <- data()
        # Use `all_of()` to correctly select columns by their names
        df %>% dplyr::select(all_of(input$pca_vars))
    })
    
    # Reactive expression for grouping variable
    grouping_variable <- reactive({
        df <- data()
        if (input$group_var != "None" && !is.null(df[[input$group_var]])) {
            as.factor(df[[input$group_var]])
        } else {
            NULL # No grouping if "None" is selected or column doesn't exist
        }
    })
    
    # Reactive expression to perform PCA
    pca_results <- reactive({
        req(pca_data_filtered())
        # Ensure that pca_data_filtered() contains numeric columns only for PCA
        numeric_pca_data <- pca_data_filtered() %>%
            dplyr::select_if(is.numeric)
        
        # Handle case where no numeric columns are selected
        if (ncol(numeric_pca_data) == 0) {
            validate("Please select at least one numeric variable for PCA.")
            return(NULL)
        }
        PCA(numeric_pca_data, graph = FALSE)
    })
    
    # Reactive expression to calculate correlation matrix and p-values
    correlation_data <- reactive({
        req(pca_data_filtered())
        numeric_data <- pca_data_filtered() %>% dplyr::select_if(is.numeric)
        
        if (ncol(numeric_data) < 2) {
            validate("Please select at least two numeric variables for correlation analysis.")
            return(NULL)
        }
        
        M <- cor(numeric_data, use = "pairwise.complete.obs")
        p_mat <- cor.mtest(numeric_data, conf.level = 0.95)$p # Calculate p-values
        list(M = M, p_mat = p_mat)
    })
    
    # Output for correlation plot
    output$correlationPlot <- renderPlot({
        req(correlation_data())
        cor_data <- correlation_data()
        corrplot(cor_data$M, method = "square", type = "lower",
                 tl.col = "black", tl.srt = 45,
                 addCoef.col = "black", # Add coefficients to the plot
                 number.cex = 0.7,
                 p.mat = cor_data$p_mat, # Provide p-values
                 sig.level = 0.05,     # Significance level
                 insig = "blank"       # Blank out insignificant relationships
        )
    })
    
    # Output for correlation summary (text)
    output$correlationSummary <- renderPrint({
        req(correlation_data())
        cor_data <- correlation_data()
        print("Correlation Matrix:")
        print(cor_data$M)
        print("\nP-value Matrix:")
        print(cor_data$p_mat)
    })
    
    
    # Output for scree plot
    output$screePlot <- renderPlot({
        req(pca_results())
        fviz_eig(pca_results(), addlabels = TRUE, ylim = c(0, 50))
    })
    
    # Output for PCA summary
    output$pcaSummary <- renderPrint({
        req(pca_results())
        summary(pca_results())
    })
    
    # Output for biplot
    output$biplotPlot <- renderPlot({
        req(pca_results())
        
        p <- fviz_pca_biplot(pca_results(),
                             geom.ind = "point",
                             col.ind = if (!is.null(grouping_variable())) grouping_variable() else "steelblue",
                             palette = "jco",
                             addEllipses = !is.null(grouping_variable()), # Only add ellipses if grouping
                             ellipse.type = input$ellipse_type,
                             label = "var",
                             pointshape = 19,
                             pointsize = input$point_size,
                             repel = input$repel_labels,
                             arrowsize = input$arrow_size,
                             col.var = "black",
                             title = "PCA Biplot of Data",
                             legend.title = if (!is.null(grouping_variable())) paste("Group by", input$group_var) else "No Grouping"
        ) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
        
        print(p)
    })
    
    # Download handlers
    output$downloadPcaVarCoords <- downloadHandler(
        filename = function() {
            paste("pca_variable_coordinates-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame(pca_results()$var$coord), file, row.names = TRUE)
        }
    )
    
    output$downloadPcaIndCoords <- downloadHandler(
        filename = function() {
            paste("pca_individual_coordinates-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame(pca_results()$ind$coord), file, row.names = TRUE)
        }
    )
    
    # Reactive value to store the biplot plot for download
    biplot_plot_reactive <- reactive({
        req(pca_results())
        
        p <- fviz_pca_biplot(pca_results(),
                             geom.ind = "point",
                             col.ind = if (!is.null(grouping_variable())) grouping_variable() else "steelblue",
                             palette = "jco",
                             addEllipses = !is.null(grouping_variable()),
                             ellipse.type = input$ellipse_type,
                             label = "var",
                             pointshape = 19,
                             pointsize = input$point_size,
                             repel = input$repel_labels,
                             arrowsize = input$arrow_size,
                             col.var = "black",
                             title = "PCA Biplot of Data",
                             legend.title = if (!is.null(grouping_variable())) paste("Group by", input$group_var) else "No Grouping"
        ) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
        return(p)
    })
    
    output$downloadBiplot <- downloadHandler(
        filename = function() {
            paste("pca_biplot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            # Use the reactive plot directly
            ggsave(file, plot = biplot_plot_reactive(), device = "png", width = 10, height = 8, units = "in", dpi = 300)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)