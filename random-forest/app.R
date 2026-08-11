library(shiny)
library(randomForest)
library(ggplot2)
library(DT)
library(rpart)
library(rpart.plot)

# --- User Interface ---
ui <- fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
    titlePanel("🌲 Automated Random Forest Analyzer & Predictor"),
    
    sidebarLayout(
        sidebarPanel(
            h4("1. Model Training Data"),
            fileInput("file", "Upload Training CSV (With Y)", accept = c(".csv")),
            checkboxInput("header", "File contains header row", TRUE),
            
            hr(),
            h4("2. Configure Model"),
            uiOutput("target_select"),
            uiOutput("predictor_select"),
            actionButton("train_btn", "Train Model Engine", class = "btn-success btn-block", style = "width: 100%;"),
            
            hr(),
            h4("3. Predict on New Data (Option 2)"),
            p("Once trained, upload a new CSV with matching predictor columns to estimate your missing Y values."),
            fileInput("new_file", "Upload New CSV (No Y needed)", accept = c(".csv")),
            uiOutput("download_ui")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("📊 Data Preview", 
                         p("A glance at the first few rows of your training dataset."),
                         DTOutput("data_preview")),
                
                tabPanel("🧠 Model Evaluation", 
                         p("Performance metrics and out-of-bag (OOB) error rates."),
                         verbatimTextOutput("model_summary")),
                
                tabPanel("📈 Feature Importance", 
                         p("Which variables played the biggest role in the model's decisions?"),
                         plotOutput("importance_plot", height = "500px")),
                
                tabPanel("🌳 Representative Tree", 
                         p("Standalone Decision Tree trained on your data to visualize typical branching logic."),
                         plotOutput("tree_plot", height = "600px")),
                
                tabPanel("🔮 Prediction Results",
                         p("Preview of your new data with the generated predictions appended at the end."),
                         DTOutput("prediction_preview"))
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive training dataset reader
    raw_data <- reactive({
        req(input$file)
        read.csv(input$file$datapath, header = input$header, stringsAsFactors = TRUE)
    })
    
    # Dynamically populate Target Variable UI
    output$target_select <- renderUI({
        req(raw_data())
        selectInput("target", "Select Target Variable (Y):", choices = names(raw_data()))
    })
    
    # Dynamically populate Predictors UI
    output$predictor_select <- renderUI({
        req(raw_data(), input$target)
        predictors <- setdiff(names(raw_data()), input$target)
        selectizeInput("predictors", "Select Predictor Variables (X):", 
                       choices = predictors, selected = predictors, multiple = TRUE)
    })
    
    # Render Data Preview Table
    output$data_preview <- renderDT({
        req(raw_data())
        datatable(head(raw_data(), 100), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Train Random Forest Model on button click
    model_results <- eventReactive(input$train_btn, {
        req(raw_data(), input$target, input$predictors)
        
        working_data <- raw_data()[, c(input$target, input$predictors), drop = FALSE]
        working_data <- na.omit(working_data)
        
        if (is.character(working_data[[input$target]])) {
            working_data[[input$target]] <- as.factor(working_data[[input$target]])
        }
        
        model_formula <- as.formula(paste(input$target, "~ ."))
        
        withProgress(message = 'Growing the forest...', value = 0.5, {
            rf_mod <- randomForest(model_formula, data = working_data, importance = TRUE, ntree = 500)
        })
        
        return(rf_mod)
    })
    
    # Render Text Summary of the Model
    output$model_summary <- renderPrint({
        req(model_results())
        cat("### RANDOM FOREST SUMMARY ###\n\n")
        print(model_results())
    })
    
    # Render Feature Importance Plot
    output$importance_plot <- renderPlot({
        req(model_results())
        imp_matrix <- as.data.frame(importance(model_results()))
        imp_matrix$Variable <- rownames(imp_matrix)
        metric <- if ("MeanDecreaseAccuracy" %in% names(imp_matrix)) "MeanDecreaseAccuracy" else if ("%IncMSE" %in% names(imp_matrix)) "%IncMSE" else names(imp_matrix)[1]
        
        ggplot(imp_matrix, aes(x = reorder(Variable, .data[[metric]]), y = .data[[metric]])) +
            geom_bar(stat = "identity", fill = "#2c3e50", width = 0.7) +
            coord_flip() + labs(title = paste("Feature Importance based on", metric), x = "Features", y = metric) +
            theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })
    
    # Render a Standalone Decision Tree Flowchart
    output$tree_plot <- renderPlot({
        req(input$train_btn, raw_data(), input$target, input$predictors)
        working_data <- na.omit(raw_data()[, c(input$target, input$predictors), drop = FALSE])
        if (is.character(working_data[[input$target]])) working_data[[input$target]] <- as.factor(working_data[[input$target]])
        
        fit_tree <- rpart(as.formula(paste(input$target, "~ .")), data = working_data)
        rpart.plot(fit_tree, type = 2, extra = "auto", under = TRUE, fallen.leaves = TRUE, box.palette = "Auto", shadow.col = "gray", main = paste("Representative Decision Tree Structure for", input$target))
    })
    
    # --- NEW: Prediction Logic for Option 2 ---
    
    # Reactive function to generate predictions when a new file is uploaded
    predicted_data <- reactive({
        req(input$new_file, model_results(), input$predictors)
        
        # Read the new data sheet
        new_df <- read.csv(input$new_file$datapath, header = input$header, stringsAsFactors = TRUE)
        
        # Run the trained Random Forest model over the new data
        # The predict function maps matching column names automatically
        predictions <- predict(model_results(), newdata = new_df)
        
        # Append the predictions as a brand new column named after your target variable + '_Predicted'
        col_name <- paste0(input$target, "_Predicted")
        new_df[[col_name]] <- predictions
        
        return(new_df)
    })
    
    # Render Prediction Table Preview
    output$prediction_preview <- renderDT({
        req(predicted_data())
        datatable(head(predicted_data(), 100), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Show Download Button only when predictions are ready
    output$download_ui <- renderUI({
        req(predicted_data())
        downloadButton("download_predictions", "Download Predicted Data (.csv)", class = "btn-info", style = "width: 100%; margin-top: 10px;")
    })
    
    # Handle CSV download processing
    output$download_predictions <- downloadHandler(
        filename = function() {
            paste0("Predicted_", input$target, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(predicted_data(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)