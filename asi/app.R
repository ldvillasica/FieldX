# app.R
# This Shiny app is designed for batch analysis of soil aggregate stability by image quantification,
# integrating the Soil Health Institute's original R code.
# The app pairs initial and final images based on their filenames and provides
# a table and a bar graph of stability indices.

library(shiny)
library(dplyr)
library(EBImage) # Helpful resource to download EBImage: https://bioconductor.org/packages/release/bioc/html/EBImage.html
library(stringr) # For string manipulation, especially for filename parsing
library(ggplot2) # For plotting the bar graph

# Helper function to extract a base ID from filenames
# Assumes filenames are like 'sampleID_initial.jpg' or 'sampleID_final.jpg'
# You might need to adjust this regex based on your actual naming conventions
get_sample_id <- function(filename) {
    # Remove common suffixes like _initial, _final, _start, _end, and file extension
    id <- tolower(filename) %>%
        str_replace_all(c("_initial", "_final", "_start", "_end", "\\.jpg", "\\.jpeg", "\\.png", "\\.tif", "\\.tiff"), "") %>%
        str_trim() # Remove leading/trailing whitespace
    return(id)
}


# Define UI for application
ui <- fluidPage(
    titlePanel("Batch Soil Aggregate Stability by Image Quantification"),
    
    sidebarLayout(
        sidebarPanel(
            h4("1. Upload Your Images"),
            fileInput("initial_images",
                      "Select Initial Images (e.g., _initial.jpg)",
                      multiple = TRUE,
                      accept = c(".tif", ".tiff", ".jpg", ".jpeg", ".png")),
            fileInput("final_images",
                      "Select Final Images (e.g., _final.jpg)",
                      multiple = TRUE,
                      accept = c(".tif", ".tiff", ".jpg", ".jpeg", ".png")),
            helpText("Ensure initial and final images for the same sample have matching base filenames (e.g., 'sampleX_initial.jpg' and 'sampleX_final.jpg')."),
            hr(),
            
            h4("2. Define Cropping Area"),
            helpText("Adjust these values to isolate the three aggregates across all images. Values are in pixels."),
            numericInput("x_start", "X-axis Start (columns)", value = 250, min = 1),
            numericInput("x_end", "X-axis End (columns)", value = 650, min = 1),
            numericInput("y_start", "Y-axis Start (rows)", value = 350, min = 1),
            numericInput("y_end", "Y-axis End (rows)", value = 870, min = 1),
            hr(),
            
            h4("3. Run Batch Analysis"),
            actionButton("run_batch_analysis", "Run Batch Analysis", class = "btn-primary"),
            tags$br(), tags$br(),
            helpText("Upload images and set cropping parameters, then click 'Run Batch Analysis'.")
        ),
        
        mainPanel(
            h3("Batch Analysis Results"),
            uiOutput("analysis_status"), # To display messages or errors about file pairing
            hr(),
            
            h4("Summary of All Samples"),
            p("Below is the calculated Mean Stability Index for each matched sample pair."),
            dataTableOutput("batch_results_table"),
            hr(),
            
            h4("Stability Index Bar Graph"),
            p("Visual representation of the Mean Stability Index for each sample."),
            plotOutput("stability_bar_plot"), # New plot output
            hr(),
            
            h4("Visual Check: First Processed Sample"),
            p("For verification, the original and cropped images of the first successfully processed sample are displayed here."),
            fluidRow(
                column(6,
                       h5("Initial Image (Original)"),
                       imageOutput("plot_initial_original"),
                       h5("Initial Image (Cropped)"),
                       imageOutput("plot_initial_cropped")
                ),
                column(6,
                       h5("Final Image (Original)"),
                       imageOutput("plot_final_original"),
                       h5("Final Image (Cropped)"),
                       imageOutput("plot_final_cropped")
                )
            ),
            hr(),
            
            downloadButton("download_batch_results", "Download All Results (.csv)")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive value to store the results of all processed samples
    batch_analysis_results <- reactiveVal(NULL)
    analysis_status_msg <- reactiveVal("")
    first_processed_images <- reactiveVal(list(initial_original = NULL, initial_cropped = NULL,
                                               final_original = NULL, final_cropped = NULL))
    
    
    # Observe the run_batch_analysis button click
    observeEvent(input$run_batch_analysis, {
        req(input$initial_images, input$final_images) # Ensure files are uploaded
        
        analysis_status_msg("Matching files and initiating batch analysis...")
        all_results <- list()
        first_images_set <- FALSE # Flag to capture images of the first processed sample
        
        initial_files_df <- data.frame(
            original_name = input$initial_images$name,
            temp_path = input$initial_images$datapath
        ) %>%
            mutate(sample_id = sapply(original_name, get_sample_id))
        
        final_files_df <- data.frame(
            original_name = input$final_images$name,
            temp_path = input$final_images$datapath
        ) %>%
            mutate(sample_id = sapply(original_name, get_sample_id))
        
        # Join initial and final files by their derived sample_id
        matched_pairs <- inner_join(initial_files_df, final_files_df, by = "sample_id", suffix = c("_initial", "_final"))
        
        if (nrow(matched_pairs) == 0) {
            analysis_status_msg("No matching initial and final image pairs found based on filenames. Please check your naming convention.")
            batch_analysis_results(NULL)
            return()
        } else {
            analysis_status_msg(paste0("Found ", nrow(matched_pairs), " matching pairs. Starting analysis..."))
        }
        
        # Check for unmatched files and warn user
        unmatched_initial <- anti_join(initial_files_df, matched_pairs, by = c("sample_id" = "sample_id_initial"))
        unmatched_final <- anti_join(final_files_df, matched_pairs, by = c("sample_id" = "sample_id_final"))
        
        if (nrow(unmatched_initial) > 0) {
            warning(paste("Unmatched initial files:", paste(unmatched_initial$original_name, collapse = ", ")))
        }
        if (nrow(unmatched_final) > 0) {
            warning(paste("Unmatched final files:", paste(unmatched_final$original_name, collapse = ", ")))
        }
        
        # --- Batch Processing Loop ---
        withProgress(message = 'Processing image pairs...', value = 0, {
            for (i in 1:nrow(matched_pairs)) {
                sample_id <- matched_pairs$sample_id[i]
                initial_path <- matched_pairs$temp_path_initial[i]
                final_path <- matched_pairs$temp_path_final[i]
                
                incProgress(1/nrow(matched_pairs), detail = paste("Analyzing", sample_id))
                
                tryCatch({
                    img_Initial <- readImage(initial_path)
                    img_Final <- readImage(final_path)
                    
                    # Capture images of the first processed sample for display
                    if (!first_images_set) {
                        first_processed_images(list(
                            initial_original = img_Initial,
                            final_original = img_Final
                        ))
                        first_images_set <- TRUE
                    }
                    
                    # 1) Crop images
                    img_Initial_crop <- img_Initial[input$y_start:input$y_end, input$x_start:input$x_end, ]
                    img_Final_crop <- img_Final[input$y_start:input$y_end, input$x_start:input$x_end, ]
                    
                    # Capture cropped images for the first sample
                    if (i == 1) { # Only do this for the first one for display purposes
                        first_processed_images(c(first_processed_images(),
                                                 list(initial_cropped = img_Initial_crop,
                                                      final_cropped = img_Final_crop)))
                    }
                    
                    # 2) Make images gray scale
                    colorMode(img_Initial_crop) <- Grayscale
                    colorMode(img_Final_crop) <- Grayscale
                    
                    # 3) Get the threshold values and make binary
                    threshold_Initial <- otsu(img_Initial_crop)
                    threshold_Final <- otsu(img_Final_crop)
                    
                    img_binary_Initial <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_Initial_crop), threshold_Initial, SIMPLIFY=FALSE) )
                    img_binary_Final <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_Final_crop), threshold_Final, SIMPLIFY=FALSE) )
                    
                    area_Initial <- apply(img_binary_Initial, MARGIN = 3, sum, na.rm = TRUE)
                    area_Final <- apply(img_binary_Final, MARGIN = 3, sum, na.rm = TRUE)
                    
                    # 4) Calculate Stability Index
                    StabilityIndex <- mean(area_Initial / area_Final)
                    
                    all_results[[sample_id]] <- data.frame(
                        Sample_ID = sample_id,
                        Initial_Image = matched_pairs$original_name_initial[i],
                        Final_Image = matched_pairs$original_name_final[i],
                        Mean_Stability_Index = round(StabilityIndex, 4),
                        Status = "Processed Successfully",
                        stringsAsFactors = FALSE # Important for consistent data types
                    )
                    
                }, error = function(e) {
                    warning(paste("Error processing sample", sample_id, ":", e$message))
                    all_results[[sample_id]] <- data.frame(
                        Sample_ID = sample_id,
                        Initial_Image = matched_pairs$original_name_initial[i],
                        Final_Image = matched_pairs$original_name_final[i],
                        Mean_Stability_Index = NA,
                        Status = paste("Error:", e$message),
                        stringsAsFactors = FALSE
                    )
                })
            }
        })
        batch_analysis_results(do.call(rbind, all_results))
        analysis_status_msg("Batch analysis complete!")
    })
    
    # Render analysis status messages
    output$analysis_status <- renderUI({
        if (nchar(analysis_status_msg()) > 0) {
            tags$p(analysis_status_msg(), style = "color: blue; font-weight: bold;")
        }
    })
    
    # Output the batch results table
    output$batch_results_table <- renderDataTable({
        req(batch_analysis_results())
        batch_analysis_results()
    }, options = list(pageLength = 10))
    
    
    # --- NEW: Stability Index Bar Graph ---
    output$stability_bar_plot <- renderPlot({
        df <- batch_analysis_results()
        req(df) # Ensure data exists
        
        # Filter out NA values if any errors occurred
        df_plot <- df %>% filter(!is.na(Mean_Stability_Index))
        
        if (nrow(df_plot) == 0) {
            return(ggplot() + annotate("text", x=0.5, y=0.5, label="No valid data to plot.", size=5) + theme_void())
        }
        
        # Order bars by Stability Index (optional, but often helpful)
        df_plot$Sample_ID <- factor(df_plot$Sample_ID, levels = df_plot$Sample_ID[order(df_plot$Mean_Stability_Index)])
        
        ggplot(df_plot, aes(x = Sample_ID, y = Mean_Stability_Index)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            geom_text(aes(label = round(Mean_Stability_Index, 2)), vjust = -0.5, size = 3) + # Add value labels
            labs(title = "Mean Aggregate Stability Index per Sample",
                 x = "Sample ID",
                 y = "Mean Stability Index") +
            ylim(0, 1) + # Stability Index typically ranges from 0 to 1
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels if many samples
                  plot.title = element_text(hjust = 0.5))
    })
    
    
    # --- Image Plotting Outputs (for the first processed sample) ---
    output$plot_initial_original <- renderImage({
        img_list <- first_processed_images()
        if (!is.null(img_list$initial_original)) {
            outfile <- tempfile(fileext = '.png')
            writeImage(img_list$initial_original, outfile, type = 'png')
            list(src = outfile, contentType = 'image/png', width = "100%", height = "auto",
                 alt = "Initial Original Image")
        } else {
            # Display a placeholder if no image is available
            return(list(src = "https://via.placeholder.com/200x200?text=Upload+Initial+Images",
                        contentType = 'image/png',
                        width = 200, height = 200))
        }
    }, deleteFile = TRUE)
    
    output$plot_final_original <- renderImage({
        img_list <- first_processed_images()
        if (!is.null(img_list$final_original)) {
            outfile <- tempfile(fileext = '.png')
            writeImage(img_list$final_original, outfile, type = 'png')
            list(src = outfile, contentType = 'image/png', width = "100%", height = "auto",
                 alt = "Final Original Image")
        } else {
            return(list(src = "https://via.placeholder.com/200x200?text=Upload+Final+Images",
                        contentType = 'image/png',
                        width = 200, height = 200))
        }
    }, deleteFile = TRUE)
    
    output$plot_initial_cropped <- renderImage({
        img_list <- first_processed_images()
        if (!is.null(img_list$initial_cropped)) {
            outfile <- tempfile(fileext = '.png')
            writeImage(img_list$initial_cropped, outfile, type = 'png')
            list(src = outfile, contentType = 'image/png', width = "100%", height = "auto",
                 alt = "Initial Cropped Image")
        } else {
            return(list(src = "https://via.placeholder.com/200x200?text=Initial+Cropped+View",
                        contentType = 'image/png',
                        width = 200, height = 200))
        }
    }, deleteFile = TRUE)
    
    output$plot_final_cropped <- renderImage({
        img_list <- first_processed_images()
        if (!is.null(img_list$final_cropped)) {
            outfile <- tempfile(fileext = '.png')
            writeImage(img_list$final_cropped, outfile, type = 'png')
            list(src = outfile, contentType = 'image/png', width = "100%", height = "auto",
                 alt = "Final Cropped Image")
        } else {
            return(list(src = "https://via.placeholder.com/200x200?text=Final+Cropped+View",
                        contentType = 'image/png',
                        width = 200, height = 200))
        }
    }, deleteFile = TRUE)
    
    
    # Download button for batch results
    output$download_batch_results <- downloadHandler(
        filename = function() {
            paste0("slaking_batch_results_", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
        },
        content = function(file) {
            req(batch_analysis_results())
            write.csv(batch_analysis_results(), file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)