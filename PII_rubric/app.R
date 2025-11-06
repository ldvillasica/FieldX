# This is a complete, standalone R Shiny application file (app.R).

# Load necessary libraries
library(shiny)
library(DT) # For interactive data tables
library(dplyr) # For data manipulation

# --- 1. Data Definition ---
PII_CRITERIA <- data.frame(
    Score = 0:10,
    Level = c(
        "Absent", "Trace", "Very Weak", "Weak", "Weak to Moderate", "Moderate", 
        "Moderate to Strong", "Strong", "Very Strong", "Near Maximum", "Maximum"
    ),
    Description = c(
        "The process is absent; no visible or measurable evidence of its expression.",
        "Extremely weak expression; evidence is negligible, requiring very careful observation or advanced laboratory analysis to detect.",
        "Sparse and faint expression; features are few, thin, or small, but visibly present. Minimal impact on horizon characteristics.",
        "Sparse but distinct expression; features are clearly visible, but localized or very thin. The process is definitively active but not dominant.",
        "Approaching moderate expression; features are distinct and common, but still isolated or discontinuous. Borderline expression.",
        "Clear and easily observed expression; features are distinct, common, and moderately thick/sized. The process is a significant, but not overwhelming, factor.",
        "Common and prominent expression; features are distinct and widespread, often thick or continuous in localized areas, beginning to dominate the horizon appearance.",
        "Dominant expression; features are prominent, thick, and continuous across most relevant surfaces (e.g., ped faces, pores).",
        "Pervasive expression; features are ubiquitous, dominant, and thick/massive throughout the entire horizon.",
        "Intense and nearly complete expression; evidence is massive, continuous, and the process has virtually finished developing the diagnostic horizon.",
        "Maximum possible degree of expression; the process has completely formed a massive, diagnostic horizon (e.g., a massive petrocalcic horizon), representing the theoretical limit of its intensity."
    ),
    stringsAsFactors = FALSE
)

# New list of Soil Processes for the dropdown selector
PROCESS_LIST <- c(
    "Organic Matter Addition (Litter/Root)",
    "Mineral Addition (Dust/Ash)",
    "Leaching (Base/Cation Removal)",
    "Erosion/Removal (Physical Loss)",
    "Clay Illuviation (Bt Horizon)",
    "Clay Eluviation (E Horizon)",
    "Fe/Al Illuviation (Podzolization)",
    "Organic Translocation (Podzolization)",
    "Chemical Weathering (Primary Mineral Breakdown)",
    "Humification (Organic Matter Decomp.)",
    "Calcification (CaCO3 Accumulation)",
    "Decalcification (CaCO3 Removal)",
    "Salinization (Soluble Salt Accumulation)",
    "Desalinization (Soluble Salt Removal)",
    "Gleying (Reduction/Redox)",
    "Rubefacation (Fe Oxide Reddening)"
)

# --- 2. User Interface (UI) ---
ui <- fluidPage(
    
    # Application title and theme
    titlePanel(
        tags$span(
            style = "color: #3174AD; font-weight: bold;", 
            "PII Process Expression Rubric Tool"
        )
    ),
    
    # Custom CSS for a cleaner, modern look (similar to the React dark theme)
    tags$head(
        tags$style(HTML("
      body {
        background-color: #f7f7f7;
        color: #333;
      }
      .panel-heading {
        background-color: #3174AD !important;
        color: white !important;
        font-weight: bold;
      }
      .well {
        background-color: #ffffff;
        border-color: #ccc;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .score-display {
        font-size: 3em;
        font-weight: bold;
        color: #E66C37;
      }
    "))
    ),
    
    # Layout: Sidebar for selection, Main panel for tables
    sidebarLayout(
        
        # Sidebar Panel (Inputs and Live Criterion Display)
        sidebarPanel(
            width = 4,
            
            # Score Selector
            h3("Live Scoring Tool"),
            sliderInput(
                inputId = "pii_score",
                label = tags$div("Select PII Score:", class = "score-display"),
                min = 0,
                max = 10,
                value = 5,
                step = 1
            ),
            
            # Selected Criterion Details
            div(
                class = "well",
                h4(tags$b("Selected Intensity Level (Qualifier)")),
                htmlOutput("selected_level"),
                tags$hr(),
                h4(tags$b("Description")),
                htmlOutput("selected_description")
            ),
            
            # Observation Saving Form - UPDATED TO INCLUDE HORIZON NAME
            tags$hr(),
            h3("Save Observation"),
            # NEW INPUT FOR HORIZON NAME
            textInput( 
                inputId = "horizon_name",
                label = "Layer ID or Horizon Name:",
                placeholder = "e.g., 'A', 'Bt1', '2C'"
            ),
            selectInput(
                inputId = "process_name",
                label = "Select Process Expression Name:",
                choices = PROCESS_LIST,
                selected = PROCESS_LIST[1] # Select the first item by default
            ),
            actionButton(
                inputId = "save_obs",
                label = tags$b("Save Observation"),
                icon = icon("save"),
                class = "btn-primary btn-lg"
            )
        ),
        
        # Main Panel (Tables)
        mainPanel(
            width = 8,
            
            # Full Rubric Table
            tabsetPanel(
                tabPanel("Full PII Rubric",
                         tags$br(),
                         DTOutput("full_rubric_table")
                ),
                
                # Real-time Observations Table - NOW INCLUDES DOWNLOAD BUTTON
                tabPanel("Real-Time Observations",
                         tags$br(),
                         downloadButton("downloadData", "Download Observations (CSV)", class = "btn-success"),
                         tags$br(),
                         DTOutput("observations_table")
                )
            )
        )
    )
)

# --- 3. Server Logic (Server) ---
server <- function(input, output, session) {
    
    # Reactive state for storing observations (in-session persistence)
    rv <- reactiveValues(
        observations = data.frame(
            Time = character(),
            Score = integer(),
            Level = character(),
            ProcessName = character(),
            HorizonName = character(), # Initialized new column
            stringsAsFactors = FALSE
        )
    )
    
    # --- Live Criterion Display ---
    
    # Find the row corresponding to the selected score
    selected_criterion <- reactive({
        PII_CRITERIA %>%
            filter(Score == input$pii_score)
    })
    
    # Output the Level
    output$selected_level <- renderUI({
        req(selected_criterion())
        tags$p(tags$strong(selected_criterion()$Level), style = "font-size: 1.5em; color: #E66C37;")
    })
    
    # Output the Description
    output$selected_description <- renderUI({
        req(selected_criterion())
        tags$p(selected_criterion()$Description, style = "font-style: italic;")
    })
    
    # --- Observation Saving Logic ---
    observeEvent(input$save_obs, {
        # input$process_name is guaranteed to have a value since it's a selectInput
        req(input$process_name)
        
        process_name <- input$process_name
        horizon_name <- trimws(input$horizon_name) # Capture new input and trim whitespace
        
        # Validation check: Ensure Horizon Name is entered
        if (nchar(horizon_name) > 0) {
            new_obs <- data.frame(
                Time = format(Sys.time(), "%H:%M:%S"),
                Score = input$pii_score,
                Level = selected_criterion()$Level,
                ProcessName = process_name,
                HorizonName = horizon_name, # Include in data frame
                stringsAsFactors = FALSE
            )
            
            # Prepend new observation to reactive value
            rv$observations <- rbind(new_obs, rv$observations)
            
            # Clear the Horizon Name input field after saving
            updateTextInput(session, "horizon_name", value = "") 
            
        } else {
            showNotification("Error: Please enter the Layer ID or Horizon Name.", type = "error")
        }
    })
    
    # --- Table Rendering ---
    
    # Render the full PII Rubric table
    output$full_rubric_table <- renderDT({
        datatable(
            PII_CRITERIA,
            options = list(
                dom = 't', # Only show the table itself
                pageLength = 11,
                autoWidth = TRUE,
                columnDefs = list(
                    list(width = '10%', targets = 0),
                    list(width = '20%', targets = 1),
                    list(width = '70%', targets = 2)
                )
            ),
            rownames = FALSE,
            selection = 'none'
        ) %>% 
            formatStyle(
                'Score',
                target = 'row',
                backgroundColor = styleEqual(input$pii_score, 'lightblue'), # Highlight selected row
                fontWeight = styleEqual(input$pii_score, 'bold')
            )
    })
    
    # Render the Real-Time Observations table
    output$observations_table <- renderDT({
        req(nrow(rv$observations) > 0)
        
        datatable(
            rv$observations,
            options = list(
                paging = TRUE,
                pageLength = 10,
                searching = TRUE,
                ordering = TRUE
            ),
            rownames = FALSE,
            caption = "Observations are stored locally for this session only."
        )
    })
    
    # --- Download Handler for CSV ---
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("pii_observations-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            # Ensure data exists before trying to write it
            if (nrow(rv$observations) > 0) {
                write.csv(rv$observations, file, row.names = FALSE)
            } else {
                # Optional: write a placeholder or log a warning if the table is empty
                warning("Attempted to download empty observation table.")
            }
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)