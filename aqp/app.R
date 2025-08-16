# soil profile sketcher

# Load necessary packages
library(shiny)
library(aqp)
library(RColorBrewer)
library(latticeExtra)
library(plyr)
library(reshape)
library(dendextend) # For dendrogram manipulation and plotting
library(cluster)    # For daisy (dissimilarity matrix) - though profile_compare is used, this is a common companion
library(grid)       # Core graphics system for viewports (used by gridExtra implicitly)
library(gridExtra)  # For arranging plots

# ---
# Define UI for soil profile sketcher app
ui <- fluidPage(
    titlePanel("Flexible Soil Profile Sketcher"),
    
    sidebarLayout(
        sidebarPanel(
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
            # Sliders for controlling plot width and height
            sliderInput("plotWidth", "Plot Width (pixels):",
                        min = 400, max = 2000, value = 1200, step = 50), # Increased max width
            sliderInput("plotHeight", "Plot Height (pixels):",
                        min = 300, max = 2000, value = 900, step = 50), # Increased max height
            tags$hr(),
            helpText(
                "Your CSV file must include columns named exactly:",
                tags$ul(
                    tags$li("Profile"),
                    tags$li("top"),
                    tags$li("bottom"),
                    tags$li("Hue"),
                    tags$li("Value"),
                    tags$li("Chroma"),
                    tags$li("Horizon.Name")
                )
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Profile Sketch",
                         h3("Soil Profile Sketch with Depth Labeling"),
                         fluidRow(
                             column(12, align = "center",
                                    plotOutput("profilePlot",
                                               width = "auto",
                                               height = "auto")
                             )
                         )
                ),
                tabPanel("Profile Similarity (Dendrogram)",
                         h3("Soil Profile Similarity Dendrogram"),
                         fluidRow(
                             column(12, align = "center",
                                    # This single plotOutput will contain both the dendrogram and profiles
                                    plotOutput("dendrogramPlot",
                                               width = "auto",
                                               height = "auto")
                             )
                         )
                )
            )
        )
    )
)

# ---
# Define server logic to read file and plot
server <- function(input, output) {
    
    pedon_data <- reactive({
        req(input$file1) # Require a file to be uploaded
        
        inFile <- input$file1
        
        df <- read.csv(inFile$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        # Basic validation for required columns
        required_cols <- c("Profile", "top", "bottom",
                           "Hue", "Value", "Chroma", "Horizon.Name")
        if (!all(required_cols %in% names(df))) {
            stop(paste("Missing required columns. Please ensure your CSV has:",
                       paste(required_cols, collapse = ", ")))
        }
        
        # Color convert munsell to rgb
        df$color <- munsell2rgb(df$Hue, df$Value, df$Chroma)
        
        # Add for label (though not directly used in the current plot, good for debug)
        df <- within(df, label <- paste(Profile, Horizon.Name, sep=" , "))
        
        # Convert to SoilProfileCollection object using 'top' and 'bottom'
        depths(df) <- Profile ~ top + bottom
        return(df)
    })
    
    output$profilePlot <- renderPlot({
        pedon <- pedon_data()
        if (!is.null(pedon)) {
            # --- IMPORTANT: Adjust plot margins here ---
            old_par <- par(no.readonly = TRUE)
            on.exit(par(old_par))
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            
            plot(pedon,
                 name = 'Horizon.Name',
                 color = 'color',
                 cex.names = .7,
                 width = 0.3,
                 depth.axis = FALSE,
                 hz.depths = TRUE,
                 fixLabelCollisions = TRUE,
                 hz.depths.offset = 0.08)
        }
    },
    res = 96,
    width = function() input$plotWidth,
    height = function() input$plotHeight
    )
    
    
    output$dendrogramPlot <- renderPlot({
        pedon <- pedon_data()
        if (!is.null(pedon)) {
            
            # Debugging check: Confirm aqp is loaded
            if (!requireNamespace("aqp", quietly = TRUE)) {
                stop("The 'aqp' package is not loaded. Please install and load it.")
            }
            print("aqp package is loaded and dendrogram plotting is starting.")
            
            # 1. Calculate Dissimilarity Matrix
            # Explicitly calling aqp::profile_compare to resolve "could not find function" error
            d <- aqp::profile_compare(pedon, vars = c("Hue", "Value", "Chroma"),
                                      max_d = 200, # Max depth for comparison
                                      wt = c(1, 1, 1), # Weights for Hue, Value, Chroma
                                      name.field = "Horizon.Name")
            
            # 2. Hierarchical Clustering
            h <- hclust(d, method = "average") # You can experiment with different methods: "ward.D2", "complete", "average"
            
            # Get the order of profiles from the clustering
            profile_order <- h$labels[h$order]
            
            # Reorder the SoilProfileCollection object according to the dendrogram order
            # This is crucial for aligning the plots
            pedon_ordered <- pedon[profile_order, ]
            
            # 3. Create the Dendrogram Plot
            # Convert hclust to dendrogram object for more plotting options
            dend <- as.dendrogram(h)
            
            # Adjust dendrogram for plotting (e.g., smaller labels if many profiles)
            dend <- dend %>%
                set("labels_cex", 0.7) %>%
                set("branches_lwd", 1.5)
            
            # Save current par settings to restore them later
            old_par <- par(no.readonly = TRUE)
            on.exit(par(old_par)) # Ensure settings are restored when function exits
            
            # Set up the layout for grid.arrange
            # Top row for dendrogram, bottom row for profiles
            # The heights ratio will need tuning based on the number of profiles and plot sizes
            # We need to explicitly manage the plotting with grid functions
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, heights = unit(c(0.3, 0.7), "npc"))))
            
            
            # Plot Dendrogram in top viewport
            pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
            # Margins for dendrogram: bottom, left, top, right
            # Left margin needs to be generous if labels are long and horizontal=TRUE
            par(mar = c(0.1, 5, 3, 0.1), new = TRUE)
            plot(dend, horiz = TRUE, main = "", axes = FALSE) # No title here, add with title()
            title("Hierarchical Clustering of Soil Profiles", line = 1)
            popViewport()
            
            # Plot Soil Profiles in bottom viewport
            pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
            # Margins for profiles. Set new=TRUE so it draws on top of the grid layout
            par(mar = c(0.5, 0.5, 0.5, 0.5), new = TRUE)
            plot(pedon_ordered,
                 name = 'Horizon.Name',
                 color = 'color',
                 cex.names = .7,
                 width = 0.3,
                 depth.axis = FALSE,
                 hz.depths = TRUE,
                 fixLabelCollisions = TRUE,
                 hz.depths.offset = 0.08,
                 plot.order = FALSE) # Important: do not let plot.SoilProfileCollection reorder
            popViewport()
        }
    },
    res = 96,
    width = function() input$plotWidth,
    height = function() input$plotHeight
    )
}

# ---
# Run the application
shinyApp(ui = ui, server = server)