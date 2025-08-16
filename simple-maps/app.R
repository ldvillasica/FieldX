# app.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(tools) # For file_path_sans_ext

# Set global options (as in your script)
options(timeout = 1200)

# Increase maximum file upload size for location data (CSV) if needed
# (Shapefile is now embedded, so this primarily applies to CSV)
options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Caraga Region Municipality Map"),
    
    sidebarLayout(
        sidebarPanel(
            # File input for location data (CSV) - ONLY this is uploaded by user now
            fileInput("loc_csv_file", "Upload Location CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$hr() # Horizontal rule for separation
            # Removed fileInput for shp_file as it's now embedded
        ),
        
        # Main panel for displaying the map
        mainPanel(
            plotOutput("caragaMap", height = "700px") # Adjust height as needed
        )
    )
)

# Define server logic required to draw the map
server <- function(input, output) {
    
    # Reactive expression to load location data
    loc_data <- reactive({
        req(input$loc_csv_file) # Require CSV file to be uploaded
        
        # Read the uploaded CSV file
        read.csv(input$loc_csv_file$datapath)
    })
    
    # Reactive expression to load embedded shapefile data
    shp_data <- reactive({
        # --- IMPORTANT: Place your Caraga Region shapefile components here ---
        # Create a folder named 'www' in the same directory as app.R
        # Inside 'www', create another folder named 'shp'
        # Place all your shapefile components (e.g., Caraga_Mun.shp, Caraga_Mun.dbf, etc.)
        # into the 'www/shp' folder.
        
        # Define the directory where the embedded shapefile components are located
        shp_dir <- file.path("www", "shp")
        
        # Define the base name of your main shapefile (.shp file without extension)
        # REPLACE 'Caraga_Mun' with the actual base name of your shapefile if it's different
        shp_base_name <- "Caraga_Mun" # Example: if your files are 'my_caraga.shp', 'my_caraga.dbf', set this to "my_caraga"
        
        # Check if the main .shp file exists in the expected location
        if (!file.exists(file.path(shp_dir, paste0(shp_base_name, ".shp")))) {
            stop(paste0("Error: The embedded shapefile '", shp_base_name, ".shp' was not found in the 'www/shp' folder.
                   Please ensure all Caraga region shapefile components are placed in 'www/shp' and the base name is correct."))
        }
        
        # Set GDAL environment variable to attempt to restore missing .shx
        Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
        
        # Read the shapefile from the embedded directory
        cara <- st_read(dsn = shp_dir, layer = shp_base_name)
        
        # Check if 'adm3_en' column exists in the embedded shapefile
        if (!"adm3_en" %in% names(cara)) {
            stop("Error: The embedded shapefile does not contain a column named 'adm3_en'.
            Please ensure your Caraga shapefile has this column for municipality identification.")
        }
        
        cara$adm3_en <- as.factor(cara$adm3_en) # Convert to factor
        
        return(cara)
    })
    
    
    output$caragaMap <- renderPlot({
        # Ensure both data sources are available before plotting
        req(loc_data(), shp_data())
        
        # Load world data (from rnaturalearth) - still included for completeness
        world <- ne_countries(scale = "medium", returnclass = "sf")
        
        # Get the loaded data
        loc1 <- loc_data()
        cara <- shp_data()
        
        # Subset data as in your original script
        cara1 <- subset(cara, select = c("adm3_en", "geometry"))
        # Note: "Butuan City" must exist in your uploaded shapefile's adm3_en column
        munici <- subset(cara1, adm3_en == "Butuan City", select = c("adm3_en", "geometry"))
        
        # --- Visualization (Mapping) ---
        ggplot() +
            # Plot the highlighted municipality (Butuan City)
            geom_sf(data = munici, aes(fill = adm3_en)) +
            # Plot all municipalities as a grey background
            geom_sf(data = cara, fill = "grey50", alpha = 0.5) +
            xlab("Longitude") +
            ylab("Latitude") +
            # Plot location points
            geom_point(data = loc1, aes(x = Longitude, y = Latitude, colour = Location)) +
            # Set legend titles
            labs(fill = "Municipality", colour = "Location") +
            # Set coordinate limits for the map view
            coord_sf(xlim = c(125.3, 125.75), ylim = c(8.7, 9.15), expand = TRUE) +
            # Add scale bar
            annotation_scale(location = "bl", width_hint = 0.5) +
            # Add north arrow
            annotation_north_arrow(location = "bl", which_north = "true",
                                   pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                   style = north_arrow_fancy_orienteering) +
            # Set a clean theme
            theme_bw()
    })
}

# Run the application
shinyApp(ui = ui, server = server)