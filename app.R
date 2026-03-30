# ==============================================================================
# Project: Potential Denitrification Rate (PDR) – Shiny Web App
# Published on: Posit Connect Cloud
# Author: Biplob Biswash
# ==============================================================================

# -----------------------------
# Load libraries 
# -----------------------------



library(shiny)
library(leaflet)
library(terra)
library(sf)
library(leaflet.extras)
library(bslib)

# -----------------------------
# Raster file paths(.tif file)
# -------------------------------

# -------- Dataset 1 --------------------
ndvi_path_1 <- "data1/NDVI.tif"
b11_path_1  <- "data1/B11.tif"
lulc_path_1 <- "data1/LULC.tif"

# -------- Dataset 2 ----------------------
ndvi_path_2 <- "data2/NDVI_2.tif"
b11_path_2  <- "data2/B11_2.tif"
lulc_path_2 <- "data2/LULC_2.tif"

# ---------------------------------
# Load rasters file in list 
# ------------------------------
rasters <- list(
  set1 = list(
    ndvi = rast(ndvi_path_1),
    b11  = rast(b11_path_1),
    lulc = rast(lulc_path_1)
  ),
  set2 = list(
    ndvi = rast(ndvi_path_2),
    b11  = rast(b11_path_2),
    lulc = rast(lulc_path_2)
  )
)





# =========================================================================
# UI .web front-end (shinny framework for resposive work)
# ==============================================================================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
   .sidebar-panel { padding: 12px; }
  .main-panel { padding: 8px; background:#e6ddd6; }
      .app-title { font-weight: 700; font-size: 25px; color:#df602b; }
    "))
  ),
  
  div(class = "container",
  div(class = "row mb-3",
       div(class = "col-12",
     span(class = "app-title",
                  "Potential Denitrification Rate (PDR) Visualization"),br(),
     tags$small("*Draw an area on the map for Estimation of potential denitrification Rate.", style = "margin-left:8px;color:#2596be;"),br(),
          )
      )
  ),
  
  sidebarLayout(
    
 sidebarPanel(class = "sidebar-panel", width = 3,
                 
       selectInput("dataset","Please Select Region",
                   choices = c("Arial Beal" = "set1","Padma River" = "set2"
                   ),
                  selected = "set1",
                   width = "100%"
                 ),
                 
                 tags$hr(),
                 
                 actionButton("compute", "Compute PDR",
                              class = "btn-primary", width = "100%"),
                 
              br(), br(),
                 
                 downloadButton("download_pdr", "Download PDR GeoTIFF",
                                class = "btn-secondary", style = "width:100%"),
                 
                 br(), br(),
                 
                 downloadButton("download_plot", "Download Plot Image",
                                class = "btn-secondary", style = "width:100%"),
                 
                 tags$hr(),
                 tags$small(
                   "Tip: Draw a rectangle on the map to define AOI.",
                   style = "color:#666;"
                 )
    ),
    
    mainPanel(class = "main-panel", width = 9,
              tabsetPanel(
                
                tabPanel("Map",
          leafletOutput("map", height = "560px")
                ),
                
                tabPanel("Plots",
                         fluidRow(
            column(6, plotOutput("pdr_plot", height = 300)),
             column(6, plotOutput("pdr_hist", height = 300))
                         ),
         fluidRow(
            column(12, verbatimTextOutput("stats")))
                ),
                
   tabPanel("Logs / Info",
           verbatimTextOutput("log_text")
                )
              )
    )
  )
)





# ==============================================================================
# SERVER side operation
# ==============================================================================
server <- function(input, output, session) {
  
  # --------------------------------------
  # Select current raster set from list
  # --------------------------------------
  current_rasters <- reactive({
    rasters[[input$dataset]]
  })
  
  # -----------------------------
  # Leaflet Map(online map)
  # -----------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 90.3563, lat = 23.6850, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "draw",
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(color = "red")
        ),
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions()
      )
  })
  
  # ---------------------------
  # Select desire AREA (aoi)
  # -----------------------------
  aoi <- reactiveVal(NULL)
  # initialize at server top
  
  PDR_val <- reactiveVal(NULL)
  
  
  observeEvent(input$map_draw_new_feature, {
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    poly <- st_polygon(list(do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]]))
    ))) |> st_sfc(crs = 4326)
    
    aoi(poly)
    
    leafletProxy("map") %>%
      clearGroup("aoi") %>%
      addPolygons(data = poly, color = "red",
                  weight = 2, fill = FALSE, group = "aoi")
  })
  
  
  
  
  # --------------------------
  # Logs /notifiy
  # --------------------------
  output$log_text <- renderText({
    if (is.null(aoi())) "Draw AOI rectangle on map."
    else paste("AOI selected\nDataset:", input$dataset)
  })
  
  # -------------------------------
  # Empty plots textstats
  # -------------------------------
  output$pdr_plot <- renderPlot({
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "PDR map will appear after computation")
  })
  
  output$pdr_hist <- renderPlot({
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "",
    main = "Histogram will appear after computation")
  })
  
  
  
  
  
  # -----------------------------
  # Compute PDR -MAIN OBSERVATION
  # -----------------------------
  observeEvent(input$compute, {
    req(aoi())
    
    r <- current_rasters()
    
    # ensure AOI in raster CRS
    poly <- tryCatch({ st_transform(aoi(), crs(r$ndvi)) }, error = function(e) NULL)
    if (is.null(poly)) {
      showModal(modalDialog(title = "Error", "Failed to transform AOI to raster CRS."))
      return()
    }
    
    # Check intersection--oUTSIDE DETECTION
    raster_extent <- st_as_sfc(st_bbox(r$ndvi))
    if (!st_intersects(poly, raster_extent, sparse = FALSE)[1,1]) {
      showModal(modalDialog(title = "Warning", "Selected area is outside the raster extent."))
      return()
    }
    
    
    withProgress(message = "Computing PDR", value = 0, {
      
      incProgress(0.15, detail="Cropping rasters")
      ndvi <- crop(r$ndvi, vect(poly))
      b11 <- clamp(r$b11, 0, 1)
    
      b11  <- crop(b11, vect(poly))
      lulc <- crop(r$lulc, vect(poly))
      
      incProgress(0.15, detail="Resampling")
      b11  <- resample(b11, ndvi, method = "bilinear")
      lulc <- resample(lulc, ndvi, method = "near")
      
      incProgress(0.2, detail="LULC intercept")
      rcl <- matrix(c(
        1, 0.3983,
        2, -0.2820,
        3, 0.0437,
        4, -0.1599,
        5, 0.1230
      ), ncol = 2, byrow = TRUE)
      
      intc <- classify(lulc, rcl, others = NA)
     
      incProgress(0.3, detail="PDR calculation")
      exp_term <- (-1.357) + intc + 1.264 * ndvi - 2.271 * b11
      PDR <- 240 * (10 ^ exp_term)
      PDR_masked <- mask(PDR, ndvi)
      
      PDR_val(PDR_masked)   # <-- THIS triggers reactivity
      
      session$userData$PDR <- PDR_masked
      
      # inside compute
    
      
      incProgress(0.2, detail="Rendering outputs")
      
      output$pdr_plot <- renderPlot({
        plot(PDR_masked, col = hcl.colors(50, "Viridis"),
             main = "Denitrification Rate (mg N2O-N/m²/h)")
      })
      
      output$pdr_hist <- renderPlot({
        hist(values(PDR_masked), breaks = 50,
             main = "PDR Histogram", xlab = "PDR")
        
      })
    })
  })

  # -----------------------------
  # Stats
  # -----------------------------
  output$stats <- renderText({
    req(PDR_val())   # forces update every time
    
    pdr <- PDR_val()
    v <- values(pdr)
    v <- v[!is.na(v)]
    sprintf(
      "PDR stats:N=%d,Mean=%.3f,SD=%.3f,Min=%.3f,Max=%.3f",
      length(v), mean(v), sd(v), min(v), max(v)
    
    )
  })
  
  # -----------------------------
  # Downloads
  # -----------------------------
  output$download_pdr <- downloadHandler(
    filename = function() paste0("PDR_", input$dataset, ".tif"),
    content = function(file) {
      writeRaster(session$userData$PDR, file, overwrite = TRUE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("PDR_", input$dataset, ".png"),
    content = function(file) {
      png(file, width = 1200, height = 900)
      plot(session$userData$PDR,
           col = hcl.colors(50, "Viridis"),
           main = "PDR")
      dev.off()
    }
  )
}


# ==============================================================================
# Run App
# ==============================================================================
shinyApp(ui = ui, server = server)
