# Load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shinydashboard)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(phylocanvas)
library(plotly)
library(networkD3)
library(stringr)
library(xlsx)
library(shinycssloaders)

# Import sampling data (first tab of excel spreadsheet) into R
sampling_data <- read.csv("Trevally.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data$location.latitude <- as.double(sampling_data$location.latitude)
sampling_data$location.longitude <- as.double(sampling_data$location.longitude)

# Control sequenced data (including data filtered by sampling season)
sampling_data_control <- read.csv("sampling_data_control.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_control_winter <- read.csv("sampling_data_control_winter.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_control_spring <- read.csv("sampling_data_control_spring.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_control_summer <- read.csv("sampling_data_control_summer.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_control_autumn <- read.csv("sampling_data_control_autumn.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")

# NZ subsampled data
sampling_data_aus <- read.csv("sampling_data_aus.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_nz_1 <- read.csv("sampling_data_nz_1.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_nz_2 <- read.csv("sampling_data_nz_2.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_nz_3 <- read.csv("sampling_data_nz_3.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")

# COI sequenced data
sampling_data_coi <- read.csv("sampling_data_coi.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_coi_georgianus <- read.csv("sampling_data_coi_georgianus.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_coi_wrighti <- read.csv("sampling_data_coi_wrighti.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")
sampling_data_coi_dentex <- read.csv("sampling_data_coi_dentex.csv", stringsAsFactors = FALSE, fileEncoding = "cp1252")

# Import management layer files (and rename columns for later dataset joins)
qma <- shapefile("QMAs/Trevally.shp") %>% 
  st_as_sf() %>%
  dplyr::select("LayerName", "FishstockC", "Shape_STAr", "Shape_STLe", "geometry") %>%
  plyr::rename(c("FishstockC" = "location.qma"))

qma_transformed <- qma %>% as_Spatial() %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +lon_wrap=180"))

fma <- shapefile("FMAs/FisheriesManagementAreas.shp") %>% 
  st_as_sf() %>%
  dplyr::filter(LayerName == "General FMAs") %>%  
  dplyr::select("LayerName", "FmaId", "Annotation", "SHAPE_STAr", "SHAPE_STLe", "geometry") %>%
  plyr::rename(c("FmaId" = "location.fma"))

fma_transformed <- fma %>% as_Spatial() %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +lon_wrap=180"))

statarea <- shapefile("StatisticalAreas/StatisticalAreas.shp") %>% 
  st_as_sf() %>%
  dplyr::filter(LayerName == "General Statistical Areas") %>% 
  dplyr::select("LayerName", "Statistica", "SHAPE_STAr", "SHAPE_STLe", "geometry") %>%
  plyr::rename(c("Statistica" = "location.stat.area"))

statarea_transformed <- statarea %>% as_Spatial() %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +lon_wrap=180"))

# Import biology data
distribution <- shapefile("FinfishDistributions/FinfishDistributions.shp") %>%
  st_as_sf()
springDist <- dplyr::filter(distribution, LayerName == "Trevally - Spring Distribution")
summerDist <- dplyr::filter(distribution, LayerName == "Trevally - Summer Distribution")
autumnDist <- dplyr::filter(distribution, LayerName == "Trevally - Autumn Distribution")
winterDist <- dplyr::filter(distribution, LayerName == "Trevally - Winter Distribution")
annualDist <- dplyr::filter(distribution, LayerName == "Trevally - Annual Distribution")
spawningDistribution <- shapefile("FinfishSpawningDistributions/FinfishSpawningDistributions.shp") %>%
  st_as_sf()
annualSpawningDist <- dplyr::filter(spawningDistribution, LayerName == "Trevally - Annual Spawning Distribution")
annualJuvDist <- dplyr::filter(spawningDistribution, LayerName == "Trevally - Annual Juvenile Distribution")

# Population number and sample number for each qma, fma, and stat area for which the control region was sequenced (for population genetics)
manage_group <- read.csv("manage_group.csv", fileEncoding = "cp1252")
manage_group$location.stat.area <- str_pad(manage_group$location.stat.area, 3, pad = "0")
manage_group$location.stat.area <- as.character(manage_group$location.stat.area)
manage_group$location.fma <- as.character(manage_group$location.fma)
manage_group$location.qma <- as.character(manage_group$location.qma)

qma_pop_n <- manage_group %>% 
  dplyr::group_by(location.qma) %>% 
  count() %>% 
  dplyr::right_join(qma, by = "location.qma")
qma_pop_n$sampled <- !is.na(qma_pop_n$n)

qma_sample_n <- sampling_data %>%  
  dplyr::filter(control.successfully.sequenced=="Y") %>%                                                                          
  dplyr::group_by(location.qma) %>%
  count() %>%
  dplyr::right_join(qma, by = "location.qma")
qma_sample_n$sampled <- !is.na(qma_sample_n$n)

fma_pop_n <- manage_group %>% 
  dplyr::group_by(location.fma) %>% 
  count() %>% 
  dplyr::right_join(fma, by = "location.fma")
fma_pop_n$sampled <- !is.na(fma_pop_n$n)

fma_sample_n <- sampling_data %>%  
  dplyr::filter(control.successfully.sequenced=="Y") %>% 
  dplyr::group_by(location.fma) %>%
  count() %>%
  dplyr::right_join(fma, by = "location.fma")
fma_sample_n$sampled <- !is.na(fma_sample_n$n)

stat_pop_n <- manage_group %>% 
  dplyr::group_by(location.stat.area) %>%
  count() %>% 
  dplyr::right_join(statarea, by = "location.stat.area")
stat_pop_n$sampled <- !is.na(stat_pop_n$n)

stat_sample_n <- sampling_data %>%  
  dplyr::filter(control.successfully.sequenced=="Y") %>% 
  dplyr::group_by(location.stat.area) %>%
  count() %>%
  dplyr::right_join(statarea, by = "location.stat.area")
stat_sample_n$sampled <- !is.na(stat_sample_n$n)

# Palettes
palqma <- colorFactor(palette = c("#1864BA", "#5B83A1", "#F98D00"), levels = c("TRE1", "TRE2", "TRE7"))

palfma <- colorFactor(palette = c("#1864BA", "#5B83A1", "#F98D00", "#FCBB66"), levels = c("FMA1", "FMA2", "FMA8", "FMA9"))

palstatarea <- colorFactor(palette = c("#1864BA", "#5B83A1", "#F98D00", "#FCBB66", "#42B202", "#BBFF7F", "#E20000", "#FFB2B2", "#9255FA", "#D0C1D7", "#664040", "#BCA2A2", "#FA8BB8", "#FFDFEC"), levels = c("002", "003", "008", "009", "010", "011", "013", "016", "039", "040", "041", "042", "046", "047"))

# Haplotype network data
NZnodeList <- read.xlsx("NZnodeList.xlsx", sheetName = "nodeList")
NZedgeList <- read.csv("NZedgeList.csv", fileEncoding = "cp1252")
NZandAUSnodeList1 <- read.csv("NZandAUSnodeList1.csv", fileEncoding = "cp1252")
NZandAUSedgeList1 <- read.csv("NZandAUSedgeList1.csv", fileEncoding = "cp1252")
NZandAUSnodeList2 <- read.csv("NZandAUSnodeList2.csv", fileEncoding = "cp1252")
NZandAUSedgeList2 <- read.csv("NZandAUSedgeList2.csv", fileEncoding = "cp1252")
NZandAUSnodeList3 <- read.csv("NZandAUSnodeList3.csv", fileEncoding = "cp1252")
NZandAUSedgeList3 <- read.csv("NZandAUSedgeList3.csv", fileEncoding = "cp1252")

# Phylogney data
tree <- load.tree("tree.newick")

# Colour P. wrighti in phylogney
node  <- phylobase::MRCA(tree,c("P.wrighti1", "P.wrighti8"))
commonancestor <- names(node)
descendantsW <- get.descendants(tree, commonancestor)

# Colour P. georgianus in phylogney
node  <- phylobase::MRCA(tree,c("P.georgianus15", "P.georgianus19"))
commonancestor <- names(node)
descendantsG <- get.descendants(tree, commonancestor)

# Colour P. dentex in phylogney
node  <- phylobase::MRCA(tree,c("P.dentex3", "P.dentex4"))
commonancestor <- names(node)
descendantsD <- get.descendants(tree, commonancestor)

# Heatmap data
percentIdentity <- read.csv("percentIdentity.csv", header = TRUE, row.names = 1, fileEncoding = "cp1252") %>% as.matrix()
numSims <- read.csv("numSims.csv", header = TRUE, row.names = 1, fileEncoding = "cp1252") %>% as.matrix()
numDiffs <- read.csv("numDiffs.csv", header = TRUE, row.names = 1, fileEncoding = "cp1252") %>% as.matrix()

# Table data
table_data <- read.xlsx("table_data.xlsx", sheetName = "Sheet 1")



server <- function(input, output, session) {
  
   # Provide labels for qma, fma and stat area polygons from which samples have been collected
  view_label <- reactive({
    switch(input$management_layers,
           "Quota Management Areas" = "",
           "Fisheries Management Areas" = "",
           "Statistical Areas" = "Stat area:")
  }) 
  
  sampled <- reactive({
    switch(input$management_layers,
           "Quota Management Areas" = qma_sample_n$location.qma,
           "Fisheries Management Areas" = fma_sample_n$location.fma,
           "Statistical Areas" = stat_sample_n$location.stat.area)
  })
  
  # Plot number of populations per qma, fma or stat area
  npops <- reactive({
    switch(input$management_layers,
           "Quota Management Areas" = qma_pop_n$n,
           "Fisheries Management Areas" = fma_pop_n$n,
           "Statistical Areas" = stat_pop_n$n)
  })
  
  # Plot number of samples per qma, fma or stat area
  nsamples <- reactive({
    switch(input$management_layers,
           "Quota Management Areas" = qma_sample_n$n,
           "Fisheries Management Areas" = fma_sample_n$n,
           "Statistical Areas" = stat_sample_n$n)
  })
  
  # Add map layers depending on the user choice of "Management layers" 
  manage_layer_input <- reactive ({
    switch(input$management_layers,
           "Quota Management Areas" = qma_transformed,
           "Fisheries Management Areas" = fma_transformed,
           "Statistical Areas" = statarea_transformed)
  })
  
  # Add map layer labels depending on user choice of "Management layers" 
  manage_layer_labels <- reactive ({
    switch(input$management_layers,
           "Quota Management Areas" = qma$location.qma,
           "Fisheries Management Areas" = fma$location.fma,
           "Statistical Areas" = statarea$location.stat.area)
  })
  
  # Add haplotype network grouping data (New Zealand) depending on user choice of "Management layers" 
  group <- reactive ({
    switch(input$management_layers,
           "Quota Management Areas" = "qmaGroup",
           "Fisheries Management Areas" = "fmaGroup",
           "Statistical Areas" = "statGroup")
  })
  
  # Add haplotype network naming data (New Zealand) depending on user choice of "Management layers" 
  name <- reactive ({
    switch(input$management_layers,
           "Quota Management Areas" = "qmaName",
           "Fisheries Management Areas" = "fmaName",
           "Statistical Areas" = "statName")
  })
  
  # Pick colouring system depending on user choice of "Management layers" 
  pal <- reactive ({
    switch(input$management_layers,
           "Quota Management Areas" = palqma,
           "Fisheries Management Areas" = palfma,
           "Statistical Areas" = palstatarea)
  })
  
  # Display circles on map for user chosen "Subsample"
  NZsubsampleForMap  <- reactive ({
    switch(input$subsample, 
           "Subsample 1" = sampling_data_nz_1,
           "Subsample 2" = sampling_data_nz_2,
           "Subsample 3" = sampling_data_nz_3)
  })
  
  # Disply haplotype network (Australasia) node data depending on the users choice of "Subsample"
  NZandAUSnodeList  <- reactive ({
    switch(input$subsample, 
           "Subsample 1" = NZandAUSnodeList1,
           "Subsample 2" = NZandAUSnodeList2,
           "Subsample 3" = NZandAUSnodeList3)
  })
  
  # Disply haplotype network (Australasia) edge data depending on the users choice of "Subsample"
  NZandAUSedgeList  <- reactive ({
    switch(input$subsample, 
           "Subsample 1" = NZandAUSedgeList1,
           "Subsample 2" = NZandAUSedgeList2,
           "Subsample 3" = NZandAUSedgeList3)
  })
  
  # Add biology map layers based on user choice of "Biology layers"
  biology_layer_input <- reactive ({
    switch(input$biology_layers,
           "None" = NA,
           "Annual Distribution" = annualDist,
           "Spring Distribution" = springDist,
           "Summer Distribution" = summerDist,
           "Autumn Distribution" = autumnDist,
           "Winter Distribution" = winterDist,
           "Annual Juvenile Distribution" = annualJuvDist,
           "Annual Spawning Distribution" = annualSpawningDist)
  })
  
  # Filter sampling data by season
  season <- reactive ({
    switch(input$season_choice,
           "All Seasons" = sampling_data_control,
           "Winter" = sampling_data_control_winter,
           "Spring" = sampling_data_control_spring,
           "Summer" = sampling_data_control_summer,
           "Autumn" = sampling_data_control_autumn)
  })
  
  # Choose heatmap dataset based on user choice of "Percentage identity", "Number of similarities" or "Number of differences"
  heatmap_view <- reactive ({
    switch(input$heatmapView,
           "Percentage identity" = percentIdentity,
           "Number of similarities" = numSims,
           "Number of differences" = numDiffs)
  })
  
  # Produce sampling map
  output$map_sampling <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      addScaleBar() %>%
      setView(148.651864, -34.123610, zoom = 4) %>%
      addDrawToolbar(
        targetGroup='Draw',
        position = "topright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addPolygons(data = biology_layer_input(), weight = 0.5, fillOpacity = 0.2, fillColor = "green") %>%
      addCircles(data = season(),
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = ~radius*1300,
                 color = c("#1864BA"),
                 opacity = 0.6,
                 fillOpacity = 0.3) %>%
      addProviderTiles(providers$Esri.WorldPhysical) 
  }) 
  
  # Produce population genetics map (New Zealand view)
  output$map_pop_nz <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, worldCopyJump = TRUE)) %>%
      addScaleBar() %>%
      setView(173.246277, -39.509186, zoom = 4) %>%
      addDrawToolbar(
        targetGroup = "Draw",
        position = "topright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addPolygons(data = manage_layer_input(), 
                  weight = 0.5, 
                  fillColor = ~pal()(sampled()),
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(color = "white", weight = 2),
                  label = sprintf("%s %s, Locations sampled: %s, Sample number: %s", view_label(), manage_layer_labels(), npops(), nsamples()),
                  labelOptions = labelOptions(style = list(
                    "font-size" = "9px"))) %>%
      addProviderTiles(providers$Esri.WorldPhysical)
  })
  
  # Produce haplotype network (New Zealand) grouped by Statistical Area, FMA or QMA
  output$haplotypeNetworkNZ <- renderForceNetwork({
    forceNetwork(Links = NZedgeList,
                 Nodes = NZnodeList, 
                 Source = "SourceID",
                 Target = "TargetID",
                 Value = "Weight", 
                 NodeID = name(), 
                 Nodesize = 5,
                 Group = group(),
                 height = NULL,
                 width = NULL,
                 fontSize = 12, 
                 linkDistance = 20,
                 linkWidth = 1,
                 linkColour = "black",
                 opacity = 1,
                 charge = -7)
  })  
  
  # Produce population genetics map (Australasia)
  output$map_pop_aus <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      addScaleBar() %>%
      setView(148.651864, -34.123610, zoom = 3) %>%
      addDrawToolbar(
        targetGroup = "Draw",
        position = "topright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addCircles(data = sampling_data_aus,
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = 20000,
                 color = c("#5B83A1"),
                 opacity = 1,
                 fillOpacity = 0.6) %>%
      addCircles(data = NZsubsampleForMap(),
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = 20000,
                 color = c("#1864BA"),
                 opacity = 1,
                 fillOpacity = 0.6) %>%
      addLegend(position = "bottomleft",
                colors = c("#5B83A1",  "#1864BA"),
                labels = c("Australia", "New Zealand"),
                opacity = 0.8) %>%
      addProviderTiles(providers$Esri.WorldPhysical)
  })
  
  # Produce haplotype network (Australasia) grouped by country (Australia and NZ)
  output$haplotypeNetworkNZandAUS <- renderForceNetwork({
    forceNetwork(Links = NZandAUSedgeList(),
                 Nodes = NZandAUSnodeList(), 
                 Source = "SourceID",
                 Target = "TargetID",
                 Value = "Weight", 
                 NodeID = "country", 
                 Nodesize = 5, 
                 Group = "group",
                 height = NULL,
                 width = NULL,
                 fontSize = 12, 
                 linkDistance = 20,
                 linkWidth = 1,
                 linkColour = "black",
                 opacity = 1,
                 charge = -15) 
  })  
  
  # Produce taxonomy map
  output$map_tax <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      addScaleBar() %>%
      setView(79.453738, -25.476532, zoom = 2) %>%
      addScaleBar() %>%
      addDrawToolbar(
        targetGroup = "Draw",
        position = "topright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addCircles(data = sampling_data_coi_georgianus,
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = 50000,
                 color = c("#1864BA"),
                 opacity = 1,
                 fillOpacity = 0.6) %>%
      addCircles(data = sampling_data_coi_wrighti,
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = 50000,
                 color = c("#30AC40"),
                 opacity = 1,
                 fillOpacity = 0.6) %>%
      addCircles(data = sampling_data_coi_dentex,
                 lat = ~location.latitude,
                 lng = ~location.longitude,
                 label = ~sprintf("Sample number: %s, Date caught: %s ", n, landing.date),
                 radius = 50000,
                 color = c("#FF8000"),
                 opacity = 1,
                 fillOpacity = 0.7) %>%
      addLegend(position = "bottomleft",
                colors = c("#1864BA",  "#30AC40", "#FF8000"),
                labels = c("P. georgianus", "P. wrighti", "P. dentex"),
                opacity = 0.8) %>%
      addProviderTiles(providers$Esri.WorldPhysical)
  })
  
  # Produce phylogeny (taxonomy)
  output$phylogeny <- renderPhylocanvas({
    canvas <- phylocanvas(tree, 
                treetype = "rectangular", 
                textsize = 16, 
                linewidth = 2, 
                showscalebar = TRUE, 
                alignlabels = TRUE, 
                nodesize = 10,
                width = "auto",
                height = "auto")
    
    canvas <- phylocanvas(tree, 
                          treetype = "rectangular", 
                          linewidth = 2,
                          textsize = 8,
                          showscalebar = TRUE, 
                          alignlabels = TRUE, 
                          nodesize = 10,
                          width = "auto",
                          height = "auto")
    for (nodename in descendantsW) {
      canvas <- style_node(canvas, nodeid = nodename, fillcolor = "#30AC40", labelfont = "Arial", labelformat = "regular")
    }
    for (nodename in descendantsG) {
      canvas <- style_node(canvas, nodeid = nodename, fillcolor = "#1864BA", labelfont = "Arial", labelformat = "regular")
    }
    for (nodename in descendantsD) {
      canvas <- style_node(canvas, nodeid = nodename, fillcolor = "#FF8000", labelfont = "Arial", labelformat = "regular")
    }
    canvas
  })
  
  # Produce heatmap (taxonomy) based on user input of % identity, number of differences or number of similarities
  output$heatmap <- renderPlotly({
    plot_ly(x = colnames(heatmap_view()), y = rownames(heatmap_view()), z = heatmap_view(), colors = "GnBu", type = "heatmap") %>%
      layout(
        xaxis = list(tickangle = 45))
  }) 
  
  # Create table of data based on user input and download current table user is viewing
  output$table <- renderDataTable(table_data, 
                                  options = list(pageLength = 10))
  
  # Create button that allows the user to download the table data
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(table_data, con)
       }
     )
  
  # Create URL links
  url1 <- a("Trevally QMAs", href = "https://mpi.maps.arcgis.com/home/item.html?id=709ad5499e314b799a14f33edebcb6a9")
  output$qmaurl <- renderUI({
    tagList(url1)
  })
  
  url2 <- a("General FMAs", href = "https://mpi.maps.arcgis.com/home/item.html?id=933abb45718f4a01834cd4d64b2a2272")
  output$fmaurl <- renderUI({
    tagList(url2)
  })
  
  url3 <- a("General Statistical Areas", href = "https://mpi.maps.arcgis.com/home/item.html?id=12f6e5ce811541eb947168560040586e")
  output$staturl <- renderUI({
    tagList(url3)
  })
  
  url4 <- a("Trevally - Spawning Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=898c193af45f4b19a9f86bf78c8b7307")
  output$spawningurl <- renderUI({
    tagList(url4)
  })
  
  url5 <- a("Trevally - Juvenile Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=199944742bc44e11bb2b2c2289c0d4cd")
  output$juvenilleurl <- renderUI({
    tagList(url5)
  })
  
  url6 <- a("Trevally - Annual Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=1c2958c4cf3b4f2d926befcae5e89c82")
  output$annualurl <- renderUI({
    tagList(url6)
  })  
  
  url7 <- a("Trevally - Winter Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=16b07f3a21db4696a2ab65eb3bc92ae1")
  output$winterurl <- renderUI({
    tagList(url7)
  })
  
  url8 <- a("Trevally - Spring Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=d449a3be359247ae8c9743cc0adc18b9")
  output$springurl <- renderUI({
    tagList(url8)
  })
  
  url9 <- a("Trevally - Autumn Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=3ab21d977dad43879fcc0a80dd3657b4")
  output$autumnurl <- renderUI({
    tagList(url9)
  })
  
  url10 <- a("Trevally - Summer Distribution", href = "https://mpi.maps.arcgis.com/home/item.html?id=3d243bce136f4208a71062783f8fc3e1")
  output$summerurl <- renderUI({
    tagList(url10)
  })
  
  url11 <- a("Github repository - thesis", href = "https://github.com/leahkemp/welly-trevally")
  output$giturlthesis <- renderUI({
    tagList(url11)
  })
  
  url12 <- a("Github repository - trevallyApp", href = "https://github.com/leahkemp/welly-trevally/tree/master/trevallyApp")
  output$giturlapp <- renderUI({
    tagList(url12)
  })
  
}
