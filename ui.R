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

ui <- dashboardPage(skin = c("blue"),
                    
                    # Application title
                    dashboardHeader(titleWidth = 300,
                                    title = "Trevally"),
                    
                    # Side bar panel
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                       menuItem("Sampling", tabName = "sampling", icon = icon("fish")),
                                       menuItem("Population genetics (New Zealand)", tabName = "pop_gen_nz", icon = icon("dna")),
                                       menuItem("Population genetics (Australasia)", tabName = "pop_gen_nz_aus", icon = icon("dna")),
                                       menuItem("Taxonomy", tabName = "taxphylo", icon = icon("tree")),
                                       menuItem("Data Explorer", tabName = "data_explorer", icon = icon("database")),
                                       menuItem("Information", tabName = "info", icon = icon("chart-bar"))
                                     )),
                    # Main body of app
                    dashboardBody(
                      
                      tabItems(
                        
                        ##### PAGE 1 - SAMPLING #####
                        tabItem(tabName = "sampling",
                                fluidRow(width = 12,
                                         box(
                                           title = "Trevally sampling",
                                           width = 6,
                                           height = 200,
                                           status = "warning",
                                           helpText(tags$i(
                                             "Check out the Trevally sampling in Australia and New Zealand. Filter the data by the season the fish was caught and compare to juvenile, spawning or seasonal distributions of Trevally in New Zealand.")
                                           ), 
                                           helpText(tags$i(
                                           "Note: the size of the circles reflect uncertainty in the sampling location.")
                                           )
                                         ),
                                         
                                         box(width = 6,
                                             height = 200,
                                             status = "warning",
                                             selectInput("season_choice",
                                                         h4("Sampling season"),
                                                         choices = list("All Seasons",
                                                                        "Spring",
                                                                        "Summer",
                                                                        "Autumn",
                                                                        "Winter"),
                                                         selected = "All Seasons"),
                                             
                                             selectInput("biology_layers",
                                                         h4("Trevally distribution (New Zealand only)"),
                                                         choices = list("Annual Juvenile Distribution",
                                                                        "Annual Spawning Distribution",
                                                                        "Annual Distribution",
                                                                        "Spring Distribution",
                                                                        "Summer Distribution",
                                                                        "Autumn Distribution",
                                                                        "Winter Distribution"),

                                                         selected = "Annual Juvenile Distribution")
                                         )
                                ),
                                
                                fluidRow(width = 12,
                                         box(width = 12,
                                             height = 650,
                                             status = "warning",
                                             solidHeader = TRUE,
                                             leafletOutput("map_sampling", height = 630) %>% 
                                               withSpinner(color="#0dc5c1")
                                         )
                                )
                        ),
                        
                        ##### PAGE 2 - POPULATION GENETICS - NEW ZEALAND #####
                        tabItem(tabName = "pop_gen_nz",
                                
                                fluidRow(width = 12,
                                         
                                         box(
                                           title = "Population Genetics of Trevally - New Zealand (Control region)",
                                           width = 6,
                                           height = 150,
                                           status = "success",
                                           helpText(tags$i(
                                             "Check out the haplotype network of Trevally sampled in New Zealand. View at the scale of Statistical Area, Fisheries Management Area (FMA) or Quota Management Area (QMA).")
                                           )
                                         ),
                                         
                                         box(
                                           width = 6,
                                           height = 150,
                                           status = "success",
                                           selectInput("management_layers",
                                                       h4("Fisheries Management Layers"),
                                                       choices = list("Quota Management Areas",
                                                                      "Fisheries Management Areas",
                                                                      "Statistical Areas"),
                                                       selected = "Quota Management Areas")
                                         )
                                ),
                                
                                fluidRow(width = 12,
                                         
                                         box(width = 6,
                                             height = 700,
                                             status = "success",
                                             solidHeader = TRUE,
                                             leafletOutput("map_pop_nz", height = 680) %>% 
                                               withSpinner(color="#0dc5c1")
                                         ),
                                         
                                         box(width = 6,
                                             height = 700,
                                             status = "success",
                                             solidHeader = TRUE,
                                             forceNetworkOutput("haplotypeNetworkNZ", height = 680) %>% 
                                               withSpinner(color="#0dc5c1")
                                         )
                                )
                        ),
                        
                        ##### PAGE 3 - POPULATION GENETICS - AUSTRALASIA #####  
                        tabItem(tabName = "pop_gen_nz_aus",
                                
                                fluidRow(width = 12,
                                         box(
                                           title = "Population Genetics of Trevally - New Zealand (Control region)",
                                           width = 6,
                                           height = 150,
                                           status = "info",
                                           helpText(tags$i(
                                             "Check out the haplotype network of Trevally sampled in New Zealand and Australia. Compare Trevally from Western Australia to three random subsamples of Trevally from New Zealand."))
                                         ),
                                         
                                         box(
                                           width = 6,
                                           height = 150,
                                           status = "info",
                                           selectInput("subsample",
                                                       h4("New Zealand subsample"),
                                                       choices = list("Subsample 1",
                                                                      "Subsample 2",
                                                                      "Subsample 3"),
                                                       selected = "Subsample 1")
                                         )
                                ),
                                
                                fluidRow(width = 12,
                                         box(width = 6,
                                             height = 700,
                                             status = "info",
                                             solidHeader = TRUE,
                                             leafletOutput("map_pop_aus", height = 680) %>% 
                                               withSpinner(color="#0dc5c1")
                                         ),
                                         
                                         box(width = 6,
                                             height = 700,
                                             status = "info",
                                             solidHeader = TRUE,
                                             forceNetworkOutput("haplotypeNetworkNZandAUS", height = 680) %>% 
                                               withSpinner(color="#0dc5c1")
                                         )
                                )
                        ),
                        
                        ##### PAGE 4 - TAXONOMY (PHYLOGENY) #####
                        tabItem(tabName = "taxphylo",
                                
                                fluidRow(width = 12,
                                         
                                         box(
                                           title = "Pseudocaranx Taxonomy (COI gene)",
                                           width = 6,
                                           height = 150,
                                           status = "primary",
                                           helpText(tags$i(
                                             "Let's zoom out a little and look at some of Trevally's relatives. How are the species in the Pseudocaranx genus related?")
                                           )
                                         ),
                                         
                                         box(width = 6,
                                             height = 150,
                                             status = "primary"
                                         )
                                ),
                                
                                fluidRow(width = 12,
                                         
                                         box(width = 6, 
                                             height = 700,
                                             status = "primary",
                                             solidHeader = TRUE,
                                             leafletOutput("map_tax", height = 680) %>% 
                                               withSpinner(color="#0dc5c1")
                                         ),
                                         
                                         tabBox(width = 6,
                                                height = 700,
                                                selected = "Phylogeny",
                                                tabPanel("Phylogeny", phylocanvasOutput("phylogeny", height = 620)),
                                                tabPanel("Heatmap", 
                                                         selectInput("heatmapView",
                                                                     h4("Measure"),
                                                                     choices = list("Percentage identity",
                                                                                    "Number of differences",
                                                                                    "Number of similarities"),
                                                                     selected = "Percentage identity"),
                                                         plotlyOutput("heatmap", height = 540) %>% 
                                                           withSpinner(color="#0dc5c1")
                                                         )
                                         )
                                )
                        ),
                        
                        ##### PAGE 5 - DATA EXPLORER #####
                        tabItem(tabName = "data_explorer",
                                
                                fluidRow(width = 12,
                                         box(
                                           title = "Data Explorer",
                                           width = 6,
                                           height = 150,
                                           status = "warning",
                                           helpText(tags$i(
                                             "Let's get into the nitty gritty and delve into the data!")
                                           )
                                         ),
                                         
                                         box(
                                           width = 6,
                                           height = 150,
                                           status = "warning",
                                           downloadLink('downloadData', 'Download table data')
                                         ) 
                                ),
                                
                                fluidRow(width = 12,
                                         box(width = 12,
                                             status = "warning",
                                             solidHeader = TRUE,
                                             dataTableOutput("table") %>% 
                                               withSpinner(color="#0dc5c1")
                                         )
                                )
                        ), 
                        
                        ##### PAGE 6 - INFORMATION #####
                        tabItem(tabName = "info",
                                
                                fluidRow(width = 12,
                                         box(
                                           title = "Information",
                                           width = 12,
                                           height = 150,
                                           status = "success",
                                           helpText(tags$i("This app is based on the results of a thesis submitted to Victoria Univeristy of Wellington in partial fulfilment of the requirement for the degree of Master of Science in Ecology and Biodiversity. Start by choosing to investigate either the population genetic structure of Trevally in New Zealand/Australasia or the taxonomy of the wider Pseudocaranx genus. Have fun!")
                                           )
                                         )
                                ),
                                
                                fluidRow(width = 12,
                                         box(width = 6,
                                             title = "Map layer data",
                                             status = "success",
                                             solidHeader = TRUE,
                                             uiOutput("qmaurl"),
                                             uiOutput("fmaurl"),
                                             uiOutput("staturl"),
                                             uiOutput("juvenileurl"),
                                             uiOutput("spawningurl"),
                                             uiOutput("annualurl"),
                                             uiOutput("springurl"),
                                             uiOutput("summerurl"),
                                             uiOutput("autumnurl"),
                                             uiOutput("winterurl")
                                             ),
                                         box(width = 6,
                                             title = "Other links",
                                             status = "success",
                                             solidHeader = TRUE,
                                             uiOutput("giturlthesis"),
                                             uiOutput("giturlapp")
                                             )
                                )
                        )
                        
                      )
                    )
)