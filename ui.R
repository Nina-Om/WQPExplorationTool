library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(shinyBS)
library(datasets) 
library(shinyWidgets)
library(RCurl)
library(lubridate)
library(purrr)
library(magrittr)
library(httr)
library(geoshaper)
library(psych)
library(reshape2)
library(data.table)
library(maptools)  # Tools and functions for sp and other spatial objects e.g. spCbind
library(sf)
library(sp)        # Spatial pacakge with class definition
library(rgdal)     # GDAL wrapper for R, spatial utilities
library(tidyverse)
library(tidyr)
library(lattice)
library(plotly)
library(dplyr)
library(devtools)
library(scales)
library(Hmisc)
library(fst)
library(totalcensus)
library(dataRetrieval)
library(viridis)
library(DT)
library(rmarkdown)
library(stringr)
library(shinyalert)
library(htmltools)
library(cdlTools)
library(hrbrthemes)
#library(RLumShiny)
# List of all water quality parameters and activity media name available in Water Quality Portal Database; comes from here:
# activitymedia.name <- readRDS("activitymedia_name.rds")
# characteristics.name <- readRDS("characteristics_name.rds")
# activitymedia.name <- readRDS("activitymedia_name.rds")
# activitymedia.name2 <- c("",sort(unique(as.character(activitymedia.name))))
# 
# characteristics.name <- readRDS("characteristics_name.rds")
# characteristics.name2 <- c("", sort(unique(as.character(characteristics.name))))

Characteristictype <- c("Physical.2009-2019"="Physical", 
                        "Organics, Pesticide.1990-2019" ="pesticide",
                        "Inorganics, Major, Non-metals.1990-2019" ="Non_metals", 
                        "Inorganics, Major, Metals.1990-2019" = "Metals")

sampleMedia <- c("Water", "water")

Subdivision.selection <- c("", "Surface Water", "Groundwater")

wq_columns = read.csv("column-names.csv")

source("respin.R", local = TRUE)

#############################################################################################################
shinyUI(navbarPage("WQP Data Exploration Tool", 
            
                   theme = "bootstrap.css",
                   inverse = TRUE,
            #headerPanel("Water Quality Portal Data Exploration"),
        tabsetPanel(
            tabPanel("1. Query Data",
            sidebarLayout(
              sidebarPanel(width=3,
                           SpinBox(),
                           selectizeInput('Chartype', 'Characteristic Group', 
                                          c("Physical.2009-2019"="Physical", 
                                            "Organics, Pesticide.1990-2019" ="Organics, Pesticide",
                                            "Inorganics, Major, Non-metals.1990-2019" ="Inorganics, Major, Non-metals", 
                                            "Inorganics, Major, Metals.1990-2019" = "Inorganics, Major, Metals"), 
                                          selected= "", 
                                          multiple = F),
                           #selectizeInput('subdiv', 'Water Type', Subdivision.selection, multiple = TRUE),
                           
                           uiOutput("stsel"),
                           br(),
                           actionButton("goButton0","Filter Data!"),
                           br(),
                           br(),
                           uiOutput("charsel"),
                           br(),
                           uiOutput("loadsel"),
                          
                            conditionalPanel(
                              SpinBox(),
                              condition = "input.load == 'Local Server'",
                               uiOutput("yearsel"),
                               #uiOutput("subdivisionsel"),
                               #uiOutput("unitsel"),
                              br()
                            ),
                           
                           conditionalPanel(
                             SpinBox(),
                             condition = "input.load == 'Water Quality Portal'",
                             dateInput('date1', label = "Date from:", format="mm-dd-yyyy"),
                             dateInput('date2', label = 'to:', format="mm-dd-yyyy"),
                             #  selectizeInput('medianame2', 'Water Type', activitymedia.name2, 
                             #                 selected= "Water", multiple = F),
                             # selectizeInput('charname1', 'A.I. Name', characteristics.name2, selected= "", multiple = F),
                             #actionButton("goButton1", "Load Sites/Sample Results"),
                             br(),

                             actionButton("goButton","Retrieve Data!")
                           ),
                           hr(),
                           verbatimTextOutput("print1"),
                           actionButton("goButton2","Load Data/Display Sites!"),
                           br(),
                           br(),
                           downloadButton("downloadData", "Save Data!"),
                          br()
              

                          ), #sidebarpanel
              mainPanel(
                conditionalPanel(
                  SpinBox(),
                  condition = "input.load == 'Water Quality Portal'",
                fluidRow(h4("Data retrieval Summary", style  = "text-align:center")),
                verbatimTextOutput("siteinfo"),
                fluidRow(br()),
                fluidRow(h4("WQP Web Service Query URL", style  = "text-align:center")),
                verbatimTextOutput("URL")
                ),#conditionalPanel
                
                leafletOutput('mymap1'),
                br(),
                br(),
                actionButton("goButton4","Clear map!"),
                hr(),
                br(),
                br(),
                br(),
                DT::dataTableOutput("site_table")
              )#mainPanel

              )#sidebarLayout
            ), #tabPanel1
              #   tabsetPanel(
#################################################################################################################################
            navbarMenu("2. Data Tables", icon = icon("table"),
                       tabPanel("All Data",
                                br(),
                                br(),
                                DT::dataTableOutput("all_data_table")
                       ),

                     tabPanel("Missing",
                              br(),
                              br(),
                              DT::dataTableOutput("missing_table")
                     ),
                     tabPanel("Duplicates",
                              br(),
                              br(),
                              DT::dataTableOutput("dup_table")
                     )
            ), #tabsetPanel
#################################################################################################################################
            navbarMenu("3. Check Data and Summary", icon = icon("table"),
                       
                     tabPanel("Undetections",
                              br(),
                              wellPanel(fluidRow(column(1),
                                                 column(7,
                                                        fluidRow(h3("Please select a method for calculation of non-detections:", style = "text-align: center")),
                                                        fluidRow(radioButtons("ND_method","",
                                                                              c("Remove Non-Detections from data"=1,
                                                                                "Set Non-Detections equal to zero"=2,
                                                                                "Set Non-Detections equal to the Limit of Detection"=3,
                                                                                "Set Non-Detections equal to the 1/2 times the Limit of Detection"=4)))))
                              ),
                              br(),
                              br(),
                              downloadButton("downloadData2", "Save Cleaned Data!"),
                              #actionButton("goButton3","Apply Method!"),
                              br(),
                              br(),
                              hr(),
                              DT::dataTableOutput("undetections_table"),
                              br()
                     ),
                     tabPanel("Summary",
                              br(),
                              br(),
                              uiOutput("group.sel1"),
                              uiOutput("group.sel2"),
                              hr(),
                              
                              DT::dataTableOutput("summary_table"),
                              br(),
                              downloadButton("downloadData3", "Save Data Summary!"),
                              hr(),
                              actionButton("goButton11","Click for Boxplots!"),
                              br(), 
                              plotOutput("boxplot", width="50%"),
                              hr(),
                              DT::dataTableOutput("summary_table2", width = "50%"))
            ),
 ##################################################################################       
                  tabPanel("4. Search Data", icon = icon("flask"),
                           sidebarLayout(
                             sidebarPanel(width=3,
                                          SpinBox(),
                                    br(),
                        
                                    h4("1. Please select water characteristic"),
                           uiOutput("charmap_sel"),
                           hr(),
                           h4("2. Please define percentile of data (optional)"),
                           numericInput(inputId="pr", label="Samples Percentile (default: median):", value= 0.50, 0, 1, 0.01)
                           ),
                           mainPanel(
                           br(),
                           br(),
                           h3("3. Please select at least two sites on map"),
                           br(),
                           leafletOutput('mymap'),
                           #actionButton("act1", "Refresh map!"),
                           br(),
                           h4("Note: Please unselect all of the selected sites before selecting new characteristics or new set of stations."),
                             hr(),
                             actionButton("Plot", "Plot Percentiles Frequency!"),
                             br(),
                             br(),
                           plotOutput("plot_map1", width = "50%", height="400px", hover = TRUE),
                           fixedRow(
                             br(),
                             h3("4. Group data by selected features:"),
                             br(),
                             column(4, 
                             uiOutput("featureInput1")),
                             br(),
                             br(),
                             br(),
                           plotlyOutput("plot_map2"),
                           br(),
                           br(),
                           column(4, 
                                  uiOutput("featureInput2"))),
                           br(),
                           br(),
                           plotlyOutput("plot_map3"),
                           br(),
                           hr(),
                           h3("5. Please set maximum value of water quality parameter (optional)"),
                           uiOutput("ui"),
                           br(),
                           br()
                           )#main panel
                           )#sidebar
                           ),#tab panel
################################################################################## 
                           tabPanel("5. Filter Data", icon = icon("flask"),
                                    sidebarLayout(
                                      sidebarPanel(width=3,
                                                   SpinBox(),
                                                   uiOutput("provider.sel"),                 
                                                   uiOutput("orgname.sel"),
                                                   uiOutput("station.sel"),
                                                   uiOutput("subdiv.sel"),
                                                   uiOutput("unicode.sel"),
                                                   uiOutput("fraction.sel"),
                                                   uiOutput("method.sel")
                                                   ),
                             mainPanel(
                               br(),
                               h4("Please select the sites in 'Search Data' Panel to activate Filtering options."),
                               hr(),
                               br(),
                               br(),
                               
                               DT::dataTableOutput("final_filtered_data")
                             )#mainpanel
                           )),#tab panel
###################################################################################                 
navbarMenu("6. SSD Tool", icon = icon("bar-chart-o"),
  tabPanel("SSD Tool", icon = icon("bar-chart-o"),
           sidebarLayout(
             sidebarPanel(width=3,
                          SpinBox(),
                          h4("1. Please upload input data files:"),
                          br(),
                          fileInput("file1", label="Toxicity Data"),
                          fileInput("file2", label="Exposure Data (must have column 'ResultMeasureValue'"),
                          hr(),
                          h4("Example input data files:"),
                          br(),
                          downloadButton("downTemplate","Download Toxicity Data Template"),
                          br(),
                          br(),
                          downloadButton("downTemplate2","Download Exposure Data Template")
             ),
             mainPanel(
               
# navbarMenu("6. SSD Tool", icon = icon("bar-chart-o"),
#         tabPanel("SSD Plot",
#                            SpinBox(),
#                            fluidRow(
#                              h4("1. Please upload input data files:"),
#                              br(),
#                              fileInput("file1", label="Toxicity Data"),
#                              fileInput("file2", label="Exposure Data"),
#                              hr(),
#                              h4("Example input data files:"),
#                              br(),
#                              downloadButton("downTemplate","Download Toxicity Data Template"),
#                              br(),
#                              br(),
#                              downloadButton("downTemplate2","Download Exposure Data Template")
#                            ),
                 
                           hr(),
                           fluidRow(
                             SpinBox(),
                             h4("2. Specify Options for SSD"),
                             br(),
                             column(3,
                                    fluidRow(
                                      uiOutput("unitselssd"),
                                      bsTooltip("unitselssd", "Units of measure for toxicity data", placement = "top", 
                                                trigger = "hover", options = NULL),
                                      uiOutput("plotsel"),
                                      bsTooltip("plotsel", "Only Hazen plotting position is active", 
                                                placement = "top", trigger = "hover", options = NULL),
                                      
                                      radioButtons("scale", "Data Scale", 
                                                   list("Arthmetic"=1, "Logarithmic"=2), selected = 1),
                                      uiOutput("samplsizesel")
                                    )),
                             column(7,
                                    textInput("chem", ("Chemical Name (optional, used in report body."), value = ""), 
                                    shinyBS::bsTooltip("chem", "Active ingredient name (optional)", placement = "top", 
                                              trigger = "hover", options = NULL),
                                    textInput("exposure", "Exposure Concentration", value ="0.0"),
                                    bsTooltip("exposure", "Is required to calculate Fraction Affected (FA), unit of EC value must be identical to the unit of the toxicity values. (optional)", 
                                              placement = "top", trigger = "hover", options = NULL),
                                    numericInput("conf", "Desired Confidence %", value = 95)),
                             bsTooltip("conf", "Confidence interval for median by bootstrap", 
                                       placement = "top", trigger = "hover", options = NULL)
                           ),
                           
                           fluidRow(
                             SpinBox(),
                             hr(),
                             h4("3. Species Sensitivity Distributions"),
                             br(),
                             actionButton("dist", "Calculate"),
                             bsTooltip("dist", "The program calculates various distributions through toxicity values for the species", 
                                       placement = "top", trigger = "hover", options = NULL),
                             br(),
                             br(),
                             DT::dataTableOutput("FitTable"),
                             br(),
                             hr(),
                             h4("4. Outputs"),
                             br(),
                             actionButton("act2", "Distribution Parameters"),
                             bsTooltip("act2", "Table of distributions parameters", placement = "top", trigger = "hover",
                                       options = NULL),
                             br(),
                             br(),
                             DT::dataTableOutput("fitpars.table"),
                             br(),
                             actionButton("act3","Goodness of Fit Tests"),
                             bsTooltip("act3", "Goodness-of-fit tests to decide whether your data comes from the specific population distribution", placement = "top", trigger = "hover",
                                       options = NULL),
                             br(),
                             br(),
                             DT::dataTableOutput("gofPars"),
                             br(),
                             actionButton("act4","SSE and MSE"),
                             bsTooltip("act4", "Sum of Squared Errors and Mean Squared Error to determine the distribution with minimum deviation", placement = "top", trigger = "hover",
                                       options = NULL),
                             br(),
                             br(),
                             DT::dataTableOutput("sse.mse"),
                             br(),
                             actionButton("act5","HC5 and HC50"),
                             bsTooltip("act5", "Hazardous concentration affecting 5% and 50% of the species", placement = "top", trigger = "hover",
                                       options = NULL),
                             br(),
                             br(),
                             DT::dataTableOutput("hcTabl")
                           ),
                           
                           fluidRow(
                             hr(),
                             h4("4. Plot Species Sensitivity Distribution"),
                             br(),
                             uiOutput("modelsel"),
                             column(9,jqui_resizable(plotOutput("plot"))),
                             column(3, uiOutput("taxasel"))
                           )
             )
                 )),
tabPanel("Table and Summary",
                 fluidRow(
                   h3("Summary of Toxicity Data"),
                   column(6, jqui_resizable(verbatimTextOutput("summary.tox"))),
                   br(),
                   br(),
                   hr(),
                   DT::dataTableOutput("SSDtable", width = "80%")
                 )),
tabPanel("Help",
         h4("Input exposure Data"),
         "Exposure data file must have column 'ResultMeasureValue' for concentrations. 
            All other columns in the input data file are redundant.
            Example input data file includes 'Prometon' concentrations from WQP website."),
tabPanel("7. Report", icon=icon("pencil",lib="glyphicon"),
         SpinBox(),
         radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                      inline = TRUE),
         downloadButton("report", "Generate report")
)
        )


        )# tabspanel
 )#navpage
  )
