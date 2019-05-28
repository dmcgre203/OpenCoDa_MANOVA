
library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Physical Activity CODA MANOVA"),
  
  shinyUI(navbarPage("OpenCoDa",
                     tabPanel("Read_Data",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                  
                                  # Input: Select a file ----
                                  fileInput("file1", "Choose CSV File",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  # Input: Checkbox if file has header ----
                                  checkboxInput("header", "Header", TRUE),
                                  
                                  # Input: Select separator ----
                                  radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("quote", "Quote",
                                               choices = c(None = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"'),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("deci", "Decimal separator",
                                               choices = c(Period = ".",
                                                           Comma =  ","),
                                               selected = '.'),
                                  
                                  # Input: Select number of rows to display ----
                                  radioButtons("disp", "Display",
                                               choices = c(Head = "head",
                                                           All = "all"),
                                               selected = "head"),
                                  
                                  # Input: Select response variables ----
                                  uiOutput("choose_response")
                                  
                                ),
                                
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  tags$h3(textOutput("rawData_Header")), 
                                  tableOutput("contents"),
                                  
                                  # Horizontal line ----
                                  tags$hr(),        
                                  
                                  tags$h3(textOutput("rawDataSummary_Header")), 
                                  tableOutput("rawSummary")
                                )
                                
                              )),

                     tabPanel("Categories",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Additional parameters
                                  uiOutput("choose_responseCats"),
                                  uiOutput("responseCats_numQuantiles"),
                                  uiOutput("responseCats_thresholds")
                                  
                                ),
                                
                                mainPanel(
                                  plotOutput("histogram1")
                                  
                                )
                              )),
                     
                     tabPanel("Compositional Data",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Compositional Variables
                                  uiOutput("choose_CODA"),
                                  
                                  # Input: Variation Matrix - which group
                                  uiOutput("choose_group_VarMat")
                                
                                ),
                                
                                mainPanel(
                                  # Group plots
                                  tags$h3(textOutput("groupPlots_Title")), 
                                  plotOutput("groupPlots"),
                                  
                                  # Geometric averages (by group and overall)
                                  tags$h3(textOutput("avgCODA_Title")), 
                                  tableOutput("avgCODA"),

                                  # Arithmetic averages (by group and overall)
                                  tags$h3(textOutput("avgStandard_Title")), 
                                  tableOutput("avgStandard"),
                                  
                                  # Variation matrix
                                  tags$h3(textOutput("VariationMatrix_Title")), 
                                  tableOutput("VariationMatrix")                                
                                  
                                )
                              )
                     ),
                         
                     tabPanel("Ternary Plot",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Choose ternary variables
                                  uiOutput("choose_ternVar"),
                                  uiOutput("choose_ternGroups")
                                ),
                                
                                mainPanel(
                                  
                                  plotOutput("ternPlot")
                                  
                                )
                              )),
                     
                                      
                     tabPanel("MANOVA",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Additional parameters
                                  uiOutput("choose_covariates"),
                                  uiOutput("choose_cofactors"),
                                  uiOutput("choose_ilr")
                                  
                                ),
                                
                                mainPanel(
                                  verbatimTextOutput("MANOVASummary"),
                                  verbatimTextOutput("MANOVASummaryAOV")
                                  
                                )
                              ))
                     

                     

                     
                     
                     
  )
  )
)
