#Author: Caleb Leonard
#Date: 11/23/2025
#ECE 5332 Statistics in Semiconductor Manufacturing Final Project
#Web Link: 
#https://caleb-leonard56.shinyapps.io/ECE5332StatisticsInSemiconductorManufacturingFinalProject/
#For a Usable CSV File, Go To: 
#https://github.com/Caleb-Leonard/Statistical-Methods-in-Semiconductor-Manufacturing/blob/main/Semiconductor_PCM_with_specs_and_targets.csv

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(bslib)
library(qcc)
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Process Control Dashboard for Semiconductor PCM
Data"),

    #Fluid Row to Explain Dashboard Purpose and Inputs/Outputs
    fluidRow(column(12, "This dashboard used is to take a formatted csv file, select a process capability measure, and output control charts along with some other statistical data.")),
    fluidRow(column(12, "The csv file format is as follows: 
    Row 1 is a header, 
    Rows 2-71 are 14 wafers w/ 5 test sites for 70 observations total,
    Rows 72, 73, 74 are upper spec limit (USL), lower spec limit (LSL), and target respectively,
    Columns 1-6 are index, lot#, wafer#, site#, date, time ")),
    fluidRow(column(12, "For a Usable CSV File, Go To: 
    https://github.com/Caleb-Leonard/Statistical-Methods-in-Semiconductor-Manufacturing/blob/main/Semiconductor_PCM_with_specs_and_targets.csv
    ")),
    
    #File Upload Input - Works
    #Expects a CSV file where 
    #Row 1 is Header 
    #Rows 2-71 Are 14 Wafers w/ 5 Test Sites For 70 Observations Total
    #Rows 72, 73, 74 Are Upper Spec Limit (USL), Lower Spec Limit (LSL), and Target Respectively
    #Columns 1-6 are Index, Lot#, Wafer#, Site#, Date, Time
    
    fluidRow(column(12,fileInput("file1", "Upload CSV File", accept = ".csv")
                    #verbatimTextOutput("file1_contents"),
                    #tableOutput("contents"))
    )), 
    
    fluidRow(column(12,verbatimTextOutput("file1_contents"))),
    
    fluidRow(column(12,tableOutput("contents"))),
    
    fluidRow(column(12,tableOutput("SelectedPCMMAtrix"))),
    
    #New way for selecting PCMs - Works
    #Server Component On Lines 68-72
    fluidRow(column(12, uiOutput("PCMSelector"))),
    
    #QCC X-Bar Chart Output - Works
    #Server Component On Lines 68-72
    fluidRow(column(12,plotOutput("XBarPlot"))),
    
    #QCC R Chart Output - Works
    #Server Component On Lines 68-72
    fluidRow(column(12,plotOutput("RPlot"))),
    
    #QCC Process Capability Chart Output - Works
    #Server Component On Lines 68-72
    fluidRow(column(12,plotOutput("ProcessCapabilityChart"))),
    
    #PCM LSL, Target, and USL Used for QCC Process Capability Measures - Works
    #Server Component On Lines 68-72
    fluidRow(column(1, " "),
             column(5, "The lower spec limit is: " , textOutput("LSL1")),
             column(4, "The target is: " , textOutput("Target1")),
             column(2, "The upper spec limit is: " , textOutput("USL1"))
             )
)

# Define server logic required for Inputs/Outputs
server <- function(input, output) {
  
  #Render Print to Show Name Size Type of File - Works
  output$file1_contents <- renderPrint({print(input$file1)})
  
  #Render Table to Show CSV Data - Doesn't Work
  output$contents <- renderTable({
    file <- input$file1
    req(file)
    
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
  }) # End output$contents
  
  #Reads CSV and Turns It Into Reactive Data - Works
  #UI Component on Line
  DataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE)
  })
  
  #New Way For Selecting PCMs - Works
  #UI Component on Line
  output$PCMSelector <- renderUI({
    req(DataInput())
    selectInput("PCM", "Select PCM Column", choices = names(DataInput()[,7:56]))
  })
  
  #CSV File PCM Selected Vector w/ Col Names, USL, LSL, Target - Works
  SelectedPCM <- reactive({
    req(DataInput())
    as.numeric(DataInput()[,input$PCM])
  })
    
  #Selected PCM Vector Upper Spec Limit - Works
  USL <- reactive({
    req(SelectedPCM())
    SelectedPCM()[71]
  })
  
  #Selected PCM Vector Lower Spec Limit - Works
  LSL <- reactive({
    req(SelectedPCM())
    SelectedPCM()[72]
  })
  
  #Selected PCM Vector Target - Works
  Target <- reactive({
    req(SelectedPCM())
    SelectedPCM()[73]
  })
  
  #CSV File PCM Selected Vector w/ No Col Name, USL, LSL, Target - Works
  #PCM Vector into 14 Row 5 Column Matrix
  SelectedPCMData <- reactive({
    req(SelectedPCM())
    PCMVector <- SelectedPCM()[-c( 71, 72, 73)]
    PCMVector <- PCMVector[!is.na(PCMVector)]
    t(matrix(PCMVector, nrow = 5, ncol = 14 ))
  })
  
  #Render Table to Output Selected PCM Data - Works
  #UI Component on Line
  output$SelectedPCMMAtrix <- renderTable({
    req(SelectedPCMData())
    SelectedPCMData()
  })
  
  #Render Plot to Output X-Bar Chart of Selected PCM Data - Works
  #UI Component on Line
  output$XBarPlot <- renderPlot({
    # Require that a file has been uploaded
    req(input$file1)
    
    # Require that a PCM has been selected
    req(input$PCM)
    
    req(SelectedPCMData)
    
    # Read the uploaded CSV file
    data <- read.csv(input$file1$datapath)
    
    # Create The X-Bar Chart
    qcc(SelectedPCMData(), type = "xbar", main = "X-bar Control Chart")
  }) # End output$XBarPlot
  
  #Render Plot to Output R Chart of Selected PCM Data - Works
  #UI Component on Line
  output$RPlot <- renderPlot({
    # Require that a file has been uploaded
    req(input$file1)
    
    # Require that a PCM has been selected
    req(input$PCM)
    
    req(SelectedPCMData)
    
    # Read the uploaded CSV file
    data <- read.csv(input$file1$datapath)
    
    # Create the control chart with the selected type
    qcc(SelectedPCMData(), type = "R", main = "R Control Chart")
  }) # End output$RPlot
  
  #Render Plot to Output Process Capability Chart of Selected PCM Data - Works
  #UI Component on Line
  output$ProcessCapabilityChart <- renderPlot({
    # Require that a file has been uploaded
    req(input$file1)
    
    # Require that a PCM has been selected
    req(input$PCM)
    
    req(SelectedPCMData)
    
    req(LSL, USL, Target)
    
    validate(
      need(!is.na(LSL()), "USL (row 72) is NA — please check your CSV."),
      need(!is.na(USL()), "LSL (row 73) is NA — please check your CSV."),
      need(!is.na(Target()), "TARGET (row 74) is NA — please check your CSV.")
    )
    
    # Read the uploaded CSV file
    data <- read.csv(input$file1$datapath)
    
    # Create The Process Capability Chart
    process.capability(
      qcc(SelectedPCMData(), 
          type = "xbar", 
          plot = FALSE, 
          chart.all = FALSE, 
          restore.par = TRUE), 
                       spec.limits = c(LSL(), USL()), 
                       target = Target(), print = TRUE)
  }) #End output$ProcessCapabilityChart
  
  
  #Render Text to Output LSL, USL, and Target
  #UI Component on Line
  output$LSL1 <-renderText({LSL()})
  output$USL1 <-renderText({USL()})
  output$Target1 <-renderText({Target()})
}

# Run the application 
shinyApp(ui = ui, server = server)
