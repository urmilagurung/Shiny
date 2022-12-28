library(shiny)
library(shinythemes)

####################################
# UI                               #
####################################

  # Define UI
  ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage(
      "Application",
      tabPanel("Name",
               sidebarPanel(
                 tags$h3("Input:"),
                 textInput("txt1", "First Name:", "Castle"),
                 textInput("txt2", "Last Name:", "Guras"),
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Output :"),
                            
                            h3("Full Name :"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Age",
               sidebarPanel(
                 tags$h3("Input:"),
                 dateInput("date1", "Date of Birth:", value = as.Date("27/12/2000", format = "%d/%m/%Y")),
               ), # sidebarPanel
               mainPanel(
                 h1("Output :"),
                 
                 h3("Current Age :"),
                 verbatimTextOutput ("age"),
                 
               ) # mainPanel
      ), # Navbar 2, tabpanel
      tabPanel("File Read",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput("file","Choose a csv file", accept = ".csv"),
                 checkboxInput("header", "Header", TRUE)
                 ), # sidebarPanel
               mainPanel(
                 h1("Output :"),
                 h3("Contents"),
                 tableOutput("contents")
               ) # mainPanel
      ), # Navbar 3, tabpanel
      tabPanel("BMI",
               # Input values
               sidebarPanel(
                 HTML("<h3>Input parameters</h3>"),
                 sliderInput("height",
                             label = "Height (in cm)", 
                             value = 175, 
                             min = 40, 
                             max = 250),
                 sliderInput("weight",
                             label = "Weight (in kg)", 
                             value = 70, 
                             min = 20, 
                             max = 200),
                 actionButton("submitbutton1",
                              "Submit", 
                              class = "btn btn-primary")
               ),
               
               mainPanel(
                 tags$label(h3('Status/Output')), # Status/Output Text Box
                 verbatimTextOutput('contents1'),
                 tableOutput('tabledata1') # Results table
               ) # mainPanel()
      ) # Navbar 4, tabpanel
    ) # navbarPage
  ) # fluidPage

  
####################################
# Server                           #
####################################
  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    }) # Name
    output$age <- reactive({
      as.integer((Sys.Date() - as.Date(input$date1, format = "%d/%m/%Y"))/365)
    }) # Age
    output$contents <- renderTable({
      file <- input$file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = input$header)
    }) # File
    
    # Input Data : bmi
    datasetInput1 <- reactive({  
      
      bmi <- input$weight/( (input$height/100) * (input$height/100) )
      
      if(bmi<18.5) {
        verdict <- "Underweight"
      }else if(bmi>=18.5 & bmi<24.9) {
        verdict <- "Healthy"
      }else if(bmi>=25 & bmi<29.9) {
        verdict <- "Overweight"
      }else {
        verdict <- "Obese"
      }
      
      Output <- data.frame(BMI = c(round(bmi,2),verdict))
      print(Output)
      
    })
    
    # Status/Output Text Box
    output$contents1 <- renderPrint({
      if (input$submitbutton1>0) { 
        isolate("Calculation complete.") 
      } else {
        return("Server is ready for calculation.")
      }
    })
    
    # Prediction results table
    output$tabledata1 <- renderTable({
      if (input$submitbutton1>0) { 
        isolate(datasetInput1()) 
      } 
    }) #BMI
    
  } # server
  
  
####################################
# Create the shiny app             #
####################################
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)
