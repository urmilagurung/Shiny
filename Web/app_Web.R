
# Modified from: 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
#install.packages("shinythemes")
library(shinythemes)

####################################
# UI                               #
####################################

  # Define UI
  ui <- fluidPage(
    shinythemes::themeSelector(),
    navbarPage(
      "My 1st App",
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
      tabPanel("Read the file",
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
      ) # Navbar 3, tabpanel
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
  } # server
  
  
####################################
# Create the shiny app             #
####################################
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)
