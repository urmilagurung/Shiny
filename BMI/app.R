library(shiny)
library(shinythemes)


####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("BMI Calculator:",
                           tabPanel("Adult",
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
                             actionButton("submitbutton",
                                          "Submit", 
                                          class = "btn btn-primary")
                             ),
                           
                           mainPanel(
                               tags$label(h3('Status/Output')), # Status/Output Text Box
                               verbatimTextOutput('contents'),
                               tableOutput('tabledata') # Results table
                               ) # mainPanel()
                           ), #tabPanel(), Adult
                           
                           tabPanel("Child and Teen",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput(inputId = "age",
                                                  label = "Age (in year)", 
                                                  value = 2, 
                                                  min = 0, 
                                                  max = 20,
                                                  step = 1),
                                      radioButtons(inputId = "gender",
                                                   label = "Gender",
                                                   c("Boy" = "boy",
                                                   "Girl" = "girl")),
                                      sliderInput(inputId = "height1",
                                                  label = "Height (in cm)", 
                                                  value = 175, 
                                                  min = 2, 
                                                  max = 250),
                                      sliderInput(inputId = "weight1",
                                                  label = "Weight (in kg)", 
                                                  value = 15, 
                                                  min = 0, 
                                                  max = 120),
                                      actionButton(inputId = "submitbutton1",
                                                   label = "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents1'),
                                      tableOutput('tabledata1') # Results table
                                    ) # mainPanel()
                           ), #tabPanel(), Home
                           
                           
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {

  ####Adult
  
  # Input Data
  datasetInput <- reactive({  
    
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
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })

  
  ## Child/Teen
  
  #Input
  datasetInput1 <- reactive({  
    bmi_child <- input$weight1/( (input$height1/100) * (input$height1/100) )
  
  if (input$gender == "boy" & input$age <= 4) {
    if(bmi_child<(15-(input$age*0.3))) {
        verdict1 <- "Underweight"
      }else if(bmi_child>=(15-(input$age*0.3)) & bmi_child<(18.5-(input$age*0.3))) {
        verdict1 <- "Healthy"
      }else if(bmi_child>=(18.5-(input$age*0.3)) & bmi_child<(19.5-(input$age*0.3))) {
        verdict1 <- "Overweight"
      }else {
        verdict1 <- "Obese"
      }
    }
    
  if (input$gender == "boy" & input$age > 4) {
    if(bmi_child<(15+(input$age*0.3))) {
      verdict1 <- "Underweight"
    }else if(bmi_child>=(15+(input$age*0.3)) & bmi_child<(18.5+(input$age*0.3))) {
      verdict1 <- "Healthy"
    }else if(bmi_child>=(18.5+(input$age*0.3)) & bmi_child<(19.5+(input$age*0.3))) {
      verdict1 <- "Overweight"
    }else {
      verdict1 <- "Obese"
    }
  }

  if (input$gender == "girl" & input$age <= 4) {
    if(bmi_child<(14.4-(input$age*0.3))) {
      verdict1 <- "Underweight"
    }else if(bmi_child>=(14.4-(input$age*0.3)) & bmi_child<(18-(input$age*0.3))) {
      verdict1 <- "Healthy"
    }else if(bmi_child>=(18-(input$age*0.3)) & bmi_child<(19.1-(input$age*0.3))) {
      verdict1 <- "Overweight"
    }else {
      verdict1 <- "Obese"
    }
  }
  
    if (input$gender == "girl" & input$age > 4) {
      if(bmi_child<(14.4+(input$age*0.4))) {
        verdict1 <- "Underweight"
      }else if(bmi_child>=(14.4+(input$age*0.4)) & bmi_child<(18+(input$age*0.4))) {
        verdict1 <- "Healthy"
      }else if(bmi_child>=(18+(input$age*0.4)) & bmi_child<(19.1+(input$age*0.4))) {
        verdict1 <- "Overweight"
      }else {
        verdict1 <- "Obese"
      }
    }
  
  Output1 <- data.frame(BMI = c(round(bmi_child,2),verdict1))
  print(Output1)
  
    
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
  })  
}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
