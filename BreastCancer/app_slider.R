# Import libraries
library(shiny)
library(data.table)
library(shinythemes)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

#TrainSet$y[TrainSet$y == 2] <- "benign"
#TrainSet$y[TrainSet$y == 4] <- "malignant"
str(TrainSet)
#TrainSet$y <-  as.factor(x = TrainSet$y)
#TrainSet = data.frame(TrainSet[-7],y=as.integer(TrainSet$y)*2)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("journal"),
  
  # Page header
  headerPanel('Prediction of Cancer Class'),
                
  # Input values
  sidebarPanel(
    tags$label(h3("Input parameters")),
    sliderInput(inputId = "Cl.thickness", label = "Clump Thickness:",
                min = min(TrainSet$Cl.thickness), max = max(TrainSet$Cl.thickness),
                value = 4, step =1),
    sliderInput(inputId = "Cell.shape", label = "Uniformity of Cell Shape:",
                min = min(TrainSet$Cell.shape), max = max(TrainSet$Cell.shape),
                value = 3, step =1),
    sliderInput(inputId = "Marg.adhesion", label = "Margin Adhesion:",
                min = min(TrainSet$Marg.adhesion), max = max(TrainSet$Marg.adhesion),
                value = 3, step =1),
    sliderInput(inputId = "Bare.nuclei", label = "Bare Nuclei:",
                min = min(TrainSet$Bare.nuclei), max = max(TrainSet$Bare.nuclei),
                value = 4, step =1),
    sliderInput(inputId = "Bl.cromatin", label = "Bland Chromatin:",
                min = min(TrainSet$Bl.cromatin), max = max(TrainSet$Bl.cromatin),
                value = 3, step =1),
    sliderInput(inputId = "Normal.nucleoli", label = "Normal Nucleoli:",
                min = min(TrainSet$Normal.nucleoli), max = max(TrainSet$Normal.nucleoli),
                value = 2, step =1),
    
    actionButton(inputId = "submitbutton", label = "Submit", class = "btn-primary btn-lg")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
                  
    )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # Cl.thickness,Cell.shape,Marg.adhesion,Bare.nuclei,Bl.cromatin,Normal.nucleoli
    df <- data.frame(
      Name = c("Cl.thickness",
               "Cell.shape",
               "Marg.adhesion",
               "Bare.nuclei",
               "Bl.cromatin",
               "Normal.nucleoli"),
      Value = as.numeric(c(input$Cl.thickness,
                             input$Cell.shape,
                             input$Marg.adhesion,
                             input$Bare.nuclei,
                             input$Bl.cromatin,
                             input$Normal.nucleoli)),
      stringsAsFactors = FALSE)
    
    Class <- "Class"
    df <- rbind(df, Class)
    input <- transpose(df)

    write.table(input,"input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=ifelse(predict(model,test,type="response") > 0.5, "Malignant", "Benign"))
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction complete.") 
    } else {
      return("Server is ready for prediction.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)