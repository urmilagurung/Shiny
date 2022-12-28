# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/


library(shiny)
data(airquality)

####################################
# UI                               #
####################################

# Define UI for app that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # App title
  titlePanel("Solar Radiation Level at Central Park, Langleys"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for the number of bins
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 0,
                  max = 40,
                  value = 20,
                  step = 2)
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Histogram
      plotOutput(outputId = "distPlot")
      
    )
  )
)

####################################
# Server                           #
####################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
    x    <- airquality$Solar.R
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Solar radiation level",
         main = "Histogram of Solar Radiation Level")
    
  })
  
}

####################################
# Create the shiny app             #
####################################

shinyApp(ui = ui, server = server)
