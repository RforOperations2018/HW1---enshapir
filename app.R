#------------------------------
#Eric Shapiro
# R Shiny for Operations
# Homework 1
#------------------------------

#libraies 
library(shiny)
library(tidyverse)
library(shinythemes)


#infomartion on Data Used
# 
# ChickWeight
# 
# Format
# 
# An object of class c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame") containing the following columns:
#   
# weight - a numeric vector giving the body weight of the chick (gm).
# Time - a numeric vector giving the number of days since birth when the measurement was made.
# Chick - an ordered factor with levels 18 < ... < 48 giving a unique identifier for the chick. The ordering of the levels groups chicks on the same diet together and orders them according to their final weight (lightest to heaviest) within diet.
# Diet - a factor with levels 1, ..., 4 indicating which experimental diet the chick received.


Chicken.Weight <- ChickWeight

DietAverages <- Chicken.Weight %>% 
  group_by(Diet) %>% 
  summarise(Avg_Weight = mean(weight))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navbarPage("Chicken Diet Dashboard", 
             theme = shinytheme("flatly"),
             tabPanel("Chick Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput(inputId = "ChickToDisplay", 
                                       label = "Choose a Chick (18-48)",
                                       value = 18, 
                                       min = 18, 
                                       max =48,
                                       step = 1
                          )
                        ),
                        # Output plot
                        mainPanel(
                          plotOutput(outputId = "Chickenplot")
                        )
                      )
             ),
             # Data Table
             tabPanel("Diet Weight Averages",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "DaysIncluded", 
                                       label = "How Many Day to Include",
                                       value = 22, 
                                       min = 0, 
                                       max =22,
                                       step = 2
                          )
                        ),
                        # Output plot
                        mainPanel(
                          plotOutput(outputId = "DietAverages")
                        )
                      )
             )
  )
)

# Define server logic
server <- function(input, output) {
   
  output$Chickenplot <- renderPlot({
    #chickData <- filter(.data = Chicken.Weight, Chick == input$ChickToDisplay)
    ggplot(data = Chicken.Weight, aes(x = Time, y = weight, color = Diet)) + 
      geom_point() + 
      labs(x="Days Since Birth", y="Chick's Weight (gm)", title = "Chicken's Weight Since Birth")
  })
  output$DietAverages <- renderPlot({
    #chickData <- filter(.data = Chicken.Weight, Chick == input$ChickToDisplay)
    ggplot(data = DietAverages, aes(x = Diet, y = Avg_Weight, fill = Diet)) + 
      geom_bar(stat = "identity")+
      labs(x="Experimental Diet Type", y="Average Weight (gm) of Chicks", title = "Average Weight of Chicks by Diet Type")
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

