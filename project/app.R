#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

#Define UI
ui <- navbarPage("Navigation", #clickable navbar
  tabPanel("Intro", #intro 
            mainPanel(p("hi"))),
  
  tabPanel("Plot 1", #Plot 1 page within these parentheses
           mainPanel()),
  
  tabPanel("Plot 2", #Plot 2 page within these parentheses
           mainPanel()),
  
  tabPanel("Plot 3", #Plot 3 page within these parentheses
           mainPanel()),
  
  tabPanel("Conclusion",
           mainPanel())
  
)

#Define server
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
      #table
    })
    
    output$plot2 <- renderPlot({
      #bar graph
    })
    
    output$plot3 <- renderPlot({
      #third plot
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
