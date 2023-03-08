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
  tabPanel("Introduction", #intro 
            titlePanel("Depression and Anxiety Student Data"),
            sidebarLayout(
              sidebarPanel(
                p(em("Project by: Mackenzie Kimbrough, Paul Garces, Yi Zhou, and 
                 Vanessa Sakdy"))
              ),
              
              mainPanel(
                h1("The data:"),
                p("The dataset used is is an organized collection of responses from 
                undergraduate students at the University of Lahore in Pakistan. 
                  The dataset is uploaded to Kaggle by Shahzad Ahmed."),
                p("The data contains 19 variables:"),
                p("School year: Years in school in number"), 
                p("Age: Student's age in years"),
                p("Gender: Student's gender categorized as male or female"),
                p("BMI: Body mass index in continuous numerical form"),
                p("Who BMI: Body mass index by category"),
                p("PHQ score: "),
                h2("What we want to find:"),
                p("Our goal is to assess how many of the students experience anxiety
                  and depression. Our", em("main audience"), "is the college educators
                  who would like to have better awareness of the mental state of
                  students. Specifically, we would like to use the data we have
                  to analyze attributes of students who have or can access treatment,
                  and if there is any stronger prevalence of anxiety/depression
                  by year in school."),
                h2("Potential issues:"),
                p("An", em("ethical"), "issue we quickly noticed was the students' BMI
                  (body mass index) in numerical form and category. While this 
                  information can be helpful in mental health studies, we figured
                  that it may not be relevant for our purposes. An ethical issue outside
                  of the scope of this project is whether the students' identities
                  were properly protected as this data contains sensitive information.
                  One of the notable limits of this dataset is that many of the 
                  variables are in true/false form, which makes numerical visualizations
                  limited.")
              )
              
            ),
      
           ),
  
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
