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
                p("PHQ score: Depression symptoms scored on a scale of 0-27"),
                p("Depression severity: A categorical scale of depression symptoms"),
                p("Depressiveness: True/false if student is depressive"),
                p("Suicidal: True/false if student is suicidal"),
                p("Depression diagnosis: True/false if student has a depression diagnosis"),
                p("Depression treatment: True/false if student is receiving treatment for depression"),
                p("GAD score: Anxiety symptoms scored on a scale of 0-21"),
                p("Anxiety severity: A categorical scale of anxiety symptoms"),
                p("Anxiousness: True/false if student has anxiousness"),
                p("Anxiety diagnosis: True/false if student has an anxiety diagnosis"),
                p("Anxiety treatment: True/false if student is receiving treatment for anxiety"),
                p("Epworth score: A scale of sleepiness from 0-12"),
                p("Sleepiness: True/false if student experiences sleepiness"),
                
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
  
  tabPanel("Table", #Table page within these parentheses
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("options", "Choose option(s):",
                                  choices = c("School Year" = "school_year",
                                              "Age" = "age",
                                              "Gender" = "gender",
                                              "Sleepiness" = "sleepiness"),
                                  selected = c("School Year" = "school_year",
                                               "Gender" = "gender")),
               p("Anxiety: displays the average gad_score"),
               p("gad_score scale:"),
               p("0-4: Minimal Anxiety"),
               p("5-9: Mild Anxiety"),
               p("10-14: Moderate Anxiety"),
               p("Greate than 15: Severe Anxiety"),
               p("phq_score scale:"),
               p("Depression: displays the average phq_score"),
               p("phq_score scale:"),
               p("0-4: Minimal Depression"),
               p("5-9: Mild Depression"),
               p("10-14: Moderate Depression"),
               p("Greate than 15: Severe Depression"),
               textOutput("text"),
             ),
             mainPanel(tableOutput("table")))),
  
  
  tabPanel("Conclusion",
           titlePanel("Conclusion"),
           mainPanel()
           )
  
)

#Define server
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
      #table
    })
    
    output$plot2 <- renderPlot({
      #bar graph
    })
    
    #Table
    
    mh_table <- reactive({ 
      mentalhealth %>%
        select(input$options)
    })
    
    output$table <- renderTable({ 
      mentalhealth %>%
        filter(!is.na(phq_score)) %>% 
        filter(!is.na(gad_score)) %>% 
        group_by(mh_table()) %>%
        summarise(Anxiety = mean(gad_score, na.rm = TRUE),
                  Depression = mean(phq_score, na.rm = TRUE), .groups = "drop")
    })
    
    
    output$text <- renderText({
      max_anxiety <- mentalhealth %>%
        filter(!is.na(phq_score)) %>% 
        filter(!is.na(gad_score)) %>% 
        group_by(mh_table()) %>%
        summarise(Anxiety = mean(gad_score, na.rm = TRUE), .groups = "drop") %>%
        top_n(1, Anxiety) %>%
        pull(Anxiety) %>% 
        max()
      
      max_depression <- mentalhealth %>%
        filter(!is.na(phq_score)) %>% 
        filter(!is.na(gad_score)) %>% 
        group_by(mh_table()) %>%
        summarise(Depression = mean(phq_score, na.rm = TRUE), .groups = "drop") %>% 
        top_n(1, Depression) %>%
        pull(Depression) %>% 
        max()
      paste("The maximum average anxiety score is:", round(max_anxiety, digits = 2),
            "The maximum average depression score is:", round(max_depression, digits = 2))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
