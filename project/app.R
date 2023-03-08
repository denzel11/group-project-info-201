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
library(tidyverse)

mentalhealth <- read_delim("depression_anxiety_data.csv")
score <- c("0-4", "5-9", "10-14", "15+")
anxiety_depression <- c("none", "mild", "moderate", "severe")
phq_gad <- data_frame(score, anxiety_depression)


#Define UI
ui <- navbarPage("Navigation", #clickable navbar
  tabPanel("Introduction", #intro 
            titlePanel("Depression and Anxiety Student Data"),
            sidebarLayout(
              sidebarPanel(
                p(em("Project by: Mackenzie Kimbrough, Paul Garces, Yi Zhou, and 
                 Vanessa Sakdy (Group BD3)"))
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
                  by year in school. Most importantly, we aim to visualize the
                  data so that it becomes digestible and presents the data in a way
                  where one can learn from it."),
                
                
                h2("Potential issues:"),
                p("An", em("ethical"), "issue we quickly noticed was the students' BMI
                  (body mass index) in numerical form and category. While this 
                  information can be helpful in mental health studies, we figured
                  that it may not be relevant for our purposes. An ethical issue outside
                  of the scope of this project is whether the students' identities
                  were properly protected as this data contains sensitive information.
                  One of the notable limits of this dataset is that many of the 
                  variables are in true/false form, which makes numerical visualizations
                  limited. There are also N/A values in rows that otherwise are filled
                  with data. This may affect the sampling of the data.")
              )
              
            ),
      
           ),
  
  tabPanel("Plot 1",
           titlePanel("school year and phq score scatter plot"),
           sidebarLayout(
             sidebarPanel(
               selectInput("bmi", "Choose a bmi type:",
                           choices = c("normal", "overweight")),
               p(strong("This plot produces a scatter plot in which users can select either view the normal weight or overweight result")),
               p(em("this plot intends to explore which school year students have the highest phq score. Also, this plot explores whether gender and bmi type would result in change in phq score")),
             ),
             mainPanel(
               plotOutput("plot1"),
               textOutput("text1")))),
  
  # creating the widget where the user can select whether they want to see the stack bar graph
  # for either depression or anxiety & plotting the the stack bar graph in the mainPanel
  tabPanel("Plot 2",
           titlePanel("Depression & Anxiety Stack Bar Graphs"),#Plot 2 page within these parentheses
           sidebarLayout(
             sidebarPanel(
               selectInput("graph_type", "Choose a Graph:",
                           choices = c("Depression", "Anxiety")),
               p(strong("This plot, produces a stack bar graph in which the user selects to either view the Depression or Anxiety results.")),
               p(em("The graph counts the number of students who fall in a certain category which measures the severity of the respective mental",
                    " illness & counts the number of students who are diagnosed with the mental illness.")),
             ),
             mainPanel(
               plotOutput("plot"),
               textOutput("selected_graph")
             )
           )
  ),
  
  tabPanel("Table", #Table page within these parentheses
           titlePanel("Average Anxiety and Depression Table"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("options", "Choose option(s):",
                                  choices = c("School Year" = "school_year",
                                              "Age" = "age",
                                              "Gender" = "gender",
                                              "Sleepiness" = "sleepiness"),
                                  selected = c("School Year" = "school_year",
                                               "Gender" = "gender")),
               p(strong("The table displays the average phq_score (Depression) and gad_score (Anxiety) for the data.")),
               p("The check boxes allows users to group the data by different variables. This allows users to see which variables affect depression and anxiety levels in students more."),
               tableOutput("score"),
               p(em("The table above displays the anxiety and depression severity based on the phq/gad scores.")),
               textOutput("text"),
               
             ),
             mainPanel(tableOutput("table")))),
  
  
  tabPanel("Conclusion",
           titlePanel("Conclusion"),
           mainPanel(
             p("This small study allows viewers to gather a preliminary idea of 
               how and which students experience levels of depression and anxiety."),
             p(strong("Conclusion 1: "), "Mental health issues are severely underdiagnosed.
               In all categories of severity, there is a considerable portion of
               students who have depression and anxiety based off their survey
               scores but are undiagnosed. This pattern of underdiagnosed illnesses
               suggests that students' mental health is not receiving proper attention."),
             p(strong("Conclusion 2: "), "Based off of averaged (mean) survey scores,
               anxiety and depression are more prevalent in first and second year
               students. This trend is not seen in the averages based on age, suggesting
               that year in school may be relevant in students' mental health status,
               regardless of their age."),
             p(strong("Conclusion 3: "), "Anxiety and depression scores are, on average,
               higher for female students than their male counterparts. However,
               both genders' scores are within range for some level of illness that
               may cause concern.")
           )
           )
  
)

#Define server
server <- function(input, output) {
  over_plot <- reactive({
    if(input$bmi == "overweight"){
      over_data <- mentalhealth %>%
        filter(who_bmi == "Overweight")
      ggplot(over_data, aes(x = school_year, y = phq_score, color = gender)) +
        geom_point()+
        geom_smooth()
    }
  })
  norm_plot <- reactive({
    if(input$bmi == "normal"){
      norm_data <- mentalhealth %>%
        filter(who_bmi == "Normal")
      ggplot(norm_data, aes(x = school_year, y = phq_score, color = gender)) +
        geom_point()+
        geom_smooth()
    }
  })
  output$plot1 <- renderPlot({
    if(input$bmi == "overweight"){
      over_plot()
    } else if(input$bmi == "normal"){
      norm_plot()
    }
  })
  output$text1 <- renderText({
    norm_data <- mentalhealth %>%
      filter(who_bmi == "Normal" & !is.na(phq_score))
    over_data <- mentalhealth %>%
      filter(who_bmi == "Overweight" & !is.na(phq_score))
    paste("The average phq score for normal weight people is:", round(mean(norm_data$phq_score), digits = 2),
          "   The average phq score for overweight people is:", round(mean(over_data$phq_score), digits = 2))
    
    
  })
    
    # using the reactive function and if statement so that the stack bar graph info will be produced
    # when the user selects to view the Depression stack bar graph & using ggplot to put the info
    # on the graph, but the graph is not produced as output here.
    depression_graph <- reactive({
      if (input$graph_type == "Depression"){
        depression_count <- mentalhealth %>%
          filter(!is.na(depression_severity) & depression_severity != "NA") %>%
          filter(!is.na(depression_diagnosis)) %>% 
          group_by(depression_severity, depression_diagnosis) %>%
          summarise(count = n()) %>%
          mutate(depression_diagnosis = factor(depression_diagnosis, levels = c("TRUE", "FALSE")))
        
        ggplot(depression_count, aes(x = depression_severity, y = count, fill = depression_diagnosis)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(x = "Depression Severity", y = "Count") +
          scale_fill_manual(values = c("light blue", "dark red"), name = "Depression Diagnosis",
                            labels = c("True", "False"))
      }
    })
    
    # using the reactive function and if statement so that the stack bar graph info will be produced
    # when the user selects to view the Anxiety stack bar graph & using ggplot to put the info
    # on the graph, but the graph is not produced as output here.
    anxiety_graph <- reactive({
      if(input$graph_type == "Anxiety"){
        anxiety_count <- mentalhealth %>%
          filter(!is.na(anxiety_severity) & anxiety_severity != "NA") %>%
          group_by(anxiety_severity, anxiety_diagnosis) %>%
          summarise(count = n()) %>%
          mutate(anxiety_diagnosis = factor(anxiety_diagnosis, levels = c("TRUE", "FALSE")))
        
        ggplot(anxiety_count, aes(x = anxiety_severity, y = count, fill = anxiety_diagnosis)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(x = "Anxiety Severity", y = "Count") +
          scale_fill_manual(values = c("light blue", "dark red"), name = "Anxiety Diagnosis",
                            labels = c("True", "False"))
      }  
    })
    
    # using the renderPlot function and an if/else if statement to actually print the graphs
    # depending if the user selects to the Depression or Anxiety graph
    output$plot <- renderPlot({
      if(input$graph_type == "Depression"){
        depression_graph()
      } else if(input$graph_type == "Anxiety"){
        anxiety_graph()
      }
    })
    
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
        pull(Anxiety) %>% 
        max()
      
      max_depression <- mentalhealth %>%
        filter(!is.na(phq_score)) %>% 
        filter(!is.na(gad_score)) %>% 
        group_by(mh_table()) %>%
        summarise(Depression = mean(phq_score, na.rm = TRUE), .groups = "drop") %>% 
        pull(Depression) %>% 
        max()
      paste("The maximum average anxiety score is:", round(max_anxiety, digits = 2),
            "The maximum average depression score is:", round(max_depression, digits = 2))
      
    })
    
    output$score <- renderTable({
      phq_gad
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
