library(shiny)
library(tidyverse)
library(shinythemes)
library(rstanarm)
library(RColorBrewer)

posterior_all_issue_ideology <- readRDS("posterior_all_issue_ideology.rds")
roberts_ideology_decisions <- readRDS("roberts.rds")
roberts_issues <- readRDS("robertsissues.rds")

justicelist <- unique(roberts_ideology_decisions$justiceName)
justicelist_no_oconnor <- justicelist[-2]
issuelist <- unique(roberts_ideology_decisions$issueArea)


ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
    "Examining the Ideology of the Roberts Court",
    tabPanel("About",
             h3("This is an about me! My name is Nick Maxwell"),
             p("Here is the link to my Github: https://github.com/ndmaxwell/finalproject"),
             p("The goals of this project were two-fold. First, I wanted to get a 
           better sense of how maleable justices' ideologies were over time. 
           Second, I wanted to see to what extent we would be able to predict a 
           justice's vote on a given issue area given their ideological score 
           (specifically, their Martin-Quinn score). To build this model and inform my question, 
           I first looked at date from the Washington 
           University Law's Supreme Court", a("database", href="http://scdb.wustl.edu/data.php")),
           p("This historical data, combined with the University of Michigan's Justices dataset,
             allowed me to examine historical trends in the Roberts court and make
             predictions for future voting. The Justices dataset, containing
             Martin Quinn ideology scores for each justice in each year can be found", 
             a("here", href="https://mqscores.lsa.umich.edu/measures.php"))),
    
    
    tabPanel("Historic Ideology Scores",
    sidebarLayout(
        sidebarPanel(
                checkboxGroupInput(
                    inputId = "Justices", 
                    label = "Look at the Change of in the Ideologies of Justices over Time",
                    choices = justicelist_no_oconnor) 
        ),
        
        mainPanel(
           plotOutput("justiceplotselect"),
      
        p("Justice O'Connor is excluded from the choices in justies for this plot, 
          as she retired from the Court shortly after Chief Justice Roberts joined.
          Because of this, it wouldn't make much sense to examine changes in her 
          ideology for such a short duration.")),
        )
    ),

tabPanel("How has the Roberts Court Voted on Different Issues?",
         sidebarLayout(
           sidebarPanel(
             selectInput(
               inputId = "Issue",
               label = "Select an Issue",
               choices = issuelist), 
           ),
           mainPanel(
             plotOutput("issueplot")
           ))),

tabPanel("Predicting A Convervative Vote by Issue",
         sidebarLayout(
             sidebarPanel(
              selectInput(
               inputId = "Issue_2",
               label = "Select an Issue",
               choices = issuelist), 
         ),
         mainPanel(
             plotOutput("predictionplot")
         )))

))


server <- function(input, output) {
    
  output$justiceplotselect <- renderPlot({
    roberts_ideology_decisions %>% 
      filter(justiceName %in% input$Justices) %>% 
      ggplot(aes(x = year, y = post_mn, color = justiceName)) +
      geom_line() +
      theme_classic() +
      xlim(2005, 2019) +
      labs(y = "Ideology Score - Higher Values are more Conservative",
           x = "Year",
           title = "Ideology of Justice Over Time") +
      scale_color_discrete(name = "Justice")
    
  })
  
  output$issueplot <- renderPlot({   
    roberts_issues %>% 
      filter(issueArea == input$Issue) %>% 
      ggplot(aes(y = fct_reorder(justiceName, percentage), x = percentage,
                 fill = justiceName)) +
      geom_col(show.legend = F) +
      labs(x = "Proportion of Conservative Votes",
           y = "Justice",
           title = "Examining Conservative Vote Distribution by Issue",
           subtitle = "Conservatism isn't the same across the board") +
      scale_fill_manual(values = c(rep("dodgerblue4", 14)))
  })
  
    output$predictionplot <- renderPlot({
      posterior_all_issue_ideology %>% 
        filter(issueArea == input$Issue_2) %>% 
        ggplot(aes(x = post_mn, y = outcome)) +
         geom_line(color = "dodgerblue4") +
         labs(title = "Posterior Distribution for Probability of Voting Conservative",
              x = "Ideology (Higher Scores are more Conservative)",
             y = "Probability") +
         scale_x_continuous(breaks = seq(-4, 4, 1)) +
          theme_linedraw()
    })

}

shinyApp(ui = ui, server = server)