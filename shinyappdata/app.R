#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(tidymodels)
library(rstanarm)
justiceCenteredfixed <- readRDS("justice.rds")
roberts_ideology_decisions <- readRDS("roberts.rds")

justicelist <- unique(roberts_ideology_decisions$justiceName)

issuelist <- unique(roberts_ideology_decisions$issueArea)

prediction_model <- stan_glm(data = roberts_ideology_decisions,
                             formula = conservative ~ post_mn + issueArea,
                             refresh = 0,
                             family = binomial(link = "logit"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
    "Supreme Court Ideologies Through Time",


    # Application title
    tabPanel("Main",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #  sliderInput("year",
                    #    "Year:",
                     #   min = 1946,
                     #   max = 2020,
                      #  value = 1),
                selectInput(
                    inputId = "Justice",    # a name for the value you choose here
                    label = "Look at the Change in a Particular 
                    Justice's Ideology during the Robert's Court",
                    choices = justicelist)   # your list of choices to choose from
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # plotOutput("mostliberalplot"),
           plotOutput("indivjusticetime"),
           plotOutput("alljusticetime")
        )
    )
),

tabPanel("About",
         h3("This is an about me! My name is Nick Maxwell"),
         p("Here is the link to my Github: https://github.com/ndmaxwell/finalproject"),
         p("The goals of this project were two-fold. First, I wanted to get a 
           better sense of how maleable justices' ideologies were over time. 
           Second, I wanted to see to what extent we would be able to predict a 
           justice's vote on a given issue area given their ideological score 
           (specifically, their Martin-Quinn score). To build this model and find 
           the historical data necessary, I combined data from Washington 
           University Law's Supreme Court database, which gave me historical data 
           going back to the 1940s with every case and how every justice voted with 
           the University of Michigan's Justices dataset containing Martin Quinn 
           ideology scores for each justice in each year.")),

tabPanel("Predicting A Convervative Vote by Issue",
         sidebarLayout(
             sidebarPanel(
              selectInput(
               inputId = "Issue",
               label = "Select an Issue",
               choices = issuelist), 
              
         ),
         mainPanel(
             plotOutput("predictionplot")
         )))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  #  output$mostliberalplot <- renderPlot({
      #  justiceCenteredfixed %>% 
           # filter(year == input$year) %>%
           # arrange(desc(ideology)) %>% 
           # slice(1:5) %>%
           # ggplot(aes(x = justiceName, y = ideology)) +
           # geom_col() +
           # theme_classic() +
           # labs(y = "Ideology Score - Conservative (1) - Liberal (2)",
               #  x = "Justice",
               #  title = "5 Most Liberal Justices Serving on the Court")
    
   # })
    output$indivjusticetime <- renderPlot({
      roberts_ideology_decisions %>% 
            filter(justiceName == input$Justice) %>%
            ggplot(aes(x = year, y = post_mn)) +
            geom_line() +
            theme_classic() +
            xlim(2005, 2019) +
            labs(y = "Ideology Score - Higher Values are more Conservative",
                 x = "Year",
                 title = "Ideology of Justice Over Time")
    })
    output$alljusticetime <- renderPlot({
      roberts_ideology_decisions %>% 
        ggplot(aes(x = year, y = post_mn, color = justiceName)) +
        geom_line() +
        theme_classic() +
        xlim(2005, 2019) +
        labs(y = "Ideology Score - Higher Values are more Conservative",
             x = "Year",
             title = "Ideology of Justice Over Time") +
        scale_color_discrete(name = "Justice")
    })

    output$predictionplot <- renderPlot({
        
        prediction_data <- tibble("post_mn" = seq(-4, 4, 0.1),
                                  "issueArea" = input$Issue)
        prediction_posterior <- posterior_predict(
            prediction_model, newdata = prediction_data, draws = 4000) %>%
            as_tibble() %>%
            mutate_all(as.numeric) %>% 
            pivot_longer(cols = everything(),
                         names_to = "ideology_score",
                         values_to = "outcome") %>% 
            mutate(ideology_score = as.numeric(ideology_score)) %>%
            group_by(ideology_score) %>% 
            summarize(proportion_voting = mean(outcome), .groups = "drop") %>% 
            mutate(ideology_score = -4 + .1*(ideology_score - 1))
        
        prediction_posterior %>% 
            ggplot(aes(x = ideology_score, y = proportion_voting)) +
            geom_line() +
            labs(title = "Posterior Distribution for Probability of Voting Conservative",
                 x = "Ideology (Higher Scores are more Conservative)",
                 y = "Probability") +
            scale_x_continuous(breaks = seq(-4, 4, 1)) +
            theme_linedraw()
    })
    
output$ACBplot <- renderPlot({   
    new <- tibble("post_mn" = input$Ideology,
                  "issueArea" = input$Issue_2)
    
    prediction_posterior <- posterior_predict(prediction_model, newdata = new) %>%
        as_tibble() %>%
        mutate_all(as.numeric) %>% 
        summarize(prob.conservative = mean(`1`))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)