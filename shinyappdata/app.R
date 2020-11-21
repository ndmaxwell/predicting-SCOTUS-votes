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
            sliderInput("year",
                        "Year:",
                        min = 1946,
                        max = 2020,
                        value = 1),
                selectInput(
                    inputId = "Justice",    # a name for the value you choose here
                    label = "Look at the Change in a Particular Justice's Ideology",
                    choices = justicelist)   # your list of choices to choose from
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mostliberalplot"),
           plotOutput("indivjusticetime")
        )
    )
),

tabPanel("About",
         h3("This is an about me! My name is Nick Maxwell"),
         p("Here is the link to my Github: https://github.com/ndmaxwell/finalproject"),
         p("For this project, I'm pulling data from several sources related to the Supreme Court.
               One set contains the full text from nearl every Supreme Court decision, and another
           records decisions by Justices as defined by ideology. Finally, I have a dataset that makes 
           use of more personal background information on the Justices. Collectively, I intend to use these
           datasets to explore the ideological evolutions of different Justices based on their personal characteristics.")),

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
    
    output$mostliberalplot <- renderPlot({
        justiceCenteredfixed %>% 
            filter(year == input$year) %>%
            arrange(desc(ideology)) %>% 
            slice(1:5) %>%
            ggplot(aes(x = justiceName, y = ideology)) +
            geom_col() +
            theme_classic() +
            labs(y = "Ideology Score - Conservative (1) - Liberal (2)",
                 x = "Justice",
                 title = "5 Most Liberal Justices Serving on the Court")
    
    })
    output$indivjusticetime <- renderPlot({
        justiceCenteredfixed %>% 
            filter(justiceName == input$Justice) %>%
            ggplot(aes(x = year, y = ideology)) +
            geom_line() +
            theme_classic() +
            labs(y = "Ideology Score - Conservative (1) - Liberal (2)",
                 x = "Year",
                 title = "Ideology of Justice Over Time")
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
                 x = "Ideology",
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