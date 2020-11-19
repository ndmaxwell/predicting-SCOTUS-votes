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
library(rstan)
justiceCenteredfixed <- readRDS("justice.rds")
roberts_ideology_decisions <- readRDS("roberts.rds")

justicelist <- roberts_ideology_decisions %>% 
    select(justiceName) %>% 
    unique()

issuelist <- roberts_ideology_decisions %>% 
    select(issueName) %>% 
    unique()

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
                    inputId = "Justice",                 # a name for the value you choose here
                    label = "Look at the Change in a Particular Justice's Ideology",
                    choices = justicelist)                       # your list of choices to choose from
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
           datasets to explore the ideological evolutions of different Justices based on their personal characteristics."))),

tabPanel("Predicting A Justice's Vote",
         sidebarLayout(
         selectInput(
             inputId = "Justice",
             label = "Select a Justice from the Roberts Court",
             choices = justicelist),
         selectInput(
             inputId = "Issue",
             label = "Select an Issue",
             choices = issuelist) 
         ),
         mainPanel(
             plotOutput("predictionplot")
         )))



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
        roberts_ideology_decisions %>% 
            
            filter(justiceName == input$Justice) %>%
            ggplot(aes(x = year, y = ideology)) +
            geom_line() +
            theme_classic() +
            labs(y = "Ideology Score - Conservative (1) - Liberal (2)",
                 x = "Year",
                 title = "Ideology of Justice Over Time")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)