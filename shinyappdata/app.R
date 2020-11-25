library(shiny)
library(tidyverse)
library(shinythemes)

# Load in data through .rds files.

posterior_all_issue_ideology <- readRDS("posterior_all_issue_ideology.rds")
roberts_ideology_decisions <- readRDS("roberts.rds")
roberts_issues <- readRDS("robertsissues.rds")

# Create relevant lists that will be used as selection options in models.

justicelist <- unique(roberts_ideology_decisions$justiceName)
justicelist_no_oconnor <- justicelist[-2]
issuelist <- unique(roberts_ideology_decisions$issueArea)

# Begin shiny app code. 

ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
    "Examining the Ideology of the Roberts Court",
    tabPanel("About",
             h3("Hello! My name is Nick Maxwell."),
             p("Here is the link to my Github: 
               https://github.com/ndmaxwell/finalproject"),
             p("The goals of this project were two-fold. First, I wanted to get
             a better sense of how maleable justices' ideologies were over time. 
           Second, I wanted to see to what extent we would be able to predict a 
           justice's vote on a given issue area given their ideological score 
           (specifically, their Martin-Quinn score). To build this model and 
           inform my question, I first looked at date from the Washington 
           University Law's Supreme Court This historical data, combined 
           with the University of Michigan's 
           Justices dataset, allowed me to examine historical trends in the 
           Roberts court and make predictions for future voting. 
           The Justices dataset, containing Martin Quinn ideology scores for 
             each justice in each year can be found", 
             a("here", href="https://mqscores.lsa.umich.edu/measures.php"),
             ". The Washington University Law's Supreme Court databse can be 
             found",
             a("here", href="http://scdb.wustl.edu/data.php"), ".")),
    
    # End of panel 1
    
    tabPanel("Ideological History",
    sidebarLayout(
        sidebarPanel(
                checkboxGroupInput(
                    inputId = "Justices", 
                    label = "Choose Justices to Compare",
                    choices = justicelist_no_oconnor) 
        ),
        
        mainPanel(
           plotOutput("justiceplotselect"),
      
        p("Justice O'Connor is excluded from the choices in justies for this 
        plot, as she retired from the Court shortly after Chief Justice Roberts 
        joined. Because of this, it wouldn't make much sense to 
        examine changes in her ideology for such a short duration. Most justices
          stay relatively consistent in their ideologies over time, though 
          Justice Sotomayor becomes notably more liberal during her tenure on
          the Court.")),
        )
    ),
    
    # End of panel 2. The goal here was to give the viewer an idea of historic
    # ideology scores. The data here is pulled from the
    # "roberts_issue_area_votes" tibble in the data_organizing.Rmd file. It was
    # saved as an RDS and imported.

tabPanel("Roberts Court votes by Issue",
         sidebarLayout(
           sidebarPanel(
             selectInput(
               inputId = "Issue",
               label = "Select an Issue",
               choices = issuelist), 
           ),
           mainPanel(
             plotOutput("issueplot"),
             
             p("There is relative consistency here, with conservative justices 
               frequently having a greater proportion of conservative votes 
               relative to liberal justices. Though, it is important to note 
               that there are real disparities. A prime example of this is 
               for issues of economic activit, where Justice Kennedy has the 
               second highest proportion of conservative votes while being
               considered one of the famous moderates of the court. It is 
               also ipmortant to note that because we're looking at proportions,
               Justices O'Connor, Kavanaugh, and Gorsuch have fewer data points
               than others, as they had shorter tenures on the Roberts Court.")
             
           ))),

     # End of panel 3. Similarly, the goal here was to give viewers an idea of
     # the distribution of votes by different issue areas.

tabPanel("Predicting A Convervative Vote by Issue",
         sidebarLayout(
             sidebarPanel(
              selectInput(
               inputId = "Issue_2",
               label = "Select an Issue",
               choices = issuelist), 
         ),
         mainPanel(
             plotOutput("predictionplot"),
             
             p("This plot was built from a logistic regression that generated a
               posteior distribution for conservative votes by issue, with
               respect to a justice's theoretical ideology score (measures on a 
               scale of -4, most liberal, to 4, most conservative). This 
               distribution was then used to calculate probability for a 
               conservative vote given the input of a give ideological score."),
             
             p("As you can see, the probabilites are consistently increasing 
               for higher conservative ideological scores, with the exception of
               the broad miscalleaneous category. However, the 
               probabilities do differ wildly by issue. Even with a max 
               ideological conservatism score of 4, for the issue of federal 
               taxation, we don't even reach a 50% probability of a conservative
               vote. On issues of criminal procedure, however, inputting an
               ideological score of 4 will generate a strong likelihood of a 
               conservative vote (roughly 85%). Notably, the issue of privacy
               has the largest difference in probabilites between a justice
               with a conservative versus liberal persuasion, with the most
               liberal ideological score indicating a probability of roughly
               30% and the most conservative indicating a probability of roughly
               93%. Contrasting this with the issue of federalism, which has
               probabilties of roughly 30% at the conservative end and 50%
               at the liberal end, there is much more variation."),
             
             p("This discrepancy in variability carries important implications
               for thinking about the future of the Supreme Court and how 
               votes may shift as the political winds shift. For issues of civil
               rights, unions, and privacy, there appears to be more
               variability, such that if we assume a high baseline ideological
               conservatism score for Justice Barrett, we can assume a greater
               shift in voting patterns for the court versus an issue like 
               federalism, which tracks less closely with ideological
               persuasion.")
             
         )))

     # End of panel 4. This is the statistical model of the project and the most
     # complicated element - actually giving probabilities for votes on a given
     # issue area depending on baseline ideology.

))

     # End of ui.

server <- function(input, output) {
    
  output$justiceplotselect <- renderPlot({
    roberts_ideology_decisions %>% 
      filter(justiceName %in% input$Justices) %>% 
      ggplot(aes(x = year, y = post_mn, color = justiceName)) +
      geom_line() +
      theme_classic() +
      xlim(2005, 2019) +
      labs(y = "Ideology Score - Greater Values are more Conservative",
           x = "Year",
           title = "Ideology of Justices Over Time") +
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
         labs(title = "Posterior Distribution for Probability of 
              Voting Conservative",
              x = "Ideology (Greater Values are more Conservative)",
             y = "Probability") +
         scale_x_continuous(breaks = seq(-4, 4, 1)) +
          theme_linedraw()
    })

}

shinyApp(ui = ui, server = server)