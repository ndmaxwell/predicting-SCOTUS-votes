library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)

# Load in data through .rds files. See the data_organizing.Rmd file for more
# information on each of these .rds files.

posterior_all_issue_ideology <- readRDS("posterior_all_issue_ideology.rds")
roberts_ideology_decisions <- readRDS("roberts.rds")
roberts_issues <- readRDS("robertsissues.rds")

# Create relevant lists that will be used as selection options in models.

justicelist <- unique(roberts_ideology_decisions$justiceName)
justicelist_no_oconnor <- justicelist[-2]

# We need a list without O'Connor for the panel on the ideological history of
# the justices, as Justice O'Connor was only on the Roberts Court for a brief
# period of time.

issuelist <- unique(roberts_ideology_decisions$issueArea)

# This list contains the 13 different issue areas that we'll be using. It will
# be necessary for the drop down buttons.

# Begin shiny app code. 

ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
    "Examining the Ideology of the Roberts Court",
    
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
      
        p("Justice O'Connor is excluded from the choices in justices for this 
        plot, as she retired from the Court shortly after Chief Justice Roberts 
        joined. Because of this, it wouldn't make much sense to 
        examine changes in her ideology for such a short duration. Most justices
          stay relatively consistent in their ideologies over time, though 
          Justice Sotomayor becomes notably more liberal during her tenure on
          the Court.")),
        )
    ),
    
    # End of panel 1. The goal here was to give the viewer an idea of historic
    # ideology scores. The data here is pulled from the
    # "roberts_issue_area_votes" tibble in the data_organizing.Rmd file. It was
    # saved as an RDS and imported.

tabPanel("Voting by Issue",
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
               for issues of economic activity, where Justice Kennedy has the 
               second highest proportion of conservative votes while being
               considered one of the famous moderates of the court. It is 
               also important to note that because we're looking at proportions,
               Justices O'Connor, Kavanaugh, and Gorsuch have fewer data points
               than others, as they had shorter tenures on the Roberts Court.")
             
           ))),

     # End of panel 2. Similarly, the goal here was to give viewers an idea of
     # the distribution of votes by different issue areas. This was a simple
     # enough task using the roberts_issues tibble from the robertsissues.rds
     # file. Again, see the data_organizing.Rmd file for more information on
     # this tibble.

tabPanel("Predicting Votes",
         sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(
               inputId = "Issue_2",
               label = "Select Issues to Compare",
               choices = issuelist), 
         ),
         mainPanel(
             plotOutput("predictionplot"),
             
             p("This plot was built from a logistic regression that generated a
               posterior distribution for conservative votes by issue, with
               respect to a justice's theoretical ideology score (measures on a 
               scale of -4, most liberal, to 4, most conservative). This 
               distribution was then used to calculate probability for a 
               conservative vote given the input of a give ideological score."),
             
             p("As you can see, the probabilities are consistently increasing 
               for higher conservative ideological scores, with the exception of
               the broad miscellaneous category. However, the 
               probabilities do differ wildly by issue. Even with a max 
               ideological conservatism score of 4, for the issue of federal 
               taxation, we don't even reach a 50% probability of a conservative
               vote. On issues of criminal procedure, however, inputting an
               ideological score of 4 will generate a strong likelihood of a 
               conservative vote (roughly 85%). Notably, the issue of privacy
               has the largest difference in probabilities between a justice
               with a conservative versus liberal persuasion, with the most
               liberal ideological score indicating a probability of roughly
               30% and the most conservative indicating a probability of roughly
               93%. Contrasting this with the issue of federalism, which has
               probabilities of roughly 30% at the conservative end and 50%
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
             
         ))),

# End of panel 3. This is the statistical model of the project and the most
# complicated element - actually giving probabilities for votes on a given
# issue area depending on baseline ideology. The text in the shinyapp
# itself gives context to the results.

tabPanel("About",
         h3("Hello! My name is Nick Maxwell."),
         p("Here is the link to my Github:",
           a("https://github.com/ndmaxwell", 
             href="https://github.com/ndmaxwell")),
         p("The goals of this project were two-fold. First, I wanted to get
            a better sense of how malleable justices' ideologies were over time. 
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
           ". The Washington University Law's Supreme Court database can be 
             found",
           a("here", href="http://scdb.wustl.edu/data.php"), "."))

     # End of panel 4. This is my about page to give context to the project and
     # link relevant resources!

))

     # End of ui.

server <- function(input, output) {
    
  output$justiceplotselect <- renderPlot({
    roberts_ideology_decisions %>% 
      filter(justiceName %in% input$Justices) %>% 
      ggplot(aes(x = year, y = post_mn, color = justiceName)) +
      geom_line() +
      theme_economist() +
      xlim(2005, 2019) +
      labs(y = "Ideology Score - Greater Values are more Conservative",
           x = "Year",
           title = "Ideology of Justices Over Time") +
      scale_color_discrete(name = "Justice") +
      theme(axis.title.y = 
              element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = 
              element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  })
  
  # This is the plot in the "Ideological History" tab. It is a line chart that
  # allows the user to select justices from the Roberts Court and see their
  # ideological evolution. The post_mn variable is the measurement of their
  # ideology (their Martin-Quinn score). This value was assigned each year by
  # the researchers at the University of Michigan and made the creation of this
  # particular plot relatively straightforward. Much of the code we see here are
  # design elements. I've been a regular reader of The Economist for a few years
  # now and prefer to use the theme whenever possible. I did have to use the
  # element_text call to change the axis title spacing on the plot to improve
  # the readability.
  
  output$issueplot <- renderPlot({   
    roberts_issues %>% 
      filter(issueArea == input$Issue) %>% 
      ggplot(aes(y = fct_reorder(justiceName, percentage), x = percentage,
                 fill = justiceName)) +
      geom_col(show.legend = F) +
      labs(x = "Proportion of Conservative Votes",
           y = "Justice",
           title = "Examining Conservative Vote Distribution by Issue") +
      scale_fill_manual(values = c(rep("dodgerblue4", 14))) +
      theme_economist() +
      theme(axis.title.y = 
              element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = 
              element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  })
  
  # This plot, in the "Voting by Issue" tab allows the user to see the
  # proportion of conservative votes by issue. Aesthetic decisions follow the
  # reasoning of the first plot and the theme of the shinyapp itself. The
  # creation of the plot was straightforward after the data wrangling was
  # finished. See the data_organizing.Rmd comments for more context on that if
  # desired.
  
    output$predictionplot <- renderPlot({
      posterior_all_issue_ideology %>% 
        filter(issueArea %in% input$Issue_2) %>% 
        ggplot(aes(x = post_mn, y = outcome, color = issueArea)) +
         geom_line() +
         labs(title = "Posterior Distribution for Probability of 
              Voting Conservative",
              x = "Ideology (Greater Values are more Conservative)",
             y = "Probability") +
         scale_x_continuous(breaks = seq(-4, 4, 1)) +
         theme_economist() +
         theme(axis.title.y = 
                element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = 
                element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
        scale_color_discrete(name = "Issue") 
    })
    
    # This is the big payoff of the project, which is in the "Predicting Votes"
    # tab. The complicated work, again, was done in the data wrangling. After
    # that work (see data_organizing.Rmd), I can simply create a line plot with
    # the outcome of a conservative vote probability on the y-axis and the
    # theoretical ideological score on the X. After meeting with my TF, I
    # decided to make this a checkbox input plot rather than a drop down so
    # viewers could more easily see distinctions between different issue areas.
    # Seeing them side by side, we can see that some issues have very little
    # change, while some (like unions) have probabilities that swing from near 0
    # to near 1.

}

shinyApp(ui = ui, server = server)