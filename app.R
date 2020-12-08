
library(shiny) 
library(tidyverse)
library(rstanarm)
library(ggridges)
library(patchwork)
library(viridis)
library(gtsummary)
library(gt)
library(shinythemes)
library(broom.mixed)

all_cases <- read_csv("all_cases/all_cases.csv")

# Instead of including all of my data sets on the shiny app I just brought in
# the one that I made.

ui <- fluidPage(theme = shinytheme("cosmo"), navbarPage(
  "Analyzing the Emotional and Psychological “Tells” of a Supreme Court 
  Justice",
  
  tabPanel("Discussion",
  titlePanel("Background"),
  p("I used three different datasets in completing my analysis, each 
   containing an independent study on a particular tell. The first 
   one looks at emotional arousal in Justices. It is proven that the vocal 
   pitch and tone of a Justice rises when they hear something displeasing 
   in oral argument. By analyzing vocal pitch alone, one can accurately 
   predict approximately 57.50% of a Justice’s vote on average, and correctly 
   predict the outcomes of 66.55% overall cases. Next, another study focuses on 
   the emotional importance of subconscious word choice used by Justices in 
   oral argument. The use of “unpleasant” words towards one of the lawyers 
   in question will likely indicate that the attorney will be the one 
   representing the side of the case which will not be supported by the Justice 
   in their final decision. Finally, the last study finds evidence that 
   Justices are more prone to question the lawyer who they are more likely to 
   disagree with in an oral argument than one who they are likely to eventually 
   vote in agreement with."),
  
  p("All of these three tells are succesful in predicting the probability 
   of how a Justice will vote on their own. Yet I believe that these three 
   tells should be looked at together, not individually. I believe that more 
   than one of the tells will be successfully observed in a given Justice's 
   behavior. For example, if we have a case which shows Justice Scalia raising 
   his pitch at a petitioner, questioning the petitioner more, and using 
   language considered negative towards the petitoner, than we have a higher 
   probability of predicting Scalia's vote during the oral argument stage alone. 
   I believe that these tells do not show up indviidually, but instead in pairs 
   or all three. If we find these tells in a Justice's interaction during the 
   oral argument stage, we will be able to predict their vote before they make 
   it.")),


  tabPanel(
    "Plots",
    
    mainPanel(

      plotOutput("vocalpitch"),
      plotOutput("UnpWords"),
      plotOutput("questions"),
      
    )
  ),
  
  tabPanel(
    "Models",
    
    mainPanel(
      
      gt_output("model1")
      
    )
  ),
  
  tabPanel("About",
           titlePanel("About"),
          h3("Overview"),
           p("
           I hope to study the following claim: Justices have been observed 
           to make three emotional and psychological “tells” during oral 
           arguments: a change of tone and pitch when speaking, the use of 
           specific words in their lines of questioning and the connotations of 
           these words, and the patterns of questioning they exemplify as a 
           whole during questioning. Through close analysis of these “tells,” 
           the Justice’s decision can be accurately made at the stage of oral 
           arguments. By combining the accuracies of each specific tell, one 
           will be able to calculate a higher probability of accuracy on how 
           the Justice will vote in the case."),
          h3("Data"),
          p("The datasets I used come from the following studies:
            
            - Dietrich, B. J., Enos, R. D., & Sen, M. (2018). 
            Emotional Arousal Predicts Voting on the U.S.
            
            -  Black, R. C., Treul, S. A., Johnson, T. R., & Goldman, J. (2011). 
            Emotions, Oral Arguments, and Supreme Court Decision Making. 
            The Journal of Politics, 73(2), 572–581.
            
            -  Epstein, L., Landes, W. M., & Posner, R. A. (2009). 
            Inferring the Winning Party in the Supreme Court from the Pattern 
            of Questioning at Oral Argument. SSRN Electronic Journal."),
          p("Check out my code", 
            a("here",
              href = "https://github.com/daililo/gov50-shiny")),
          h3("About me"),
          p("Hi! My name is Daiana Lilo and I am a Government concentrator 
          interested in American politics and political research. Specifically,
          I am really interested in the process of decision-making, whether it 
          be by Supreme Court Justices or US policymakers. I hope to use my new
          skills in R when working on my thesis in the future, which will also
          be focused on decision-making and predictive modelling in American
          politics."),
          
          p("When I'm not creating creating models in R, I am involved with
            the Albanian Student Association on campus, the Small Claims 
            Advisory Service, and the IOP. Check me out on my",
            a("Linkedin",
              href = "https://www.linkedin.com/in/daiana-lilo-b1a552187/"),
            "to see some other cool stuff I've worked on!"),
          
          p("You can reach me @ dlilo@college.harvard.edu, especially if you're
            working on decision-making or are a SCOTUS fanatic like me!"))
  ),

  
)


server <- function(input, output, session) {

  
  output$vocalpitch <- renderPlot({
    all_cases %>%
      ggplot(aes(x = pitch_diff, y = justiceName, fill = justiceName)) +
      geom_density_ridges(bandwidth = .214) +
      xlim(c(-2.5, 2.5)) +
      scale_fill_viridis(option = "plasma", discrete = TRUE) +
      geom_vline(xintercept = 0) +
      theme_classic() +
      theme(legend.position = "none") +
      labs(title = "Range of a Justice's Pitch Difference When Questioning the
Petitioner of a Case",
           x = "Pitch Difference",
           y = "Justice Name",
           caption = "Source = enos_sen_justices.csv")

    
  })
  
  output$UnpWords <- renderPlot({
      all_cases %>%
      ggplot(aes(x = unpleasantDiff_totalWords, y = justiceName,
                 fill = justiceName)) +
      geom_density_ridges(bandwidth = .772) +
      xlim(c(-5, 5)) +
      scale_fill_viridis(option = "cividis", discrete = TRUE) +
      geom_vline(xintercept = 0) +
      theme_classic() +
      theme(legend.position = "none") +
      labs(title = "Connotation of Word Choice for Each Justice",
           x = "Unpleasant Word Usage",
           y = "Justice Name",
           caption = "Source = black-johnson.csv") 
      
  })
  
  output$questions <- renderPlot({
    all_cases %>%
      select(justiceName, jquest_r, jquest_p) %>%
      pivot_longer(cols = jquest_r:jquest_p, 
                   names_to = "jquest",
                   values_to = "Questions") %>%
      ggplot(aes(x = Questions, y = justiceName, fill = jquest)) +
      geom_density_ridges(bandwidth = 30, alpha = .5) +
      facet_wrap(~ jquest) +
      theme_classic() +
      scale_fill_manual(values = c("red", "blue"),
                         labels = c("Questions to Petioner", 
                                    "Questions to Respondent"),
                         name = "Questions") +
      scale_x_continuous(labels = c("0", "10", "20", 
                                    "30", "40", "50",
                                    "60", "70", "80", "90",
                                    "100"),
                         breaks = seq(0, 100, 10),
                         limits = c(0, 100)) +
      labs(title = "Questions to the Petitioner vs. Respondent",
           x = "Number of Questions",
           y = "Justice Name",
           caption = "Source = epstein.csv")
    
  })
    
  output$model1 <- render_gt({
    
    set.seed(10)
    
    t_prior <- student_t(df = 7, location = 0, scale = 2.5)
    
    model_1 <- stan_glm(petitioner_vote ~ pitch_diff + 
                        unpleasantDiff_totalWords + justiceName, 
                        data = all_cases,
                        family = binomial(link = "logit"), 
                        prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                        refresh = 0) 
      
      tbl_regression(model_1, intercept = TRUE)  %>%
      
      as_gt() %>%
      
      tab_header(title = "Regression of Pitch Difference on Petioner Vote",
                 subtitle = "Per Each Justice") %>%
      
      tab_source_note("Source: enos_sen_justices.csv")
  
  })
  
}

shinyApp(ui, server)


