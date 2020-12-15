
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
   vote in agreement with. My study uses cases from the years 2004 to 2008."),
  
  p("All of these three tells are succesful in predicting the probability 
   of how a Justice will vote on their own. Yet I believe that these three 
   tells should be looked at together, not individually. I believe that more 
   than one of the tells will be successfully observed in a given Justice's 
   behavior. For example, if we have a case which shows Justice Scalia raising 
   his pitch at a petitioner, questioning the petitioner more, and using 
   language considered negative towards the petitoner, than we have a higher 
   probability of predicting Scalia's vote during the oral argument stage 
   alone. I believe that these tells do not show up indviidually, but instead 
   in pairs or all three. If we find these tells in a Justice's interaction 
   during the oral argument stage, we will be able to predict their vote before 
   they make it."),
  
  # Discussion incorporated because I wanted to bring in previous written 
  # studies on my project in case the reader wanted more information.
  
  # Added images t
  
  img(src = "SCOTUS2020.jpg", height = 400, width = 600),
  
  img(src = "Court.jpg", height = 400, width = 600)),

  # Added images to my opening page because I wanted my app to open by showing
  # the Background section but also wanted it to be colorful and inviting
  

  tabPanel(
    "Plots",
    h3("Looking at Tells Individually"),
    sidebarPanel(
      selectInput("tell_type","Tell",
                  c("Pitch", "Language", "Questions")
      ),
      
      # This panel is used to choose which plot one wants to visualize, with
      # each tell having its own plot.
    
    p("Through this first graph, we are able to visualize how pitch differs
      greatly among individual Justices. Through the usage of existing machine
      learning technology, Justice's have been shown to change the tone of 
      their voice when speaking to a petitioner they agree or disagree with.
      The higher their voice gets, the more they disagree with a petitioner.
      For example, we see Justice Rehnquist having a lower pitch and agreeing 
      with petioners more from the years 2004 to 2008. We see a similar trend
      for Justice Scalia. What is interesting is that we see nothing for 
      Justice Thomas, who is known to barely ever speak in oral arguments, 
      therefore unable to generate enough data. However, we do know that Thomas
      is the most conservative leaning Justice on the Court, making his vote 
      easier to predict."),
    
    br(),    
    
    # Super cool function to space out my paragraphs!
    
    p("Language usage shows us which Justices rely most on using words with
      negative connotations to show their disapproval in the petitioner. 
      Interestingly, we see that Rehnquist favors this tell, and uses language
      with a negative connotation frequently when in disagreement. The 
      variability in the graph shows that such difference in word usage exists
      and used when speaking to the petitioner. Again, we see no data for 
      Justice Thomas, due to the fact he simply just doesn't speak."),
    
    br(),    
    
    p("The final graph shows the variability of questions that each Justice
      asks to either the petitioner or respondent, showing that this really
      does differ significantly depending on whether or not the Justice agrees
      with their line of reasoning or not.")),
    
    mainPanel(

      plotOutput("Plots")
      
      # One plot will display per choice.
      
    )
  ),
  
  tabPanel(
    "Models",
    
    # These take time to load on the app, give them a second!
    
    h3("Looking at Models"),
    sidebarPanel(
      selectInput("model_type","Model",
                  c("Pitch", "Language", "Interaction")
      ),
      
      # Similar to the plots, this lets a viewer choose one of the plots.
      
      p("This first model shows the change in petitioner vote based on a 
        Justice's pitch. This is a Baynesian Logistic regression model, 
        meaning that the Justice's vote was coded as either a 1 or 0 (1
        indicating that the Justice did vote for the petitioner and 0 
        meaninf that they voted for the respondent). My beta_1, pitch,
        indicates that a lower pitch resulted in more petitioner votes, which
        is exactly what was expected to occur. A lower pitch indicates more of
        an agreement. For each individual Justice we see that the had a lower
        pitch when they voted for the petitioner. Even those who had a pitch 
        above 0, their normal range, it was not that significantly above it.
        Again, we see Thomas being the outlier. We also see that Justices 
        Souter, Stevens, Ginsburg, and Breyer were most reliant in speech as
        their tell, while Rehnquist was the least."),
      
      br(),    
      
      p("We see very similar results when looking at the usage of unpleasant
        words. Justices who have negative values are less likely to use 
        words with negative connotations when voting for the petitioner. The 
        values of this model are very similar to the model depicting pitch
        difference, showing that Justices are likely to indicate both tells
        when questioning during the oral argument stage."),
      
      br(),
      
      p("This model depicts the interaction between a Justice's pitch and
        their use of unpleasant words. Due to both of these variables being
        good indicators individually of how a Justice will vote in a case, it
        is likely that together they are better predictors. The model shows
        that when interacted, the tells are better able to predict a Justice's
        vote. For example, Scalia had a value of -0.28 in the unpleasant
        word category, and a value of -0.21 when looking at pitch (read
        above for description on what the negative values mean). Yet, the 
        interaction between the two has a lower negative value, -0.31, 
        indicating that together they give a better probability of how Justice 
        Scalia will vote on the case. This is apparent with the other Justices 
        as well, proving that these tells together are good indicators of how a 
        Justice will vote."),
      
      br(),
      
      p("The last tell requires further studying and testing before
        being regressed with other variables.")),
    
    mainPanel(
      
      gt_output("model1")
      
      # Was not using gt_output before and was wondering why I was getting an
      # error!
      
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
            
            -  Black, R. C., Treul, S. A., Johnson, T. R., & Goldman, J. 
            (2011). 
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

  # Created the plots below using if else statements in order to get the 
  # desired plot to correspond with the correct panel choice.
  
  # Interesting that I never liked using if else statements and am now
  # incorporating them into my final project.
  
  # These plots have been created in my rmd file and are exactly the same 
  # from what I have there. Therefore, I put the comments pertaining to this
  # code in the rmd file.
  
  output$Plots <- renderPlot({
    if(input$tell_type == "Pitch") {
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
           caption = "Source = enos_sen_justices.csv") }

    
    else if(input$tell_type == "Language") {
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
             caption = "Source = black-johnson.csv") }
  
    
    else if(input$tell_type == "Questions") {
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
             caption = "Source = epstein.csv") }
    
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
  
  # Similar to use and function of if else statements as above.
  
  # As before, comments for the code exist in the rmd file.
  
  output$model1 <- render_gt({
    
    if(input$model_type == "Pitch") {
      
      set.seed(10)
      
      t_prior2 <- student_t(df = 7, location = 0, scale = 2.5)
      
      model_2 <- stan_glm(petitioner_vote ~ pitch_diff + justiceName, 
                          data = all_cases,
                          family = binomial(link = "logit"), 
                          prior = t_prior2, prior_intercept = t_prior2, QR=TRUE,
                          refresh = 0)
      
      model_2 %>%
        
        tbl_regression()  %>%
        
        as_gt() %>%
        
        tab_header(title = "Regression of Pitch Difference on Petioner Vote",
                   subtitle = "Per Each Justice") %>%
        
        tab_source_note("Source: enos_sen_justices.csv") }
    
    
    else if(input$model_type == "Language") {
      
      set.seed(10)
      
      t_prior <- student_t(df = 7, location = 0, scale = 2.5)
      
      model_1 <- stan_glm(petitioner_vote ~ unpleasantDiff_totalWords + 
                            justiceName, 
                          data = all_cases,
                          family = binomial(link = "logit"), 
                          prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                          refresh = 0)
      
      model_1 %>%
        
        tbl_regression()  %>%
        
        as_gt() %>%
        
        tab_header(title = "Regression of Language Difference on 
                   Petioner Vote",
                   subtitle = "Per Each Justice") %>%
        
        tab_source_note("Source: black-johnson.csv") }
    
    
    else if(input$model_type == "Interaction") {
      
      set.seed(10)
      
      t_prior3 <- student_t(df = 7, location = 0, scale = 2.5)
      
      model_3 <- stan_glm(petitioner_vote ~ pitch_diff * unpleasantDiff_totalWords 
                          + justiceName, 
                          data = all_cases,
                          family = binomial(link = "logit"), 
                          prior = t_prior3, prior_intercept = t_prior3, QR=TRUE,
                          refresh = 0)
      
      model_3 %>%
        
        tbl_regression()  %>%
        
        as_gt() %>%
        
        tab_header(title = "Interaction of Pitch and Language Difference on 
             Petioner Vote",
                   subtitle = "Per Each Justice") %>%
        
        tab_source_note("Sources: enos_sen_justices.csv &
                  black-johnson.csv") }
  
  })
  
}

shinyApp(ui, server)


