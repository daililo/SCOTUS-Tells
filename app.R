library(shiny) # you may need to install.packages() this
library(tidyverse)

library(shiny)
library(fec16)


# These are the datasets I will be using
blackjohnson <- read_csv("blackjohnsondata/black-johnson.csv")
enos <- read_csv("enosdata/enos_sen_justices.csv")
epstein <- read_csv("Epsteindata/epstein.csv")


######################################################################################
######################################################################################
#
# 1. Shiny Apps have two basic parts to them
#
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
#
#      -- You can also click the green "Run App" button on the top right or
#         run runApp() in the console

ui <- fluidPage(navbarPage(
  "Shiny Example",
  
  tabPanel("Discussion",
           p("I used three different datasets in completing my analysis, each 
             containing an independent study on a particular tell. The first one 
            looks at emotional arousal in Justices. It is proven that the vocal 
             pitch and tone of a Justice rises when they hear something displeasing 
             in oral argument. By analyzing vocal pitch alone, one can accurately 
             predict approximately 57.50% of a Justice’s vote on average, and correctly 
             predict the outcomes of 66.55% overall cases. Next, another study focuses on 
             the emotional importance of subconscious word choice used by Justices in 
             oral argument. The use of “unpleasant” words towards one of the lawyers 
             in question will likely indicate that the attorney will be the one representing 
             the side of the case which will not be supported by the Justice in their 
             final decision. Finally, the last study finds evidence that Justices are 
             more prone to question the lawyer who they are more likely to disagree with 
             in an oral argument than one who they are likely to eventually vote in agreement 
             with. \nAll of these three tells are succesful in predicting the probability 
             of how a Justice will vote on their own. Yet I believe that these three tells 
             should be looked at together, not individually. I believe that more than 
             one of the tells will be successfully observed in a given Justice's behavior. 
             For example, if we have a case which shows Justice Scalia raising his pitch 
             at a petitioner, questioning the petitioner more, and using language considered
             negative towards the petitoner, than we have a higher probability of predicting 
             Scalia's vote during the oral argument stage alone. I believe that these tells
             do not show up indviidually, but instead in pairs or all three. If we find these
             tells in a Justice's interaction during the oral argument stage, we will be able to
             predict their vote before they make it. \n Currently, I am binding my datasets together
             in order to create a comprehensive dataset with this information."  )),


  tabPanel(
    "Data",
    
    
    
    # - UIs are built from "panel" functions, which specify areas of your page.
    #
    #   -- There is a "main panel," a "sidebar," a "title," etc.
    
    # Here is a sidebar!

    sidebarPanel(
      selectInput(
        inputId = "justiceName",                 # a name for the value you choose here
        label = "Choose a Justice",   # the name to display on the slider
        choices = enos$justiceName                       # your list of choices to choose from
      ),
      
      sliderInput(
        inputId = "selected_size",                  # a name for the value you choose here
        label = "Choose a number as a point size:", # the label to display above the slider
        min = 0,                                    # the min, max, and initial values
        max = 5,
        value = 2 
      )
      
    ),
    
    
    # And here is your "main panel" for the page.
    
    mainPanel(
      # - You can also make your UI more complicated with UI elements.
      #
      #   -- In general, these are defined by functions that you give arguments to 
      #      (e.g. min and max values).
      #
      # - These include:
      #
      #   -- selectInput() to choose from multiple options.
      #
      #   -- sliderInput() lets you choose a value from a slider of values you define.
      #
      #   -- radioButtons() let you choose a button from a number of options
      #
      #   -- textInput() lets you enter whatever text you want.
      #
      #   -- Lots of other options, like entering a date. Look at the resources for 
      #      other choices!
      #
      # - You then assign these inputs to a value and use those values in other places, 
      #   like in plots!
      #
      # - All of these functions have their own arguments. For example:
      
      radioButtons(
        inputId = "selected_color",             # a name for the value you choose here
        label = "Choose a color!",              # the label to display above the buttons
        choices = c("red", "blue", "green")     # the button values to choose from
      ),
      
      textInput(
        inputId = "entered_text",               # a name for the value you choose here
        label = "Place your title text here:",  # a label above the text box
        value = "Example Title"                 # an initial value for the box
      ),
      
      # Not using this yet but I did not want to comment it out because I was 
      # getting an error whenever I tried to do so.
      
      
      textOutput("state_message"),              # load a text object called "state_message"
      textOutput("size_message"),
      textOutput("color_message"),
      textOutput("text_message"),
      plotOutput("vocalpitch"),
      plotOutput("questionR"),
      plotOutput("questionP")
    )
  ),
  tabPanel("About",
             h3("This is an about me! My name is Daiana Lilo"),
           p("I am a Government concentrator interested in the workings of 
             the Supreme Court."),
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
           the Justice will vote in the case."))
  ),

  
)


server <- function(input, output, session) {
  # - Then, you use these named objects to update the data on your site via the input object.
  #
  #   -- render() functions are what show content that will change live on your site.
  #
  #   -- so here, renderText() is updating live text based on your choice.
  
  
  
  # Just like renderText(), we can renderPlot()!
  
  output$vocalpitch <- renderPlot({
    # When I tried to make the tibble and use the max, min value functions 
    # I kept getting errors, the CA in study hall said to just comment it out
    # for Milestone 4 and figure out in the future how to do it?
    # we need to use () here after the name of our dataset because it is reactive!
  # enos %>%
   #tibble(justice_name = c("AScalia", "AMKennedy", 
                                        # "DHSouter", "RBGinsburg", 
                                        # "JGRoberts"),
                      #   min_val = -2:2,
                      #   max_val = -2:2,
                       #  med_val = -2:2
      ggplot(enos, aes(x = justiceName, y = pitch_diff)) + 
        geom_point() + 
        # geom_errorbar will add a bar from ymin to ymax
       # geom_errorbar(aes(ymin = min_val, ymax = max_val),
             #         width = 0.4) + 
        coord_flip() + theme_bw() + 
        labs(title = "Justice Ranges in Pitch",
             x = "Pitch Deviation",
             y = "Justice") 
     # Tried to use xlim(-2.5, 2.5), but was not working
  })
  output$questionR <- renderPlot({
      ggplot(epstein, aes(x = jquest_r, fill = UnanVote)) + 
        geom_histogram(binwidth = 1, color = "white") + 
      labs(title = "Respondent",
           x = "Questions to Respondent",
           y = "Count") 
      
  })
  
  output$questionP <- renderPlot({
    ggplot(epstein, aes(x = jquest_p, fill = UnanVote)) + 
      geom_histogram(binwidth = 1, color = "white") + 
      labs(title = "Petitioner",
           x = "Questions to Petitioner",
           y = "Count") 
    
  })
  
}

shinyApp(ui, server)


