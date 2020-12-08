library(tidyverse)

# This is where I read in and create my final data set

SCBDIds <- read_csv("Docket Data/SCDB_2020_01_justiceCentered_Docket.csv")

# This data set contains all the case information needed in order for me to
# join my data sets together by case.

SCBDIds  <- SCBDIds %>%
  group_by(docketId) %>%
  summarize(name = docket[1], caseId = caseId[1])

# Simple group_by to focus on docketId



# -------------------------



enosdata <- read_csv("enosdata/enos_sen_justices.csv")

# Enos data set is for pitch

Edata <- read_csv("Epsteindata/epstein.csv",
                  col_types = cols(
                    X1 = col_character(), 
                    docket = col_character(),
                    term = col_character(),
                    quest_total = col_character(),
                    word_total = col_character(),
                    jquest_p = col_character(),
                    jquest_r = col_character(),
                    jwords_p = col_character(),
                    jwords_r = col_character()
                  ))

# Edata is for questions

BJdata <- read_csv("blackjohnsondata/black-johnson.csv")

# BJdata is for negative word connotation



# -------------------------



joined_enos <- inner_join(SCBDIds, enosdata, by = "docketId")

# Had to join each data set with the case data set separately in order to
# eventually join them altogether.

SCBDIds$docket <- as.character(SCBDIds$docketId)


joined_edata <- inner_join(SCBDIds, Edata, by = "docket")

# Change from double to character or vice versa
# col types argument?

joined_BJdata <- left_join(BJdata, SCBDIds, by = "caseId")



# -------------------------



joined_enos <- joined_enos %>%
  
  select(caseId, justiceName, pitch_diff, petitioner_vote)

# Realized I had too many columns for each data set, had to only choose the 
# ones I really am interested in using.

joined_BJdata <- joined_BJdata %>%
  
  select(caseId, justiceName, justiceMQ, unpleasantDiff_totalWords, 
         pleasantDiff_totalWords)


table(joined_enos$caseId %in% joined_BJdata$caseId)


joined_enosBJ <- inner_join(joined_enos, joined_BJdata, 
                            by = c("caseId", "justiceName"))

# Used inner_join to join these first two data sets together, used both
# caseId and justiceName, kept all the other selected rows



# -------------------------



newdata <- read.csv("new_datatest/SCDB_2020_01_justiceCentered_Docket.csv")

# This data set is very similar to the first one called, but with it its 
# easter to join the data from Epstein

joined_Ep <- inner_join(Edata, newdata, by = "docket")


table(Edata$docket %in% joined_enosBJ$caseId)

# summarize newdata to one row per case

newdata <- newdata %>%
  
  group_by(caseId, justiceName) %>%
  
  summarise(caseId = caseId[1],
            
            justiceName = justiceName[1],
            
            docket = docket[1])

# problem was that some cases are repeated multiple
# times in Edata: for example, 05-11284 is there four times
# not sure why, but if we summarize first we can get one row per case

# some cases are repeated, so use this section to 


Edata <- Edata %>%
  group_by(docket) %>%
  summarise(docket = docket[1],
            jquest_r = jquest_r[1],
            jquest_p = jquest_p[1],
            jwords_r = jwords_r[1],
            jwords_p = jwords_p[1])

# get the columns that you want in addition to the 
# join id docket: I used jquest_r, jquest_p, jwords_r, jwords_p



# -------------------------



enosBJ <- left_join(joined_enosBJ, newdata, by = c("caseId", "justiceName"))

# Can now begin to join all data sets together

table(enosBJ$docket %in% Edata$docket)

all_cases <- left_join(enosBJ, Edata, by = "docket")

# Creation of master data set that includes all of the tells together!

head(all_cases)

write.csv(all_cases, "all_cases/all_cases.csv")

# Super cool and useful function I learned to save my new data set for use in
# my shiny app.

