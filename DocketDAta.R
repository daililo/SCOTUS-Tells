library(tidyverse)


SCBDIds <- read_csv("Docket Data/SCDB_2020_01_justiceCentered_Docket.csv")

SCBDIds  <- SCBDIds %>%
group_by(docketId) %>%
summarize(name = docket[1], caseId = caseId[1])

enosdata <- read_csv("enosdata/enos_sen_justices.csv")
enosdata

Edata <- read_csv("Epsteindata/epstein.csv")
Edata

BJdata <- read_csv("blackjohnsondata/black-johnson.csv")
BJdata

joined_enos <- inner_join(SCBDIds, enosdata, by = "docketId")

SCBDIds$docket <- as.character(SCBDIds$docket)
joined_edata <- inner_join(SCBDIds, Edata, by = "docket")
# Change from double to character or vice versa
# col types argument?

joined_BJdata <- left_join(BJdata, SCBDIds, by = "caseId")


joined_enos <- joined_enos %>%
  select(caseId, justiceName, pitch_diff, petitioner_vote)

joined_BJdata <- joined_BJdata %>%
  select(caseId, justiceName, justiceMQ, unpleasantDiff_totalWords, 
         pleasantDiff_totalWords)

joined_enosBJ <- inner_join(joined_enos, joined_BJdata, 
                            by = c("caseId", "justiceName"))
