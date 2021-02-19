# Tabulate statistics and make graphs for the AEA data editor report
# Lars Vilhuber
# - This reformats tables created from the ScholarOne system 
# - Source data is confidential.
# 2/18/2021

# Inputs
#   - file.path(basepath,"data","scholarone","dataEditorReport_20191128-20201127.xlsx") 
# Outputs
#   - Tables

scholarone = "dataEditorReport_20191128-20201127.xlsx"

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("dplyr","here","tidyr","tibble","remotes","stringr","readxl")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")

## Non-standard - install of a page with same name
#pkgTest.github("stargazer-booktabs","markwestcott34")
install_github("markwestcott34/stargazer-booktabs")
library(stargazer)

# Get Median number of rounds

## Get list of journals

jira.journals <- readRDS(file.path(jiraanon,"jira.anon.RDS")) %>%
            distinct(Journal) %>%
            filter(! Journal %in% c("JEP","JEL","","AEA P&P")) %>%
            mutate(Journal = str_replace(Journal,":"," "),
                   Journal = str_replace(Journal," Economics",""),
                   Journal = str_replace(Journal," Economic","")) 

## cycle over journals

rounds <- tibble()
for ( j in as.list(jira.journals$Journal)) {
  print(paste0("Reading in ",j))
  so <- read_excel(file.path(basepath,"data","scholarone",scholarone),
                 sheet = j, skip = 30, n_max = 3) %>% 
    select(Rounds = `Total Rounds`,Count = `Manuscripts`)
  so$Journal <- j
  rounds <- bind_rows(rounds,so)
  print(so)
}
rounds <- select(rounds,Journal,Rounds,Count)


# summarize

rounds.all <- rounds %>%
  group_by(Rounds) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  arrange(Rounds) %>%
  mutate(Percent = cumsum(100*Count/sum(Count)))

# pre-aggregated - which is the median?

median <- NA
for ( row in 1:nrow(rounds.all) ) {
  print(paste0(row))
  if ( rounds.all$Percent[row] > 50 & is.na(median) ) {
    median = rounds.all$Rounds[row]
  }
}

update_latexnums("medianrounds",median)


# create table

rounds.wide <- rounds %>%
  select(Journal,Rounds,Count) %>%
  pivot_wider(names_from = Journal,values_from = Count)

stargazer(rounds.wide,style = "aer",
          summary = FALSE,
          out = file.path(tables,"n_rounds.tex"),
          out.header = FALSE,
          float = FALSE,
          rownames = FALSE
)

