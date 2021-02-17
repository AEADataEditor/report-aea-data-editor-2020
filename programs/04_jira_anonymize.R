# Anonymize JIRA process files and construct variables
# Harry Son
# 2020-12-11

## Inputs: export_12-22-2020.csv
## Outputs: file.path(jiraanon,"jira.anon.RDS") and file.path(jiraanon,"jira.anon.csv")

### Cleans working environment.
rm(list = ls())
gc()

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("dplyr","tidyr","splitstackshape")
results <- sapply(as.list(global.libraries), pkgTest)

# double-check

if (! file.exists(file.path(jirabase,"export_12-22-2020.csv"))) {
  process_raw = FALSE
  print("Input file for anonymization not found - setting global parameter to FALSE")
}

if ( process_raw == TRUE ) {
# Read in data extracted from Jira
#base <- here::here()
  
    
    
jira.conf.raw <- read.csv(file.path(jirabase,"export_12-22-2020.csv"), stringsAsFactors = FALSE) %>%
  rename(ticket=Key) %>%
  rename(reason.failure=Reason.for.Failure.to.Fully.Replicate) %>%
  rename(external=External.validation) %>%
  rename(subtask=Sub.tasks) %>%
  mutate(training = grepl("TRAINING", ticket, fixed = TRUE)) %>%
  filter(training == FALSE) %>% # filter out training cases
  filter(Issue.Type=="Task") %>% # leave issue type "Task"
  mutate(date_created = as.Date(substr(Created, 1,10), "%m/%d/%Y"),
         date_resolved = as.Date(substr(Resolved, 1,10), "%m/%d/%Y"),
         date_updated = as.Date(substr(As.Of.Date, 1,10), "%m/%d/%Y"),
         mc_number = sub('\\..*', '', Manuscript.Central.identifier)) %>%
  mutate(received = ifelse(Status=="Open"&Change.Author=="","Yes","No")) %>%
  mutate(has_subtask=ifelse(subtask!="","Yes","No")) %>%
  filter(! date_updated=="2020-12-22") %>% #export is counted as an action, drop
  filter(ticket!="AEAREP-365") %>% # duplicate with aearep-364
  filter(ticket!="AEAREP-1589") %>% ## Decision notice of aearep-1523
  select(-training) 

## object to filter out subtask
jira.conf.subtask <- jira.conf.raw %>%
  select(ticket, subtask) %>%
  cSplit("subtask",",")  %>%
  distinct() %>%
  pivot_longer(!ticket, names_to = "n", values_to = "value") %>%
    mutate(subtask=ifelse(!value=="","Yes","No")) %>%
  select(subtask,value) %>%
  rename(ticket=value) 

jira.conf.temp <- jira.conf.raw %>%
  select(ticket, mc_number) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  filter(mc_number!="") %>%
  left_join(jira.conf.raw,by="ticket") %>%
  rename(mc_number = mc_number.x) %>%
  select(-subtask) %>%
  left_join(jira.conf.subtask) ->jira.conf.raw


## Keep variables needed
jira.conf <- jira.conf.raw %>%
  select(ticket,date_created,date_updated,mc_number,Journal,Status,
         Software.used,received,
         Changed.Fields,external,subtask,Resolution,reason.failure,
         MCRecommendation,MCRecommendationV2)


# anonymize
jira.tmp <- jira.conf %>% 
  select(mc_number) %>% distinct() 
jira.tmp <- jira.tmp %>%
  bind_cols(as.data.frame(runif(nrow(jira.tmp))))
names(jira.tmp)[2] <- c("rand")

jira.manuscripts <- jira.tmp %>%
  arrange(rand) %>%
  mutate(mc_number_anon = row_number()) %>%
  select(-rand) %>%
  arrange(mc_number)

# Now merge the anonymized data on
jira.conf <- jira.conf %>% 
  left_join(jira.manuscripts,by="mc_number")

saveRDS(jira.conf,file=file.path(jirabase,"jira.conf.RDS"))
write.csv(jira.conf,file=file.path(jirabase,"jira.conf.csv"))

jira.conf %>% 
  select(-mc_number) -> jira.anon

saveRDS(jira.anon,file=file.path(jiraanon,"jira.anon.RDS"))
write.csv(jira.anon,file=file.path(jiraanon,"jira.anon.csv"))

} else { 
  print("Not processing anonymization due to global parameter.")
  }
