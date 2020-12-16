# Anonymize JIRA process files and construct variables
# Harry Son
# 2020-12-11

## Inputs: export_12-11-2020.csv
## Outputs: file.path(jiraanon,"jira.anon.RDS") and file.path(jiraanon,"jira.anon.csv")

### Cleans working environment.
rm(list = ls())
gc()

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("dplyr","splitstackshape")
results <- sapply(as.list(global.libraries), pkgTest)

# double-check

if (! file.exists(file.path(jirabase,"export_12-11-2020.csv"))) {
  process_raw = FALSE
  print("Input file for anonymization not found - setting global parameter to FALSE")
}

if ( process_raw == TRUE ) {
# Read in data extracted from Jira
#base <- here::here()
  
    
    
jira.raw <- read.csv(file.path(jirabase,"export_12-11-2020.csv"), stringsAsFactors = FALSE) %>%
  rename(ticket=ï..Key) %>%
  rename(reason=Reason.for.Failure.to.Fully.Replicate) %>%
  rename(external=External.validation) %>%
  rename(subtask=Sub.tasks) %>%
  mutate(training = grepl("TRAINING", ticket, fixed = TRUE)) %>%
  filter(training == FALSE) %>%
  filter(Issue.Type=="Task") %>%
  mutate(date_created = as.Date(substr(Created, 1,10), "%m/%d/%Y"),
         date_resolved = as.Date(substr(Resolved, 1,10), "%m/%d/%Y"),
         date_updated = as.Date(substr(As.Of.Date, 1,10), "%m/%d/%Y"),
         mc_number = sub('\\..*', '', Manuscript.Central.identifier)) %>%
  cSplit("Software.used",",")  %>%
  cSplit("Changed.Fields",",")  %>%
  mutate(status_change = ifelse(Changed.Fields_1=="Status","Yes",ifelse(Changed.Fields_2=="Status","Yes",ifelse(Changed.Fields_3=="Status","Yes",ifelse(Changed.Fields_4=="Status","Yes","No"))))) %>%
  mutate(received = ifelse(Status=="Open"&Change.Author=="","Yes","No")) %>%
  mutate(has_subtask=ifelse(subtask!="","Yes","No")) %>%
  filter(! date_updated=="2020-12-11") %>%
  filter(ticket!="AEAREP-365") %>%
  select(-training)

jira.raw.subtask <- jira.raw %>%
  select(ticket, subtask) %>%
  cSplit("subtask",",")  %>%
  distinct() %>%
  pivot_longer(!ticket, names_to = "n", values_to = "value") %>%
    mutate(subtask=ifelse(!value=="","Yes","No")) %>%
  select(subtask,value) %>%
  rename(ticket=value) 

  
jira.raw.temp <- jira.raw %>%
  select(ticket, mc_number) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  filter(mc_number!="") %>%
  left_join(jira.raw,by="ticket") %>%
  rename(mc_number = mc_number.x) %>%
  select(-subtask) %>%
  left_join(jira.raw.subtask) ->jira.raw


## Keep only variables needed

jira.conf <- jira.raw %>%
  select(ticket,date_created,date_resolved,date_updated,mc_number,Journal,Status,Software.used_1,Software.used_2,Software.used_3,Software.used_4,received,status_change,external,reason,subtask,Resolution)


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
jira.conf %>% 
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
