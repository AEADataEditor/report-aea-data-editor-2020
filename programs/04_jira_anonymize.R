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
  filter(! date_updated=="2020-12-22") %>%
  filter(ticket!="AEAREP-365") %>%
  select(-training)

## split out the reasons 
jira.conf.raw <- jira.conf.raw %>%
cSplit("reason.failure",",") %>% 
  transform(reason1 = ifelse(reason.failure_01=="Discrepancy"|reason.failure_02=="Discrepancy"|
                               reason.failure_03=="Discrepancy"|reason.failure_04=="Discrepancy"|
                               reason.failure_05=="Discrepancy"|reason.failure_06=="Discrepancy"|
                               reason.failure_07=="Discrepancy"|reason.failure_08=="Discrepancy"|
                               reason.failure_09=="Discrepancy"|reason.failure_10=="Discrepancy"|
                               reason.failure_11=="Discrepancy"|reason.failure_12=="Discrepancy"|
                               reason.failure_13=="Discrepancy"|reason.failure_14=="Discrepancy"|
                               reason.failure_15=="Discrepancy"|reason.failure_16=="Discrepancy"|
                               reason.failure_17=="Discrepancy"|reason.failure_18=="Discrepancy"|
                               reason.failure_19=="Discrepancy"|reason.failure_20=="Discrepancy"|
                               reason.failure_01=="Discrepancy in output"|reason.failure_02=="Discrepancy in output"|
                               reason.failure_03=="Discrepancy in output"|reason.failure_04=="Discrepancy in output"|
                               reason.failure_05=="Discrepancy in output"|reason.failure_06=="Discrepancy in output"|
                               reason.failure_07=="Discrepancy in output"|reason.failure_08=="Discrepancy in output"|
                               reason.failure_09=="Discrepancy in output"|reason.failure_10=="Discrepancy in output"|
                               reason.failure_11=="Discrepancy in output"|reason.failure_12=="Discrepancy in output"|
                               reason.failure_13=="Discrepancy in output"|reason.failure_14=="Discrepancy in output"|
                               reason.failure_15=="Discrepancy in output"|reason.failure_16=="Discrepancy in output"|
                               reason.failure_17=="Discrepancy in output"|reason.failure_18=="Discrepancy in output"|
                               reason.failure_19=="Discrepancy in output"|reason.failure_20=="Discrepancy in output","Discrepancy in output","")) %>%
  transform(reason2 = ifelse(reason.failure_01=="Bugs"|reason.failure_02=="Bugs"|
                               reason.failure_03=="Bugs"|reason.failure_04=="Bugs"|
                               reason.failure_05=="Bugs"|reason.failure_06=="Bugs"|
                               reason.failure_07=="Bugs"|reason.failure_08=="Bugs"|
                               reason.failure_09=="Bugs"|reason.failure_10=="Bugs"|
                               reason.failure_11=="Bugs"|reason.failure_12=="Bugs"|
                               reason.failure_13=="Bugs"|reason.failure_14=="Bugs"|
                               reason.failure_15=="Bugs"|reason.failure_16=="Bugs"|
                               reason.failure_17=="Bugs"|reason.failure_18=="Bugs"|
                               reason.failure_19=="Bugs"|reason.failure_20=="Bugs"|
                               reason.failure_01=="Bugs in code"|reason.failure_02=="Bugs in code"|
                               reason.failure_03=="Bugs in code"|reason.failure_04=="Bugs in code"|
                               reason.failure_05=="Bugs in code"|reason.failure_06=="Bugs in code"|
                               reason.failure_07=="Bugs in code"|reason.failure_08=="Bugs in code"|
                               reason.failure_09=="Bugs in code"|reason.failure_10=="Bugs in code"|
                               reason.failure_11=="Bugs in code"|reason.failure_12=="Bugs in code"|
                               reason.failure_13=="Bugs in code"|reason.failure_14=="Bugs in code"|
                               reason.failure_15=="Bugs in code"|reason.failure_16=="Bugs in code"|
                               reason.failure_17=="Bugs in code"|reason.failure_18=="Bugs in code"|
                               reason.failure_19=="Bugs in code"|reason.failure_20=="Bugs in code","Bugs in code","")) %>%
  transform(reason3 = ifelse(reason.failure_01=="Insufficient"|reason.failure_02=="Insufficient"|
                               reason.failure_03=="Insufficient"|reason.failure_04=="Insufficient"|
                               reason.failure_05=="Insufficient"|reason.failure_06=="Insufficient"|
                               reason.failure_07=="Insufficient"|reason.failure_08=="Insufficient"|
                               reason.failure_09=="Insufficient"|reason.failure_10=="Insufficient"|
                               reason.failure_11=="Insufficient"|reason.failure_12=="Insufficient"|
                               reason.failure_13=="Insufficient"|reason.failure_14=="Insufficient"|
                               reason.failure_15=="Insufficient"|reason.failure_16=="Insufficient"|
                               reason.failure_17=="Insufficient"|reason.failure_18=="Insufficient"|
                               reason.failure_19=="Insufficient"|reason.failure_20=="Insufficient"|
                               reason.failure_01=="Insufficient time available to replicator"|reason.failure_02=="Insufficient time available to replicator"|
                               reason.failure_03=="Insufficient time available to replicator"|reason.failure_04=="Insufficient time available to replicator"|
                               reason.failure_05=="Insufficient time available to replicator"|reason.failure_06=="Insufficient time available to replicator"|
                               reason.failure_07=="Insufficient time available to replicator"|reason.failure_08=="Insufficient time available to replicator"|
                               reason.failure_09=="Insufficient time available to replicator"|reason.failure_10=="Insufficient time available to replicator"|
                               reason.failure_11=="Insufficient time available to replicator"|reason.failure_12=="Insufficient time available to replicator"|
                               reason.failure_13=="Insufficient time available to replicator"|reason.failure_14=="Insufficient time available to replicator"|
                               reason.failure_15=="Insufficient time available to replicator"|reason.failure_16=="Insufficient time available to replicator"|
                               reason.failure_17=="Insufficient time available to replicator"|reason.failure_18=="Insufficient time available to replicator"|
                               reason.failure_19=="Insufficient time available to replicator"|reason.failure_20=="Insufficient time available to replicator","Insufficient time available to replicator","")) %>%
  transform(reason4 = ifelse(reason.failure_01=="Software"|reason.failure_02=="Software"|
                               reason.failure_03=="Software"|reason.failure_04=="Software"|
                               reason.failure_05=="Software"|reason.failure_06=="Software"|
                               reason.failure_07=="Software"|reason.failure_08=="Software"|
                               reason.failure_09=="Software"|reason.failure_10=="Software"|
                               reason.failure_11=="Software"|reason.failure_12=="Software"|
                               reason.failure_13=="Software"|reason.failure_14=="Software"|
                               reason.failure_15=="Software"|reason.failure_16=="Software"|
                               reason.failure_17=="Software"|reason.failure_18=="Software"|
                               reason.failure_19=="Software"|reason.failure_20=="Software"|
                               reason.failure_01=="Software not available to replicator"|reason.failure_02=="Software not available to replicator"|
                               reason.failure_03=="Software not available to replicator"|reason.failure_04=="Software not available to replicator"|
                               reason.failure_05=="Software not available to replicator"|reason.failure_06=="Software not available to replicator"|
                               reason.failure_07=="Software not available to replicator"|reason.failure_08=="Software not available to replicator"|
                               reason.failure_09=="Software not available to replicator"|reason.failure_10=="Software not available to replicator"|
                               reason.failure_11=="Software not available to replicator"|reason.failure_12=="Software not available to replicator"|
                               reason.failure_13=="Software not available to replicator"|reason.failure_14=="Software not available to replicator"|
                               reason.failure_15=="Software not available to replicator"|reason.failure_16=="Software not available to replicator"|
                               reason.failure_17=="Software not available to replicator"|reason.failure_18=="Software not available to replicator"|
                               reason.failure_19=="Software not available to replicator"|reason.failure_20=="Software not available to replicator","Software not available to replicator","")) %>%
  transform(reason5 = ifelse(reason.failure_01=="functional"|reason.failure_02=="functional"|
                               reason.failure_03=="functional"|reason.failure_04=="functional"|
                               reason.failure_05=="functional"|reason.failure_06=="functional"|
                               reason.failure_07=="functional"|reason.failure_08=="functional"|
                               reason.failure_09=="functional"|reason.failure_10=="functional"|
                               reason.failure_11=="functional"|reason.failure_12=="functional"|
                               reason.failure_13=="functional"|reason.failure_14=="functional"|
                               reason.failure_15=="functional"|reason.failure_16=="functional"|
                               reason.failure_17=="functional"|reason.failure_18=="functional"|
                               reason.failure_19=="functional"|reason.failure_20=="functional"|
                               reason.failure_01=="Code not functional"|reason.failure_02=="Code not functional"|
                               reason.failure_03=="Code not functional"|reason.failure_04=="Code not functional"|
                               reason.failure_05=="Code not functional"|reason.failure_06=="Code not functional"|
                               reason.failure_07=="Code not functional"|reason.failure_08=="Code not functional"|
                               reason.failure_09=="Code not functional"|reason.failure_10=="Code not functional"|
                               reason.failure_11=="Code not functional"|reason.failure_12=="Code not functional"|
                               reason.failure_13=="Code not functional"|reason.failure_14=="Code not functional"|
                               reason.failure_15=="Code not functional"|reason.failure_16=="Code not functional"|
                               reason.failure_17=="Code not functional"|reason.failure_18=="Code not functional"|
                               reason.failure_19=="Code not functional"|reason.failure_20=="Code not functional","Code not functional","")) %>%
  transform(reason6 = ifelse(reason.failure_01=="Data"&reason.failure_02=="not"|reason.failure_02=="Data"&reason.failure_03=="not"|
                               reason.failure_03=="Data"&reason.failure_04=="not"|reason.failure_04=="Data"&reason.failure_05=="not"|
                               reason.failure_05=="Data"&reason.failure_06=="not"|reason.failure_06=="Data"&reason.failure_07=="not"|
                               reason.failure_07=="Data"&reason.failure_08=="not"|reason.failure_08=="Data"&reason.failure_09=="not"|
                               reason.failure_09=="Data"&reason.failure_10=="not"|reason.failure_10=="Data"&reason.failure_11=="not"|
                               reason.failure_11=="Data"&reason.failure_12=="not"|reason.failure_12=="Data"&reason.failure_13=="not"|
                               reason.failure_13=="Data"&reason.failure_14=="not"|reason.failure_14=="Data"&reason.failure_15=="not"|
                               reason.failure_15=="Data"&reason.failure_16=="not"|reason.failure_16=="Data"&reason.failure_17=="not"|
                               reason.failure_17=="Data"&reason.failure_18=="not"|reason.failure_18=="Data"&reason.failure_19=="not"|
                               reason.failure_19=="Data"&reason.failure_20=="not"|
                               reason.failure_01=="Data not available"|reason.failure_02=="Data not available"|
                               reason.failure_03=="Data not available"|reason.failure_04=="Data not available"|
                               reason.failure_05=="Data not available"|reason.failure_06=="Data not available"|
                               reason.failure_07=="Data not available"|reason.failure_08=="Data not available"|
                               reason.failure_09=="Data not available"|reason.failure_10=="Data not available"|
                               reason.failure_11=="Data not available"|reason.failure_12=="Data not available"|
                               reason.failure_13=="Data not available"|reason.failure_14=="Data not available"|
                               reason.failure_15=="Data not available"|reason.failure_16=="Data not available"|
                               reason.failure_17=="Data not available"|reason.failure_18=="Data not available"|
                               reason.failure_19=="Data not available"|reason.failure_20=="Data not available","Data not available","")) %>%
  transform(reason7 = ifelse(reason.failure_01=="Data"&reason.failure_02=="missing"|reason.failure_02=="Data"&reason.failure_03=="missing"|
                               reason.failure_03=="Data"&reason.failure_04=="missing"|reason.failure_04=="Data"&reason.failure_05=="missing"|
                               reason.failure_05=="Data"&reason.failure_06=="missing"|reason.failure_06=="Data"&reason.failure_07=="missing"|
                               reason.failure_07=="Data"&reason.failure_08=="missing"|reason.failure_08=="Data"&reason.failure_09=="missing"|
                               reason.failure_09=="Data"&reason.failure_10=="missing"|reason.failure_10=="Data"&reason.failure_11=="missing"|
                               reason.failure_11=="Data"&reason.failure_12=="missing"|reason.failure_12=="Data"&reason.failure_13=="missing"|
                               reason.failure_13=="Data"&reason.failure_14=="missing"|reason.failure_14=="Data"&reason.failure_15=="missing"|
                               reason.failure_15=="Data"&reason.failure_16=="missing"|reason.failure_16=="Data"&reason.failure_17=="missing"|
                               reason.failure_17=="Data"&reason.failure_18=="missing"|reason.failure_18=="Data"&reason.failure_19=="missing"|
                               reason.failure_19=="Data"&reason.failure_20=="missing"|
                               reason.failure_01=="Data missing"|reason.failure_02=="Data missing"|
                               reason.failure_03=="Data missing"|reason.failure_04=="Data missing"|
                               reason.failure_05=="Data missing"|reason.failure_06=="Data missing"|
                               reason.failure_07=="Data missing"|reason.failure_08=="Data missing"|
                               reason.failure_09=="Data missing"|reason.failure_10=="Data missing"|
                               reason.failure_11=="Data missing"|reason.failure_12=="Data missing"|
                               reason.failure_13=="Data missing"|reason.failure_14=="Data missing"|
                               reason.failure_15=="Data missing"|reason.failure_16=="Data missing"|
                               reason.failure_17=="Data missing"|reason.failure_18=="Data missing"|
                               reason.failure_19=="Data missing"|reason.failure_20=="Data missing","Data missing","")) %>%
  transform(reason8 = ifelse(reason.failure_01=="Code"&reason.failure_02=="missing"|reason.failure_02=="Code"&reason.failure_03=="missing"|
                               reason.failure_03=="Code"&reason.failure_04=="missing"|reason.failure_04=="Code"&reason.failure_05=="missing"|
                               reason.failure_05=="Code"&reason.failure_06=="missing"|reason.failure_06=="Code"&reason.failure_07=="missing"|
                               reason.failure_07=="Code"&reason.failure_08=="missing"|reason.failure_08=="Code"&reason.failure_09=="missing"|
                               reason.failure_09=="Code"&reason.failure_10=="missing"|reason.failure_10=="Code"&reason.failure_11=="missing"|
                               reason.failure_11=="Code"&reason.failure_12=="missing"|reason.failure_12=="Code"&reason.failure_13=="missing"|
                               reason.failure_13=="Code"&reason.failure_14=="missing"|reason.failure_14=="Code"&reason.failure_15=="missing"|
                               reason.failure_15=="Code"&reason.failure_16=="missing"|reason.failure_16=="Code"&reason.failure_17=="missing"|
                               reason.failure_17=="Code"&reason.failure_18=="missing"|reason.failure_18=="Code"&reason.failure_19=="missing"|
                               reason.failure_19=="Code"&reason.failure_20=="missing"|
                               reason.failure_01=="Code missing"|reason.failure_02=="Code missing"|
                               reason.failure_03=="Code missing"|reason.failure_04=="Code missing"|
                               reason.failure_05=="Code missing"|reason.failure_06=="Code missing"|
                               reason.failure_07=="Code missing"|reason.failure_08=="Code missing"|
                               reason.failure_09=="Code missing"|reason.failure_10=="Code missing"|
                               reason.failure_11=="Code missing"|reason.failure_12=="Code missing"|
                               reason.failure_13=="Code missing"|reason.failure_14=="Code missing"|
                               reason.failure_15=="Code missing"|reason.failure_16=="Code missing"|
                               reason.failure_17=="Code missing"|reason.failure_18=="Code missing"|
                               reason.failure_19=="Code missing"|reason.failure_20=="Code missing","Code missing","")) %>%
  select(-reason.failure_01,-reason.failure_02,-reason.failure_03,-reason.failure_04,-reason.failure_05,
         -reason.failure_06,-reason.failure_07,-reason.failure_08,-reason.failure_09,-reason.failure_10,
         -reason.failure_11,-reason.failure_12,-reason.failure_13,-reason.failure_14,-reason.failure_15,
         -reason.failure_16,-reason.failure_17,-reason.failure_18,-reason.failure_19,-reason.failure_20)

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


## Keep only variables needed

jira.conf <- jira.conf.raw %>%
  select(ticket,date_created,date_resolved,date_updated,mc_number,Journal,Status,
         Software.used_1,Software.used_2,Software.used_3,Software.used_4,received,
         status_change,external,subtask,Resolution,
         reason1,reason2,reason3,reason4,reason5,reason6,reason7,
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
