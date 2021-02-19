# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 2/18/2021

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
#   - file.path(temp,"jira.others.RDS)
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("ggplot2","dplyr","here","tidyr","tibble","remotes","splitstackshape")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")

## Non-standard - install of a page with same name
#pkgTest.github("stargazer-booktabs","markwestcott34")
install_github("markwestcott34/stargazer-booktabs")
library(stargazer)

# Get an intermediate file
jira.others <- readRDS(file.path(temp,"jira.others.RDS"))

# Read in data extracted from Jira, anonymized
jira.publish <- readRDS(file.path(jiraanon,"jira.anon.RDS"))  %>%
  cSplit("Changed.Fields",",")  %>%
  mutate(status_change = ifelse(Changed.Fields_1=="Status","Yes",ifelse(Changed.Fields_2=="Status","Yes",ifelse(Changed.Fields_3=="Status","Yes",ifelse(Changed.Fields_4=="Status","Yes","No"))))) %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  filter(status_change=="Yes"|received=="Yes") %>%
  mutate(subtask_y=ifelse(is.na(subtask),"No",ifelse(subtask!="","Yes",""))) %>%
  filter(subtask_y=="No") %>%
  filter(Journal != "") %>%
  left_join(jira.others,by="ticket") %>%
  transform(others=ifelse(is.na(others),"No",as.character(others))) %>%
  filter(others=="No") %>%
  transform(pending_pub = ifelse(Status=="Pending publication",1,0),
            done = ifelse(Status=="Done",1,0)) %>%
  group_by(mc_number_anon) %>%
  mutate(pending_pub = max(pending_pub), 
            done = max(done)) %>%
  select(ticket, mc_number_anon,Journal,pending_pub, done) %>%
  filter(pending_pub==1&done==1) %>%
  distinct(mc_number_anon, .keep_all=TRUE)  %>%
  ungroup %>%
  mutate(journal_group = ifelse(Journal=="AEA P&P","Papers and Proceedings","AER and journals")) %>%
  group_by(journal_group) %>%
  summarise(issues = n_distinct(mc_number_anon)) 

stargazer(jira.publish,style = "aer",
          summary = FALSE,
          out = file.path(tables,"n_published_manuscript.tex"),
          out.header = FALSE,
          float = FALSE
)

  