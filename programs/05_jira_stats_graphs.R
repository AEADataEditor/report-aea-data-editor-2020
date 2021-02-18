# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/11/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
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


# Read in data extracted from Jira, anonymized
jira.raw <- readRDS(file.path(jiraanon,"jira.anon.RDS")) 

# Read in data extracted from openICPSR,
icpsr <- read.csv(file.path(icpsrbase,"AEA Report Fedora.csv"), stringsAsFactors = FALSE)

# A list of non-issues, typically for information-only
jira.pyear <- jira.raw %>%
  cSplit("Changed.Fields",",")  %>%
  mutate(status_change = ifelse(Changed.Fields_1=="Status","Yes",ifelse(Changed.Fields_2=="Status","Yes",ifelse(Changed.Fields_3=="Status","Yes",ifelse(Changed.Fields_4=="Status","Yes","No"))))) %>%
  select(-Changed.Fields_1,-Changed.Fields_2,-Changed.Fields_3,-Changed.Fields_4) %>%
  cSplit("Software.used",",")  %>%
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
         -reason.failure_16,-reason.failure_17,-reason.failure_18,-reason.failure_19,-reason.failure_20) %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  filter(status_change=="Yes"|received=="Yes") %>%
  mutate(subtask_y=ifelse(is.na(subtask),"No",ifelse(subtask!="","Yes",""))) %>%
  filter(subtask_y=="No") %>%
  filter(Journal != "AEA P&P")  ## Removing all P&P


#### Break out of the issues
jira.issues.breakout <- jira.pyear %>%
  arrange(desc(row_number())) %>%
  group_by(ticket) %>%
  mutate(status_order =  row_number(), st = "Status") %>%
  mutate(new1 = paste(st, status_order, sep="")) %>%
  select(ticket,Status,new1) %>%
  pivot_wider(names_from = new1, values_from = "Status")

# identifying the list of received tickets
ji <- jira.pyear %>%
  select(ticket,Journal) %>%
  distinct(ticket, .keep_all = TRUE) 

# identifying the list of issues went through alternate workflow
ji_alt <- jira.pyear %>%
  filter(Status == "Alternate"|Status=="Alternate workflow") %>%
  select(ticket) %>%
  distinct() %>%
  mutate(alternate="Yes")

# identifying the list of submitted issues 
jis <- jira.pyear %>%
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") %>%
  select(ticket) %>%
  distinct() %>%
  mutate(submitted="Yes")

# figure out the final status of the issue as of 12/01/2020
jib <- jira.issues.breakout %>%
  left_join(jis,by="ticket") %>%
  transform(submitted=ifelse(is.na(submitted),"No",as.character(submitted))) %>%
  filter(submitted=="No") %>%
  select(-Status26,-Status27,-Status28,-Status29,-Status30,-Status31,-Status32,-Status33) %>%
  transform(final_status=ifelse(!is.na(Status25),as.character(Status25),
                                ifelse(!is.na(Status24),as.character(Status24),
                                       ifelse(!is.na(Status23),as.character(Status23),
                                              ifelse(!is.na(Status22),as.character(Status22),
                                                     ifelse(!is.na(Status21),as.character(Status21),
                                                            ifelse(!is.na(Status20),as.character(Status20),
                                                                   ifelse(!is.na(Status19),as.character(Status19),
                                                                          ifelse(!is.na(Status18),as.character(Status18),
                                                                                 ifelse(!is.na(Status17),as.character(Status17),
                                                                                        ifelse(!is.na(Status16),as.character(Status16),
                                                                                               ifelse(!is.na(Status15),as.character(Status15),
                                                                                                      ifelse(!is.na(Status14),as.character(Status14),
                                                                                                             ifelse(!is.na(Status13),as.character(Status13),
                                                                                                                    ifelse(!is.na(Status12),as.character(Status12),
                                                                                                                           ifelse(!is.na(Status11),as.character(Status11),
                                                                                                                                  ifelse(!is.na(Status10),as.character(Status10),
                                                                                                                                         ifelse(!is.na(Status9),as.character(Status9),
                                                                                                                                                ifelse(!is.na(Status8),as.character(Status8),
                                                                                                                                                       ifelse(!is.na(Status7),as.character(Status7),
                                                                                                                                                              ifelse(!is.na(Status6),as.character(Status6),
                                                                                                                                                                     ifelse(!is.na(Status5),as.character(Status5),
                                                                                                                                                                            ifelse(!is.na(Status4),as.character(Status4),
                                                                                                                                                                                   ifelse(!is.na(Status3),as.character(Status3),
                                                                                                                                                                                          ifelse(!is.na(Status2),as.character(Status2),
                                                                                                                                                                                                 ifelse(!is.na(Status1),as.character(Status1),"")))))))))))))))))))))))))) %>%
  select(ticket,final_status)

# categorize issues: P&P, Submitted, Not yet submitted, Alternate workflow, Others.
jira.issues.breakout <- jira.issues.breakout %>%  
  left_join(jis,by="ticket") %>%
  left_join(ji,by="ticket") %>%
  left_join(ji_alt,by="ticket") %>%
  transform(submitted=ifelse(is.na(submitted),"No",as.character(submitted))) %>%
  transform(alternate=ifelse(is.na(alternate),"No",as.character(alternate))) %>%
  select(ticket,Journal,submitted,alternate) %>%
  left_join(jib, by="ticket") %>%
  transform(outcome=ifelse(Journal=="AEA P&P","P&P",
                           ifelse(final_status=="Open"|final_status=="Assigned"|final_status=="In Progress"|final_status=="Report Under Review"|final_status=="Write Preliminary Report"|final_status=="Verification"|final_status=="Pre-Approved"|final_status=="Approved"|final_status=="Data"|final_status=="Waiting for info"|final_status=="Waiting for external report","Not yet submitted",
                                  ifelse(alternate=="Yes","Alternate","Others"))))  %>%
  transform(outcome=ifelse(submitted=="Yes","Submitted",as.character(outcome)))

# summarize the breakdown of the cases
summary.breakout <- jira.issues.breakout %>%  
  group_by(outcome) %>%
  summarise(n_outcome = n_distinct(ticket)) 

## separate the "Others" case
jira.others <- jira.issues.breakout %>%
  filter(outcome=="Others"|outcome=="Alternate") %>%
  select(ticket) %>%
  mutate(others="Yes")

## filter out the "others" case
jira.pyear <- jira.pyear %>%
  left_join(jira.others,by="ticket") %>%
  transform(others=ifelse(is.na(others),"No",as.character(others))) %>%
  filter(others=="No") 

# This filters for the cases **submitted** in the past 12 months
issues_all <- jira.pyear %>%
  select(ticket) %>%
  distinct() %>% nrow()

manuscript_all <- jira.pyear %>%
  select(mc_number_anon) %>% 
  distinct() %>% nrow()

# we used the start and end date here, so write them out
update_latexnums("firstday",firstday)
update_latexnums("lastday",lastday)
update_latexnums("jiraissues",issues_all)
update_latexnums("jiramcs",manuscript_all)

## By journal
issues_total_journal <- jira.pyear %>%
  filter(!(Journal=="")) %>%
  group_by(Journal) %>%
  summarise(issue_numbers = n_distinct(ticket),
            mcs_numbers   = n_distinct(mc_number_anon))

stargazer(issues_total_journal,style = "aer",
          summary = FALSE,
          out = file.path(tables,"issues_total_journal.tex"),
          out.header = FALSE,
          float = FALSE
)

#### Number of reports processed (went past submitted to MC) since December 1, 2019
## Total submitted records


jira.filter.submitted <- jira.pyear %>%  
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") 

assess_cplt <- jira.filter.submitted %>%
  select(ticket) %>% 
  distinct() %>%
  nrow()
update_latexnums("jiraissuescplt",assess_cplt)


manuscript_cplt <- jira.filter.submitted %>%
  select(mc_number_anon) %>% 
  distinct() %>% nrow()
update_latexnums("jiramcscplt",manuscript_cplt)


## By journal
assess_cplt_journal <- jira.filter.submitted %>%
  group_by(Journal) %>%
  summarise(issues_cplt = n_distinct(ticket),
            mcs_cplt    = n_distinct(mc_number_anon)) 

stargazer(assess_cplt_journal,style = "aer",
          summary = FALSE,
          out = file.path(tables,"assess_cplt_journal.tex"),
          out.header = FALSE,
          float = FALSE
          )

# Histogram
n_assessments_journal_plot <- ggplot(assess_cplt_journal, aes(x = Journal, y = issues_cplt)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of assessments", title = "Total assessments completed, by journal") + 
  theme_classic() +
  geom_text(aes(label=issues_cplt), hjust=1.3, size=3.5) +
  coord_flip()

ggsave(file.path(images,"n_assessments_journal_plot.png"), 
       n_assessments_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_assessments_journal_plot



#### Number of unique paper processed since December 1, 2019
## Total


# Histogram
n_unique_journal_plot <- ggplot(assess_cplt_journal, aes(x = Journal, y = mcs_cplt)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of papers", title = "Number of Papers, completed, by journal") + 
  theme_classic() +
  geom_text(aes(label=mcs_cplt), hjust=1.3, size=3.5) +
  coord_flip()


ggsave(file.path(images,"n_unique_journal_plot.png"), 
       n_unique_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_unique_journal_plot





#### Number of assessments/manuscript that are pending publication

jira.filter.pending <- jira.pyear %>%
  filter(Status == "Pending publication")

manuscript_pending <- jira.filter.pending %>%
  select(mc_number_anon) %>% 
  distinct() %>% nrow()
update_latexnums("jiramcspending",manuscript_pending)


## By journal
pendingpub_by_journal <- jira.filter.pending %>%
  group_by(Journal) %>%
  summarise(mcs_pendingpub   = n_distinct(mc_number_anon))

stargazer(pendingpub_by_journal,style = "aer",
          summary = FALSE,
          out = file.path(tables,"pendingpub_by_journal.tex"),
          out.header = FALSE,
          float = FALSE
)



#### Number of assessment processed by external replicator since December 1, 2019
## Total

external_total <- jira.filter.submitted %>%
  filter(external == "Yes") %>%
  select(ticket) %>% distinct() %>%
  nrow()
update_latexnums("jiraexternal",external_total)
mcs_external <- jira.filter.submitted %>%
  filter(external == "Yes") %>%
  select(mc_number_anon) %>% distinct() %>%
  nrow()
update_latexnums("jiramcsexternal",mcs_external)

## By journal
external_total_journal <- jira.filter.submitted %>%
  filter(external == "Yes") %>%
  group_by(Journal) %>%
  summarise(mcs_external = n_distinct(mc_number_anon),
            issues_external=n_distinct(ticket))

# output table
stargazer(external_total_journal,style = "aer",
          summary = FALSE,
          out = file.path(tables,"external_total_journal.tex"),
          out.header = FALSE,
          float = FALSE
)
# Histogram
n_external_journal_plot <- ggplot(external_total_journal, aes(x = Journal, y = issues_external)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of cases processed by external replicator", title = "Total usage of external replicators by journal") + 
  theme_classic() +
  geom_text(aes(label=issues_external), hjust=1.5, size=3.5) +
  coord_flip()


ggsave(file.path(images,"n_external_journal_plot.png"), 
       n_external_journal_plot  +
         labs(y=element_blank(),title=element_blank()))

n_external_journal_plot


### Combine five data columns

n_journal_table <- full_join(issues_total_journal,assess_cplt_journal,by=c("Journal")) %>%
                   full_join(external_total_journal,by=c("Journal")) %>%
                   full_join(pendingpub_by_journal,by=c("Journal")) %>%
                   mutate(Journal = if_else(Journal=="AEA P&P","AEA P+P",Journal)) %>%
                   select(Journal,"Issues (rcvd)" = issue_numbers,
                          "Issues (cplt)" = issues_cplt,
                          "Issues (external)" = issues_external,
                          "Manuscripts (rcvd)" = mcs_numbers,
                          "Manuscripts (cplt)" = mcs_cplt,
                          "Manuscripts (ext.)" = mcs_external,
                          "Manuscripts (pend.)"= mcs_pendingpub)

# output table
stargazer(n_journal_table,style = "aer",
          summary = FALSE,
          out = file.path(tables,"n_journal_numbers.tex"),
          out.header = FALSE,
          float = FALSE,
          rownames = FALSE
)


#### Length of an assessment rounds (initial submission to us, and filing to Manuscript Central). 
# This is the duration of each Jira ticket.
# Uses start date and date of status update after restricting to the cases "Submitted to MC"

duration.data <- jira.filter.submitted  %>%
  mutate(length=difftime(date_updated,date_created,units="days")) %>%
  arrange(length) %>% ungroup()
   
table.duration <- duration.data %>% group_by(length) %>%
  summarise(n_tickets = n_distinct(ticket))
mode <- table.duration %>% filter(n_tickets == max(n_tickets)) %>% select(n_tickets)
mode.days <- table.duration %>% filter(n_tickets == max(n_tickets)) %>% select(length) %>% as.numeric()
#------
# Histogram
#geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+

revision_round_length <- ggplot(duration.data, aes(x = length)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  geom_density(alpha=.2, fill = "black", col="black") +
  geom_vline(aes(xintercept=mode.days),
             linetype="dashed", size=0.5) +
  annotate("text",x=as.numeric(mode.days)+20,y=0.03,
              label=paste("Mode:",mode.days,"days"),size=3.5) +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Days", y = "Density", title = "Length of revision rounds") 

revision_round_length.bar <- ggplot(table.duration, aes(x = length,y=n_tickets)) +
                          geom_bar(stat = "identity") +
                          theme_classic() +
                          scale_colour_brewer(palette = "Paired") +
                          labs(x = "Days", y = "Density", title = "Length of revision rounds") 

revision_round_length.pos <- ggplot(duration.data %>% filter(length >= 1), aes(x = length)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  geom_density(alpha=.2, fill = "black", col="black") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Days", y = "Density", title = "Length of revision rounds (no zeros)") 

ggsave(file.path(images,"revision_round_length_hist.png"), 
       revision_round_length +
         labs(y=element_blank(),title=element_blank()))

ggsave(file.path(images,"revision_round_length_hist2.png"), 
       revision_round_length.bar +
         labs(y=element_blank(),title=element_blank()))

ggsave(file.path(images,"revision_round_length_hist_pos.png"), 
       revision_round_length.pos +
         labs(y=element_blank(),title=element_blank()))

revision_round_length
revision_round_length.bar
revision_round_length.pos



#### Length of complete cycle time (initial submission to us, and pending publication).
# Uses start date and resolution date after restricting to the cases to "pending publications"
#
#  THIS IS NOT RELIABLE - many of the older issues were not transitioned 
#                         to "Pending openICPSR" nor to "Pending publication"

cycle.data <- jira.pyear %>%
  filter(Status == "Pending publication") %>%
  mutate(length=difftime(date_updated,date_created,units="days")) %>%
  arrange(length) %>% ungroup()

table.cycle <- cycle.data %>% group_by(length) %>%
  summarise(n_tickets = n_distinct(ticket))
#------
# Histogram
#geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+

cycle_round_length <- ggplot(table.cycle, aes(x = length)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  geom_density(alpha=.2, fill = "black", col="black") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Days", y = "Density", title = "Length of complete cycle") 

cycle_round_length.bar <- ggplot(table.cycle, aes(x = length,y=n_tickets)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Days", y = "Density", title = "Length of complete cycle") 

cycle_round_length.pos <- ggplot(table.cycle %>% filter(length >= 1), aes(x = length)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  geom_density(alpha=.2, fill = "black", col="black") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Days", y = "Density", title = "Length of complete cycle (no zeros)") 

ggsave(file.path(images,"cycle_length_hist.png"), 
       cycle_round_length +
         labs(y=element_blank(),title=element_blank()))

ggsave(file.path(images,"cycle_length_hist2.png"), 
       cycle_round_length.bar +
         labs(y=element_blank(),title=element_blank()))

ggsave(file.path(images,"cycle_length_hist_pos.png"), 
       cycle_round_length.pos +
         labs(y=element_blank(),title=element_blank()))

cycle_round_length
cycle_round_length.bar
cycle_round_length.pos


#### Number of cases using different statistical packages
software_used <- jira.filter.submitted %>%
  transform(Software.used_1 = ifelse(Software.used_1 == "", "None",as.character(Software.used_1))) %>%
  filter(Journal != "") %>%
  select(mc_number_anon, Software.used_1, Software.used_2, Software.used_3, Software.used_4) %>%
  distinct() 

plot_software_used <- software_used %>%
  pivot_longer(!mc_number_anon, names_to = "n", values_to = "software") %>%
  filter(software!="") %>%
  filter(software != "macbook"& software != "MacBook"& software!= "texstudio"&software!= "Visual_Studio"& software!="LATEX"&software!="Linux"&software!="CISER"&software!="IRIS_Toolbox") %>%
  select(!n) %>%
  transform(software = ifelse(software == "r", "R",as.character(software))) %>%
  transform(software = ifelse(software == "Java", "Javascript", as.character(software))) %>%
  transform(software = ifelse(software == "stata", "Stata", as.character(software))) %>%
  transform(software = ifelse(software == "matlab", "Matlab", as.character(software))) %>%
  mutate(software_used = ifelse(software =="BOCOP"|software =="COMPAS"|software =="Eviews"|software =="Gurobi"|software =="Javascript"|software =="Knitro"|software =="Occbin"|software =="Octave"|software =="openmp"|software =="Jupiter"|software =="Gauss"|software =="oTree"|software =="Perl"|software =="SQL"|software =="zTree","Others",ifelse(software=="ArcGIS"|software=="MapBasic"|software=="QGIS","GIS",as.character(software)))) %>% 
  group_by(software_used) %>%
  summarise(n_papers = n_distinct(mc_number_anon))

# Histogram
n_software_used_plot <- ggplot(plot_software_used, aes(x = software_used, y = n_papers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Statistical Packages", y = "Number of papers", title = "Number of papers by statistical packages used") + 
  theme_classic() +
  coord_flip()

ggsave(file.path(images,"n_software_used_plot.png"), 
       n_software_used_plot  +
         labs(y=element_blank(),title=element_blank()))
n_software_used_plot



#### Number of submitted cases by different reasons of failure to replicate
## 
##  Why does this not have "Code missing"?
## 
reasons_failure <- jira.filter.submitted %>%
  select(ticket, reason1, reason2, reason3, reason4, reason5, reason6, reason7) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  pivot_longer(!ticket, names_to = "n", values_to = "reason") %>%
  filter(reason!="") %>%
  select(-n)

summary_reasons <- reasons_failure %>%
  group_by(reason) %>%
  summarise(n_issues = n_distinct(ticket))

# Histogram
n_reasons_plot <- ggplot(summary_reasons, aes(x = reason, y = n_issues)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Reason for failure to replicate", y = "Number of issues", title = "Number of issues by reasons for failure") + 
  theme_classic() +
  coord_flip()

ggsave(file.path(images,"n_reasons_plot.png"), 
       n_reasons_plot  +
         labs(y=element_blank(),title=element_blank()))
n_reasons_plot




#### Number of submitted cases by MC recommendations
## Only for Conditional Accept

recommendation <- jira.filter.submitted %>%
  select(ticket, MCRecommendationV2,Journal,date_updated) %>%
  filter(MCRecommendationV2!="") %>%
  filter(!ticket %in% c("AEAREP-924","AEAREP-758","AEAREP-710","AEAREP-701","AEAREP-502","AEAREP-470","AEAREP-454","AEAREP-451","AEAREP-405")) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  transform(mcrec=as.character(MCRecommendationV2),
            yq=paste(year(date_updated),quarter(date_updated),sep="Q")) %>%
  select(ticket,mcrec,Journal,date_updated,yq)

summary_recommendation <- recommendation %>%
  group_by(mcrec) %>%
  summarise(n_issues = n_distinct(ticket))

# Histogram
n_recommendation_plot <- ggplot(summary_recommendation, aes(x = mcrec, y = n_issues)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Recommendation", y = "Number of issues", title = "Number of issues by recommendation") + 
  theme_classic() +
  coord_flip()

n_recommendation_plot_evo <- ggplot(recommendation %>% filter(yq > "2019Q4"), aes(x = yq)) +
  geom_bar(aes(fill=mcrec),position="fill",colour="white") +
  labs(x = "Recommendation", y = "Number of issues", title = "Number of issues by recommendation") + 
  theme_classic() +
  coord_flip()


ggsave(file.path(images,"n_recommendation_plot.png"), 
       n_recommendation_plot  +
         labs(y=element_blank(),title=element_blank()))
n_recommendation_plot

ggsave(file.path(images,"n_recommendation_plot_evo.png"), 
       n_recommendation_plot_evo  +
         labs(y=element_blank(),title=element_blank()))


#### Length of author responses (time between filing report of first assessment, and getting the next submission)
 author_length <- jira.pyear %>%
  filter(mc_number_anon != "",
         ticket != mc_number_anon,
         !grepl("#",mc_number_anon, fixed = TRUE)) %>%
  filter(Status=="Open"| Status=="Submitted to MC") %>%
  select(ticket, mc_number_anon, date_created, date_updated,Status, Journal) %>%
  distinct() 
  
al1 <- author_length %>%
  filter(Status=="Submitted to MC") %>%
  arrange(ticket, date_updated, Status) %>%
  group_by(ticket) %>%
  mutate(status_order =  row_number(), st = "Status", st2 = "date_updated") %>%
  mutate(new1 = paste(st, status_order, sep="")) %>%
  mutate(new2 = paste(st2, status_order, sep="")) %>%
  select(ticket,new1,Status) %>%
  pivot_wider(names_from = new1, values_from = "Status") %>%
  rename(Status=Status1) %>%
  select(!Status2)

  
al2 <- author_length %>%
  filter(Status=="Submitted to MC") %>%
  arrange(ticket, date_updated, Status) %>%
  group_by(ticket) %>%
  mutate(status_order =  row_number(), st = "Status", st2 = "date_updated") %>%
  mutate(new1 = paste(st, status_order, sep="")) %>%
  mutate(new2 = paste(st2, status_order, sep="")) %>%
    select(ticket,new2,date_updated) %>%
  pivot_wider(names_from = new2, values_from = "date_updated")%>%
  rename(date_updated=date_updated1) %>%
  select(!date_updated2)

  
author_length <- author_length %>%
  select(ticket,mc_number_anon, date_created, Journal) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  left_join(al1,by="ticket") %>%
  left_join(al2,by="ticket") %>%
  group_by(mc_number_anon) %>% 
  arrange(mc_number_anon,date_created) %>%
  mutate(rounds_tot = n_distinct(ticket), 
         round =  row_number(),
         round_start = date_created,
        round_end = date_updated,
        round_end_previous = lag(round_end), 
        length=difftime(round_start,round_end_previous,units="days")) %>%
  ungroup 
  
  
# Histogram
#geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  
  author_response_length <- ggplot(author_length, aes(x = length)) +
    geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
    geom_density(alpha=.2, fill = "black", col="black") +
    theme_classic() +
    scale_colour_brewer(palette = "Paired") +
    labs(x = "Days", y = "Density", title = "Length of author responses") 
  
  ggsave(file.path(images,"author_response_length.png"), 
         author_response_length +
           labs(y=element_blank(),title=element_blank()))
  
  author_response_length

  
#### Distribution of rounds each paper goes through
## THIS NEEDS DATA FROM BEFORE DEC 2019 - jira.pyear is not the right data.
## Total

n_rounds <- jira.pyear %>%
  mutate(completed = ifelse(grepl("Pending publication", Status, fixed = TRUE) == TRUE, 1, 0)) %>%
  group_by(mc_number_anon) %>%
  mutate(paper_completed = max(completed)) %>%
  ungroup %>%
  filter(paper_completed == 1) %>%
  filter(Status == "Open") %>%
  arrange(mc_number_anon,date_created) %>%
  group_by(mc_number_anon) %>%
  mutate(rounds_tot = n_distinct(ticket), 
         round =  row_number()) %>%
  select(-completed, -paper_completed) %>%
  ungroup %>%
  group_by(rounds_tot) %>%
  summarise(n_papers = n_distinct(mc_number_anon)) %>%
  ungroup %>%
  mutate(total = sum(n_papers),
         frac_papers = n_papers/total)

n_rounds_plot <- ggplot(n_rounds, aes(x = rounds_tot, y = frac_papers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Rounds", y = "Fraction of papers", title = "Distribution of review rounds")

ggsave(file.path(images,"n_rounds_plot.png"), 
       n_rounds_plot  +
         labs(y=element_blank(),title=element_blank()))
n_rounds_plot

## By journal
n_rounds_journal <- jira.pyear %>%
  mutate(completed = ifelse(grepl("Pending publication", Status, fixed = TRUE) == TRUE, 1, 0)) %>%
  group_by(mc_number_anon) %>%
  mutate(paper_completed = max(completed)) %>%
  ungroup %>%
  filter(paper_completed == 1) %>%
  filter(Status == "Open") %>%
  arrange(mc_number_anon,date_created) %>%
  group_by(mc_number_anon) %>%
  mutate(rounds_tot = n_distinct(ticket), 
         round =  row_number()) %>%
  select(-completed, -paper_completed) %>%
  group_by(Journal, rounds_tot) %>%
  summarise(n_papers = n_distinct(mc_number_anon)) %>%
  filter(Journal != "") %>%
  group_by(Journal) %>%
  mutate(total = sum(n_papers),
         frac_papers = n_papers/total) %>%
  ungroup


n_rounds_journal_plot <- ggplot(n_rounds_journal, aes(x = rounds_tot, y = frac_papers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Rounds", y = "Fraction of papers", title = "Distribution of review rounds by journal") + 
  facet_grid(Journal ~ .) +
  coord_flip()

ggsave(file.path(images,"n_rounds_journal_plot.png"), 
       n_rounds_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_rounds_journal_plot


## response options
jira.response.options <- jira.filter.submitted  %>%  
  distinct(mc_number_anon, .keep_all=TRUE) %>%
  unite(response,c(MCRecommendationV2,MCRecommendation),remove=FALSE,sep="") %>%
  group_by(response) %>%
  summarise(freq=n_distinct(mc_number_anon))

## Distribution of replication packages
dist_size <- icpsr %>% 
  cSplit("Created.Date","T") %>%
  mutate(date_created=as.Date(substr(Created.Date_1, 1,10), "%Y-%m-%d")) %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  transform(filesize=Total.File.Size/(1024^2)) %>%
  transform(filesize=ifelse(filesize>2048,2048,filesize))
  

plot_filesize_dist <- ggplot(dist_size, aes(x = filesize)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 50)+
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "File Size (MB)", y = "Density", title = "Size distribution of replication packages") 

ggsave(file.path(images,"plot_filesize_dist.png"), 
       plot_filesize_dist  +
         labs(y=element_blank(),title=element_blank()))
plot_filesize_dist
