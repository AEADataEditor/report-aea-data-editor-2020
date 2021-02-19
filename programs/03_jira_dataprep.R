# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/11/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("dplyr","here","tidyr","tibble","splitstackshape")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")


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
# we will need it again

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

## Total submitted records


jira.filter.submitted <- jira.pyear %>%  
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") 

# we will re-use these 
saveRDS(jira.pyear,file=file.path(temp,"jira.pyear.RDS"))
saveRDS(jira.filter.submitted,file=file.path(temp,"jira.submitted.RDS"))
saveRDS(jira.others,file=file.path(temp,"jira.others.RDS"))


# we used the start and end date here, so write them out
update_latexnums("firstday",firstday)
update_latexnums("lastday",lastday)
update_latexnums("jiraissues",issues_all)
update_latexnums("jiramcs",manuscript_all)

