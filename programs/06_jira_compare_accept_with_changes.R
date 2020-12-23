# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/22/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("ggplot2","dplyr","here","tidyr","tibble","devtools","hrbrthemes")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")

## Non-standard - install of a page with same name
#pkgTest.github("stargazer-booktabs","markwestcott34")
install_github("markwestcott34/stargazer-booktabs")
library(stargazer)


# Read in data extracted from Jira, anonymized
jira.raw <- readRDS(file.path(jiraanon,"jira.anon.RDS")) 

# A list of non-issues, typically for information-only 
## Select issues created in July-December period of 2019 and of 2020.
jira.pyear <- jira.raw %>%
  filter(date_created >= "2019-07-01"&date_created < "2020-01-01"|
           date_created >= "2020-07-01"&date_created < lastday) %>%
  filter(status_change=="Yes"|received=="Yes") %>%
  mutate(subtask_y=ifelse(is.na(subtask),"No",ifelse(subtask!="","Yes",""))) %>%
  filter(subtask_y=="No") %>%
  filter(ticket!="AEAREP-1589") ## Decision notice of aearep-1523


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

## Total submitted records
recommendation <- jira.pyear %>%  
  filter(mc_number_anon != "",
         ticket != mc_number_anon,
         !grepl("#",mc_number_anon, fixed = TRUE)) %>%
  filter(Status=="Submitted to MC"& Journal != "AEA P&P")  %>%
  transform(mcrec=ifelse(MCRecommendationV2!="",as.character((MCRecommendationV2)),
                         ifelse(MCRecommendation!="",as.character(MCRecommendation),""))) %>%
  select(ticket,mc_number_anon,mcrec,date_created,date_updated,Journal,Status) %>%
  mutate(length_issue = difftime(date_updated,date_created,units="days"),
         awc=ifelse(mcrec=="Accept - with Changes",1,0),
         y_created = ifelse(date_created >= "2019-07-01"&date_created < "2020-01-01","2019",
                            ifelse(date_created >= "2020-07-01"&date_created < lastday,"2020",""))) %>%
  group_by(mc_number_anon) %>%
  mutate(paper_initial = min(date_created),
         paper_last = max(date_updated),
         rounds = n_distinct(ticket),
         length_total = difftime(paper_last,paper_initial,units="days"),
         avg_length_issue = mean(length_issue),
         accept_with_change = max(awc)) %>%
  distinct(mc_number_anon, .keep_all = TRUE) %>%
  select(-awc, -length_issue)
  ungroup

### Summary
summary.recommendation <- recommendation %>%
  group_by(y_created) %>%
  summarise(n_papers = n_distinct(mc_number_anon),
            n_rounds = mean(rounds),
            mean_length_paper = mean(length_total),
            median_length_paper = median(length_total),
            mean_length_issue = mean(avg_length_issue))

  
### histogram
 plot_length_total <- ggplot(recommendation, aes(x = length_total,color=y_created, group=y_created,fill=y_created)) +
    geom_histogram(position="dodge",aes(y=..density..), colour="white", binwidth = 5)+
    geom_density(alpha=.2, col="black") +
    theme_classic() +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    labs(x = "Days", y = "Density", title = "Length of processing time for a paper") 
  
ggsave(file.path(images,"plot_length_total.png"), 
       plot_length_total +
         labs(y=element_blank(),title=element_blank()))

plot_length_total
