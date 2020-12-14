# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/11/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("ggplot2","dplyr","here","tidyr","tibble","devtools")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")

## Non-standard - install of a page with same name
#pkgTest.github("stargazer-booktabs","markwestcott34")
install_github("markwestcott34/stargazer-booktabs")
library(stargazer)


# Read in data extracted from Jira, anonymized
jira.raw <- readRDS(file.path(jiraanon,"jira.anon.RDS")) 

# A list of non-issues, typically for information-only

# This filters for the cases **submitted** in the past 12 months
jira.pyear <- jira.raw %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  filter(status_change=="Yes"|received=="Yes") %>%
  mutate(subtask_y=ifelse(is.na(subtask),"No",ifelse(subtask!="","Yes",""))) %>%
  filter(subtask_y=="No") %>%
  filter(ticket!="AEAREP-1589") ## Explain!

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
  theme(axis.text.x = element_text(angle=45))

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
  theme(axis.text.x = element_text(angle=45))


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
  theme(axis.text.x = element_text(angle=45))


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
          float = FALSE
)
#### Length of an assessment rounds (initial submission to us, and filing to Manuscript Central). 
# This is the duration of each Jira ticket.
# Uses start date and date of status update after restricting to the cases "Submitted to MC"

duration.data <- jira.filter.submitted  %>%
  mutate(length=difftime(date_updated,date_created,units="days")) %>%
  arrange(length) %>% ungroup()
   
table.duration <- duration.data %>% group_by(length) %>%
  summarise(n_tickets = n_distinct(ticket))
#------
# Histogram
#geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+

revision_round_length <- ggplot(duration.data, aes(x = length)) +
  geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+
  geom_density(alpha=.2, fill = "black", col="black") +
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
  theme(axis.text.x = element_text(angle=45))

ggsave(file.path(images,"n_software_used_plot.png"), 
       n_software_used_plot  +
         labs(y=element_blank(),title=element_blank()))
n_software_used_plot



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
        length=difftime(round_start,round_end_previous,units="days"))
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
  theme(axis.text.x = element_text(angle=45))

ggsave(file.path(images,"n_rounds_journal_plot.png"), 
       n_rounds_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_rounds_journal_plot




