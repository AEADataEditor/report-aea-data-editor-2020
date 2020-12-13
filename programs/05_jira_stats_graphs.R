# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/11/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_unique_journal_plot.png"),
#   - ggsave(file.path(images,"n_external_journal_plot.png"),
#   - ggsave(file.path(images,"revision_round_length_hist.png"), 
#   - ggsave(file.path(images,"revision_round_length_hist2.png"), 
#   - ggsave(file.path(images,"revision_round_length_hist_pos.png"),
#   - ggsave(file.path(images,"cycle_length_hist.png"), 
#   - ggsave(file.path(images,"cycle_length_hist2.png"), 
#   - ggsave(file.path(images,"cycle_length_hist_pos.png"), 
#   - ggsave(file.path(images,"n_software_used_plot.png"), 
#   - ggsave(file.path(images,"n_rounds_plot.png"), 
#   - ggsave(file.path(images,"n_rounds_journal_plot.png"), 

#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"),

#   - ggsave(file.path(images,"author_response_hist.png"), 
#   - ggsave(file.path(images,"total_length_hist.png"), 

### Cleans working environment.
rm(list = ls())
gc()

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("ggplot2","dplyr","reshape2","here","tidyr")
results <- sapply(as.list(global.libraries), pkgTest)

pkgTest.github("data.table","Rdatatable")


# Read in data extracted from Jira, anonymized
jira <- readRDS(file.path(jiraanon,"jira.anon.RDS")) %>%
  filter(date_created >= "2019-12-01") %>%
  filter(date_created < "2020-12-01") %>%
  filter(status_change=="Yes"|received=="Yes")
  

#### Number of reports processed (went past submitted to MC) since December 1, 2019
## Total
jira.assess<- jira %>% 
  filter(Status == "Submitted to MC") %>%
  select(ticket) %>% distinct() 

assess_total <- nrow(jira.assess)

## By journal
assess_total_journal <- jira %>%
  filter(Status == "Submitted to MC") %>%
  group_by(Journal) %>%
  summarise(assess_numbers = n_distinct(ticket)) 

# Histogram
n_assessments_journal_plot <- ggplot(assess_total_journal, aes(x = Journal, y = assess_numbers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of assessments", title = "Total assessments by journal") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))

ggsave(file.path(images,"n_assessments_journal_plot.png"), 
       n_assessments_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_assessments_journal_plot


#### Number of unique paper processed since December 1, 2019
## Total
jira.manuscripts<- jira %>% 
  filter(Status == "Submitted to MC") %>%
  select(mc_number_anon) %>% distinct() 

unique_total <- nrow(jira.manuscripts)

## By journal
unique_total_journal <- jira %>%
  filter(Status == "Submitted to MC") %>%
  group_by(Journal) %>%
  summarise(unique_mc_numbers = n_distinct(mc_number_anon))


# Histogram
n_unique_journal_plot <- ggplot(unique_total_journal, aes(x = Journal, y = unique_mc_numbers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of papers", title = "Total Papers by journal") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))


ggsave(file.path(images,"n_unique_journal_plot.png"), 
       n_unique_journal_plot  +
         labs(y=element_blank(),title=element_blank()))
n_unique_journal_plot


#### Number of assessment processed by external replicator since December 1, 2019
## Total
jira.external<- jira %>% 
  filter(Status == "Submitted to MC") %>%
  filter(external == "Yes") %>%
  select(ticket) %>% distinct() 

external_total <- nrow(jira.external)

## By journal
external_total_journal <- jira %>%
  filter(Status == "Submitted to MC") %>%
  filter(external == "Yes") %>%
  group_by(Journal) %>%
  summarise(external_numbers = n_distinct(ticket))


# Histogram
n_external_journal_plot <- ggplot(external_total_journal, aes(x = Journal, y = external_numbers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Journal", y = "Number of cases processed by external replicator", title = "Total usage of external replicators by journal") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))


ggsave(file.path(images,"n_external_journal_plot.png"), 
       n_external_journal_plot  +
         labs(y=element_blank(),title=element_blank()))

n_external_journal_plot



#### Length of an assessment rounds (initial submission to us, and filing to Manuscript Central). 
# This is the duration of each Jira ticket.
# Uses start date and date of status update after restricting to the cases "Submitted to MC"

duration.data <- jira %>%
  filter(Status == "Submitted to MC") %>%
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

cycle.data <- jira %>%
  filter(Status == "Pending publication") %>%
  mutate(length=difftime(date_updated,date_created,units="days")) %>%
  arrange(length) %>% ungroup()

table.cycle <- cycle.data %>% group_by(length) %>%
  summarise(n_tickets = n_distinct(ticket))
#------
# Histogram
#geom_histogram(aes(y=..density..), colour="white", fill="grey", binwidth = 5)+

cycle_round_length <- ggplot(cycle.data, aes(x = length)) +
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

cycle_round_length.pos <- ggplot(cycle.data %>% filter(length >= 1), aes(x = length)) +
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

cycle_length_hist
cycle_length_hist2
cycle_length_hist_pos


#### Number of cases using different statistical packages
software_used <- jira %>%
  filter(Status == "Submitted to MC",Software.used_1 != "") %>%
  filter(Journal != "") %>%
  select(mc_number_anon, Software.used_1, Software.used_2, Software.used_3, Software.used_4) %>%
  distinct() 

plot_software_used <- software_used %>%
  pivot_longer(!mc_number_anon, names_to = "n", values_to = "software") %>%
  filter(software!="") %>%
  select(!n) %>%
  transform(software = ifelse(software == "r", "R",as.character(software))) %>%
  transform(software = ifelse(software == "Java", "Javascript", as.character(software))) %>%
  transform(software = ifelse(software == "stata", "Stata", as.character(software))) %>%
  transform(software = ifelse(software == "matlab", "Matlab", as.character(software))) %>%
  filter(software != "macbook"& software != "MacBook"& software!= "texstudio"&software!= "Visual_Studio"& software!="LATEX"&software!="Linux"&software!= "") %>%
  group_by(software) %>%
  summarise(n_papers = n_distinct(mc_number_anon))

# Histogram
n_software_used_plot <- ggplot(plot_software_used, aes(x = software, y = n_papers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Statistical Packages", y = "Number of papers", title = "Number of papers by statistical packages used") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=90))

ggsave(file.path(images,"n_software_used_plot.png"), 
       n_software_used_plot  +
         labs(y=element_blank(),title=element_blank()))
n_software_used_plot


--------

#### Length of author responses (time between filing report of first assessment, and getting the next submission)
 author_length <- jira %>%
  filter(mc_number_anon != "",
         ticket != mc_number_anon,
         !grepl("#",mc_number_anon, fixed = TRUE),
         subtask=="") %>%
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
n_rounds <- author_length %>%
  mutate(completed = ifelse(grepl("Pending publication", Status, fixed = TRUE) == TRUE, 1, 0)) %>%
  group_by(mc_number_anon) %>%
  mutate(paper_completed = max(completed)) %>%
  ungroup %>%
  filter(paper_completed == 1) %>%
  select(-completed, -paper_completed) %>%
  group_by(rounds) %>%
  summarise(n_papers = n_distinct(mc_number_anon)) %>%
  ungroup %>%
  mutate(total = sum(n_papers),
         frac_papers = n_papers/total)

n_rounds_plot <- ggplot(n_rounds, aes(x = rounds, y = frac_papers)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  theme_classic() +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Rounds", y = "Fraction of papers", title = "Distribution of review rounds")

ggsave(file.path(images,"n_rounds_plot.png"), 
       n_rounds_plot  +
         labs(y=element_blank(),title=element_blank()))
n_rounds_plot

## By journal
n_rounds_journal <- author_length %>%
  mutate(completed = ifelse(grepl("Pending publication", Status, fixed = TRUE) == TRUE, 1, 0)) %>%
  group_by(mc_number_anon) %>%
  mutate(paper_completed = max(completed)) %>%
  ungroup %>%
  filter(paper_completed == 1) %>%
  select(-completed, -paper_completed) %>%
  group_by(Journal, rounds) %>%
  summarise(n_papers = n_distinct(mc_number_anon)) %>%
  filter(Journal != "") %>%
  group_by(Journal) %>%
  mutate(total = sum(n_papers),
         frac_papers = n_papers/total) %>%
  ungroup


n_rounds_journal_plot <- ggplot(n_rounds_journal, aes(x = rounds, y = frac_papers)) +
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




