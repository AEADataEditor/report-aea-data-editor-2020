# Tabulate statistics and make graphs for the AEA data editor report
# Harry Son
# 12/8/2020

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs
#   - ggsave(file.path(images,"revision_round_length_hist.png"), 
#   - ggsave(file.path(images,"revision_round_length_hist2.png"), 
#   - ggsave(file.path(images,"revision_round_length_hist_pos.png"), 
#   - ggsave(file.path(images,"total_length_hist.png"), 
#   - ggsave(file.path(images,"n_rounds_plot.png"), 
#   - ggsave(file.path(images,"n_rounds_journal_plot.png"), 
#   - ggsave(file.path(images,"n_assessments_journal_plot.png"), 
#   - ggsave(file.path(images,"author_response_hist.png"), 
                               
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
jira <- readRDS(file.path(jiraanon,"jira.anon.RDS"))

#### Number of reports processed (went past submitted to MC) since December 1, 2019
## Total
jira.assess<- jira %>% 
  filter(Status == "Submitted to MC"|Status ==  "Pending openICPSR changes"|Status == "Pending publication") %>%
  select(ticket) %>% distinct() 

assess_total <- nrow(jira.assess)

## By journal
assess_total_journal <- jira %>%
  filter(Status == "Submitted to MC"|Status ==  "Pending openICPSR changes"|Status == "Pending publication") %>%
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
  filter(Status == "Submitted to MC"|Status ==  "Pending openICPSR changes"|Status == "Pending publication") %>%
  select(mc_number_anon) %>% distinct() 

unique_total <- nrow(jira.manuscripts)

## By journal
unique_total_journal <- jira %>%
  filter(Status == "Submitted to MC"|Status ==  "Pending openICPSR changes"|Status == "Pending publication") %>%
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
         !grepl("#",mc_number_anon, fixed = TRUE)) %>%
  select(ticket, mc_number_anon, date_created, Status, Journal) %>%
  filter(Journal != "") %>%
  distinct() %>%
  group_by(mc_number_anon) %>%
  mutate(rounds = n_distinct(ticket),
         numeric_ticket = as.numeric(gsub("\\D", "", ticket)),
         first_ticket = min(numeric_ticket),
         last_ticket = max(numeric_ticket),
         ticket_order = ifelse(numeric_ticket == first_ticket, 1, 
                               ifelse(numeric_ticket == last_ticket & rounds == 2, 2, 
                                      ifelse(numeric_ticket == last_ticket & rounds == 3, 3,
                                             ifelse(numeric_ticket == last_ticket & rounds == 4, 4, 0)))),
         ticket_order = ifelse(ticket_order == 0 & rounds == 3, 2, ticket_order),
         middle_of_4 = ifelse(rounds == 4 & numeric_ticket != first_ticket & numeric_ticket != last_ticket, 1, 0),
         max_middle = max(numeric_ticket[middle_of_4 == 1]),
         ticket_order = ifelse(rounds == 4 & middle_of_4 == 1 & numeric_ticket == max_middle, 3, 
                               ifelse(rounds == 4 & middle_of_4 == 1 & numeric_ticket != max_middle, 2, ticket_order))) %>%
  select(-middle_of_4, -max_middle) %>%
  filter(Journal!="")
  ungroup 

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




