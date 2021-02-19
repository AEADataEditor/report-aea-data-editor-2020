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


# we will re-use these 
jira.pyear <- readRDS(file=file.path(temp,"jira.pyear.RDS"))
jira.filter.submitted <- readRDS(file=file.path(temp,"jira.submitted.RDS"))

## By journal
issues_total_journal <- jira.pyear %>%
  filter(!(Journal=="")) %>%
  group_by(Journal) %>%
  summarise(issue_numbers = n_distinct(ticket),
            mcs_numbers   = n_distinct(mc_number_anon))

# stargazer(issues_total_journal,style = "aer",
#           summary = FALSE,
#           out = file.path(tables,"issues_total_journal.tex"),
#           out.header = FALSE,
#           float = FALSE
# )

#### Number of reports processed (went past submitted to MC) since December 1, 2019

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

# stargazer(assess_cplt_journal,style = "aer",
#           summary = FALSE,
#           out = file.path(tables,"assess_cplt_journal.tex"),
#           out.header = FALSE,
#           float = FALSE
#           )






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

# stargazer(pendingpub_by_journal,style = "aer",
#           summary = FALSE,
#           out = file.path(tables,"pendingpub_by_journal.tex"),
#           out.header = FALSE,
#           float = FALSE
# )



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

# # output table
# stargazer(external_total_journal,style = "aer",
#           summary = FALSE,
#           out = file.path(tables,"external_total_journal.tex"),
#           out.header = FALSE,
#           float = FALSE
# )
# # Histogram
# n_external_journal_plot <- ggplot(external_total_journal, aes(x = Journal, y = issues_external)) +
#   geom_bar(stat = "identity", colour="white", fill="grey") +
#   labs(x = "Journal", y = "Number of cases processed by external replicator", title = "Total usage of external replicators by journal") + 
#   theme_classic() +
#   geom_text(aes(label=issues_external), hjust=1.5, size=3.5) +
#   coord_flip()
# 
# 
# ggsave(file.path(images,"n_external_journal_plot.png"), 
#        n_external_journal_plot  +
#          labs(y=element_blank(),title=element_blank()))
# 
#n_external_journal_plot


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


#