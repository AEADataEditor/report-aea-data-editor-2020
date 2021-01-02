# Identifying issues with AER
# Lars Vilhuber
# 2021-01-01

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("ggplot2","dplyr","here","tidyr","tibble","devtools","lubridate")
results <- sapply(as.list(global.libraries), pkgTest)


# Read in data extracted from Jira, anonymized
jira.raw <- readRDS(file.path(jiraanon,"jira.anon.RDS")) 

jira.raw %>% filter(Journal == "AER") -> jira.aer

# Select AER only, after 2019-12-01
inout <- jira.aer %>% 
  filter(date_created > "2019-12-01") %>% 
  filter(Status == "Open" | Status == "Submitted to MC") %>%
  mutate(MCRecommendationV2= if_else(MCRecommendationV2=="","Inflow",MCRecommendationV2)) %>%
  mutate(flow = case_when(grepl("Conditional",MCRecommendationV2 ) ~ "Revise",
                          grepl("Accept",MCRecommendationV2) ~ "Final",
                          TRUE ~ "_Inflow")) %>%
  distinct(ticket,Status, .keep_all = TRUE) %>% 
  mutate(ym = format(date_updated,"%Y-%m"))

issue_flows <- ggplot(inout %>% filter(month(date_updated) != 12),
       aes(x=ym)) + 
  geom_bar(aes(fill=flow),position="fill") + 
  labs(x = "Flows",  title = "Inflow/outflow") + 
  geom_hline(aes(yintercept=0.5),
             linetype="dashed", size=0.5)  +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  coord_flip() 

ggsave(file.path(images,"issue_flows_aer.png"), 
       issue_flows  +
         labs(y=element_blank(),title=element_blank()))

# look at recommendations

jira.filter.submitted <- jira.aer %>%  
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") %>%
  distinct(ticket, .keep_all = TRUE)

recommendation <- jira.filter.submitted %>%
  filter(date_updated > "2020-01-01") %>%
  filter(date_updated < "2020-12-01") %>%
  select(ticket, MCRecommendationV2,Journal,date_updated) %>%
  filter(MCRecommendationV2!="") %>%
  filter(!ticket %in% c("AEAREP-924","AEAREP-758","AEAREP-710","AEAREP-701","AEAREP-502","AEAREP-470","AEAREP-454","AEAREP-451","AEAREP-405")) %>%
  distinct(ticket, .keep_all = TRUE) %>%
  transform(mcrec=as.character(MCRecommendationV2),
            ym=format(date_updated,"%Y-%m")) %>%
  select(ticket,mcrec,Journal,date_updated,ym)

summary_recommendation <- recommendation %>%
  group_by(mcrec) %>%
  summarise(n_issues = n_distinct(ticket))

# Histogram
n_recommendation_plot <- ggplot(summary_recommendation, aes(x = mcrec, y = n_issues)) +
  geom_bar(stat = "identity", colour="white", fill="grey") +
  labs(x = "Recommendation", y = "Number of issues", title = "Number of issues by recommendation") + 
  theme_classic() +
  scale_fill_brewer(palette="Dark2")+
  coord_flip()

n_recommendation_plot_evo <- ggplot(recommendation, aes(x = ym)) +
  geom_bar(aes(fill=mcrec),position="stack",colour="white") +
  labs(x = "Recommendation", y = "Number of issues", title = "Number of issues by recommendation") + 
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  coord_flip()

# save images
ggsave(file.path(images,"n_recommendation_plot_aer.png"), 
       n_recommendation_plot  +
         labs(y=element_blank(),title=element_blank()))

ggsave(file.path(images,"n_recommendation_plot_evo_aer.png"), 
       n_recommendation_plot_evo  +
         labs(y=element_blank(),title=element_blank()))

