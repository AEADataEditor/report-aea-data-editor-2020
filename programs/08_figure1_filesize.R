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
global.libraries <- c("dplyr","ggplot2","here","tidyr","tibble","remotes","splitstackshape")
results <- sapply(as.list(global.libraries), pkgTest)
pkgTest.github("data.table","Rdatatable")

## Non-standard - install of a page with same name
#pkgTest.github("stargazer-booktabs","markwestcott34")
install_github("markwestcott34/stargazer-booktabs")
library(stargazer)

# Get the data
# Read in data extracted from openICPSR,
icpsr <- read.csv(file.path(icpsrbase,"AEA Report Fedora.csv"), stringsAsFactors = FALSE)

## Distribution of replication packages
icpsr.file_size <- icpsr %>% 
  cSplit("Created.Date","T") %>%
  mutate(date_created=as.Date(substr(Created.Date_1, 1,10), "%Y-%m-%d")) %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  transform(filesize=Total.File.Size/(1024^3)) %>% # in GB
  transform(intfilesize=round(filesize))

# get some stats
icpsr.file_size %>% 
  summarize(mean=round(mean(filesize),2),
            median=round(median(filesize),2),
            q75=round(quantile(filesize,0.9),2)) -> icpsr.stats

icpsr.file_size %>% 
  group_by(intfilesize) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(percent=100*n/sum(n)) -> icpsr.stats1

update_latexnums("pkgsizetwog",icpsr.stats1 %>% 
                             filter(intfilesize > 2) %>% 
                             summarize(percent=sum(percent)) %>% round(0))
update_latexnums("pkgsizetwentyg",icpsr.stats1 %>% 
                              filter(intfilesize >19) %>% 
                              summarize(percent=sum(percent)) %>% round(0))


update_latexnums("pkgsizemean",icpsr.stats$mean)
update_latexnums("pkgsizeq50x",icpsr.stats$median)
update_latexnums("pkgsizeq90x",icpsr.stats$q75)

# graph it all

dist_size <- icpsr.file_size %>%
  transform(intfilesize=pmin(round(filesize),10,na.rm = TRUE)) %>%
  group_by(intfilesize) %>%
  summarise(count=n())


plot_filesize_dist <- ggplot(dist_size, aes(x = intfilesize,y=count)) +
  geom_bar(stat="identity", colour="black", fill="grey")+
  theme_classic() +
  labs(x = "GB",
       y = "Number of Packages", 
       title = "Size distribution of replication packages") +
  coord_flip()

ggsave(file.path(images,"plot_filesize_dist.png"), 
       plot_filesize_dist  +
         labs(y=element_blank(),title=element_blank()))
#plot_filesize_dist
