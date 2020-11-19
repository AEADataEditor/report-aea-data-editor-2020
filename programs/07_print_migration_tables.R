# This script is used copy data files from the external git submodules, and create a LaTeX table from it

# Inputs
#   - file.path(tables,"table_migration.aea.doi_by_software.csv")
# Outputs
#   - file.path(tables,"table_software.tex")

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)

packages <- c("stargazer","data.table","stringr","dplyr")
lapply(as.list(packages),pkgTest)


# read the table we are interested in
software <- fread(file.path(tables,"table_migration.aea.doi_by_software.csv")) %>% select(-V1) %>%
  filter(Percent > 2)
names(software) <-  c("Software","Supplements","Percent")

stargazer(software,summary=FALSE,out=file.path(tables,"table_software.tex"),
          style="aer",
          rownames=FALSE,
          title="Software usage in supplements",
          label="tab:software",
          digits=1,
          notes=c("Software usage in supplements.","A supplement can use more than one software.","Software with less than 2 percent utilization not listed."))



