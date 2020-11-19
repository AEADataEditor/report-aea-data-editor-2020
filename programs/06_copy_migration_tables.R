# This script is used copy data files from the external git submodules, and create a LaTeX table from it

# Inputs
#   - file.path(migbase,"data","generated"),pattern = "*doi_by_software.csv")
# Outputs
#   - file.path(tables,"table_migration.aea.doi_by_software.csv")

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)

packages <- c("data.table","stringr")
lapply(as.list(packages),pkgTest)

if ( process_raw == TRUE ) {
  
# copy table data
migration_tables = list.files(path = file.path(miggen),pattern = "*doi_by_software.csv")

mycopy <- function(item) {
  file.copy(file.path(miggen,item),file.path(tables,str_replace(item,"table","table_migration")),
            overwrite = TRUE, copy.date = TRUE)
}

lapply(as.list(migration_tables), mycopy)

} else {
  print("Skipped copy")
}


