# This script only serves to copy files from the external git submodules into the image directory.
# This could also be done by hand, or using any other scripting language, 
#  or could download from the repository Github site directly
#  https://github.com/AEADataEditor/aea-supplement-migration
#
# Pre-requisites
#  - git submodule in migbase
# INPUTS:
#  - list.files(path = migprog,pattern = "*.png")
# OUTPUTS:
#  - file.path(images,str_replace(item,"figure_","figure_migration_"))

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)

mylibs <- c("stringr")
lapply(as.list(mylibs),pkgTest)

if ( download_raw == TRUE) {
  tmpfile <- file.path(migbase,"tmp.zip")
  download.file(paste0(migurlbase,"v",migversion,".zip"),destfile = tmpfile)
  unzip(tmpfile,exdir = migbase)
}

if ( process_raw == TRUE ) {
  
# copy figures
migration_figures = list.files(path = migprog,pattern = "*.png")

mycopy <- function(item) {
  file.copy(file.path(migprog,item),file.path(images,str_replace(item,"figure_","figure_migration_")),
            overwrite = TRUE, copy.date = TRUE)
}

lapply(as.list(migration_figures), mycopy)

} else {
  print("Skipped copy")
}



