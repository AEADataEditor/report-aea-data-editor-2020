# write out the numbers collected


### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("dplyr","tidyr","here")
results <- sapply(as.list(global.libraries), pkgTest)


# Make available the latexnums dataset

latexnums <- readRDS(latexnums.Rda) %>%
  mutate(pre="\\newcommand{\\",mid="}{",end="}") %>%
  unite(latexcode,c("pre","field","mid","value","end"),sep = "")
write(latexnums$latexcode,file=file.path(tables,"latexnums.tex"))

