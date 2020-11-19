###########################################################################################
#####
#####   AEA Registry Data Analysis
#####   About: This analysis is based on the main dataset for the registry, 
#####           which was split in two for download purposes.
#####   Author: James Turitto
#####   Date Created: 2019_11_22
#####   Last Modified: 2019_12_06
#####
#####   Inputs: 
#####     - aea_data.csv, aea_data_2.csv (if process_raw == TRUE)
#####     - aeareg_data.csv (if process_raw == FALSE)
#####   Outputs: 
#####     - aeareg_data.csv (if process_raw == TRUE)
#####     - regdatedf.Rdata, reg_data_q3.Rdata
############################################################################################

### Cleans working environment.
rm(list = ls())
gc()

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("plyr","dplyr","reshape2","tidyverse","lubridate","here")
results <- sapply(as.list(global.libraries), pkgTest)

### Loads and merges split registry datasets. Saves merged dataset
if ( process_raw == TRUE ) {
  reg_data1 <- read.csv(file.path(rctraw,"aea_data.csv"), header = TRUE, stringsAsFactors = FALSE)
  reg_data2 <- read.csv(file.path(rctraw,"aea_data_2.csv"), header = TRUE, stringsAsFactors = FALSE)
  reg_data <- rbind(reg_data1, reg_data2)
  rm(reg_data1,reg_data2)
  write.csv(reg_data, file.path(rctgen,"aeareg_data.csv"))
}

### Data cleaning.
reg_data <- read.csv(file.path(rctgen,"aeareg_data.csv"), header = TRUE, stringsAsFactors = FALSE)
reg_data <- reg_data [ , -c(17, 20, 21, 22, 23, 24, 25)]
reg_data$quarters <- quarter(as.Date(reg_data$First.registered.on), with_year = TRUE, fiscal_start = 1)
reg_data$count <- 1
reg_data$years <- year(as.Date(reg_data$First.registered.on))
total_year <- by(reg_data$count, reg_data$years, sum)
cumreg_year <- cumsum(total_year)
reg_data$pre_reg <- if_else(reg_data$First.registered.on <= reg_data$Intervention.start.date, 1, 0)
reg_data$First.registered.on <- as.Date(reg_data$First.registered.on) 
reg_data$Start.date <- as.Date(reg_data$Start.date) 
reg_data$pap <- if_else(reg_data$Analysis.Plan.Documents == "None", 0, 1) #create binary variable for PAPs
reg_data_q3 <- reg_data[ which(reg_data$First.registered.on < "2019-10-01"), ] #create new data with only registry entries prior to 10/01/2019

### Produce registry tables. 

# Cumulative line graph of historical registrations. 
regdate <- by(reg_data$count, reg_data$First.registered.on, FUN = sum) 
regdatedf <- data.frame(date = names(regdate), n = as.numeric(regdate))
preregdate <- by(reg_data$pre_reg, reg_data$First.registered.on, FUN = sum) 
regdatedf$prereg <- as.numeric(preregdate)

saveRDS(regdatedf,file=file.path(rctgen,"regdatedf.Rdata"))
saveRDS(reg_data_q3,file=file.path(rctgen,"reg_data_q3.Rdata"))

