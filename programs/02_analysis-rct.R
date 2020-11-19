###########################################################################################
#####
#####   AEA Registry Data Analysis
#####   About: This analysis is based on the main dataset for the registry, 
#####           which was split in town for download purposes.
#####   Author: James Turitto
#####   Date Created: 2019_11_22
#####   Last Modified: 2019_12_06
#####
#####   Inputs: 
#####     - regdatedf.Rdata, reg_data_q3.Rdata
#####   Outputs:
#####     - ggsave(file.path(images,"figure_rctgrowth.png"),cum_chart)
#####     - ggsave(file.path(images,"figure_preregistrations.png"),Prereg_chart)
#####     - ggsave(file.path(images,"figure_preanalysisplans.png"),Pap_chart)
#####     - file.path(rctgen,"AEAcharts.RData"))
############################################################################################

### Cleans working environment.
rm(list = ls())
gc()

### Load libraries 
### Requirements: have library *here*
source(here::here("programs","config.R"),echo=TRUE)
global.libraries <- c("plyr","dplyr","reshape2","tidyverse","lubridate","here")
results <- sapply(as.list(global.libraries), pkgTest)

# Load datasets
regdatedf <- readRDS(file=file.path(rctgen,"regdatedf.Rdata"))
reg_data_q3 <- readRDS(file=file.path(rctgen,"reg_data_q3.Rdata"))

# Figure 1 in Registry section
min.year <- min(as.Date(regdatedf$date))
max.year <- max(as.Date(regdatedf$date))
cum_chart <- ggplot(data = regdatedf, aes(x = as.Date(regdatedf$date), y = cumsum(regdatedf$n))) + 
        geom_line() +
        scale_y_continuous(breaks = seq(0,3500,500)) +
#        labs(title = "Cumulative Count of Registrations on AEA", 
#             subtitle = "(Data accessed November 22, 2019)") + 
        theme_classic() +
        scale_colour_brewer(palette = "Paired") +
        scale_x_date(limits=c(min.year,max.year),
                     date_breaks = "1 year",
                     name = element_blank(),
                     date_labels = "%Y") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 20))
ggsave(file.path(images,"figure_rctgrowth.png"),cum_chart)


# Figure 2 of RCT section: Stacked barchart showing share of pre-registered studies by quarter
prereg_quarter <- by(reg_data_q3$pre_reg, reg_data_q3$quarters, sum)
total_quarter <- by(reg_data_q3$count, reg_data_q3$quarters, sum)
quarter <- names(prereg_quarter)
table_preregistration <- data.frame(quarter, Prereg_byquarter = as.numeric(prereg_quarter), 
                                    Total_byquarter = as.numeric(total_quarter))
table_preregistration$proportion <- table_preregistration$Prereg_byquarter/table_preregistration$Total_byquarter
table_preregistration$nonprereg <- table_preregistration$Total_byquarter - table_preregistration$Prereg_byquarter 
table_prereg2 <- melt(table_preregistration, id.var="quarter", 
                      measure.vars = c("Prereg_byquarter", "nonprereg"))

# Write out table
write.csv(table_prereg2,file=file.path(rctgen,"table_preregistrations.csv"))

# Create figure
#title = "Share of Pre-registered Studies per Quarter, September 2019",

Prereg_chart <- ggplot(table_prereg2, aes(x = quarter, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        theme_classic() +
        scale_colour_brewer(palette = "Paired") +
        scale_fill_grey(name = "Registration Date", 
                        labels = c("Before intervention", "After intervention")) +
        labs(x = element_blank(), y = element_blank()) +
        theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
              axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(file.path(images,"figure_preregistrations.png"),Prereg_chart)

# Figure 3 of RCT section: Stacked barchart showing share of pre-analyis plans registered by quarter. 
pap_quarter <- by(reg_data_q3$pap, reg_data_q3$quarters, sum)
table_pap <- data.frame(quarter, Pap_quarter = as.numeric(pap_quarter), 
                        Total_byquarter = as.numeric(total_quarter))
table_pap$prop <- table_pap$Pap_quarter/table_pap$Total_byquarter
table_pap$nonpap <- table_pap$Total_byquarter - table_pap$Pap_quarter
table_pap2 <- melt(table_pap, id.var = "quarter", 
                   measure.vars = c("Pap_quarter", "nonpap"))
# Write out table
write.csv(table_pap2,file=file.path(rctgen,"table_preanalysisplans.csv"))

# Create figure
#title = "Share of Pre-analysis Plans Registered per Quarter, September 2019", 

Pap_chart <- ggplot(table_pap2, aes(x = quarter, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        theme_classic() +
        scale_colour_brewer(palette = "Paired") +
        scale_fill_grey(name = "Pre-Analysis Plan", 
                        labels = c("Yes", "No")) +
        labs(x = element_blank(), y = element_blank()) +
        theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
              axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(file.path(images,"figure_preanalysisplans.png"),Pap_chart)

# Save relevant objects

save(cum_chart, Prereg_chart, Pap_chart, file = file.path(rctgen,"AEAcharts.RData"))
