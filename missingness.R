# Script for identifying missingness of data

library(dplyr)

dat = read.delim("./analysis/aggregated_data.txt", 
                 #quote="", 
                 sep="\t", 
                 stringsAsFactors=F)
names(dat)[names(dat)=="Assignment"] = "DV"
dat$Subject = as.numeric(dat$Subject)

dat %>% 
  filter(is.na(DV) | is.na(Condition)) %>% 
  select(Subject, DV, Condition) %>% 
  View

dat %>% 
  filter(Subject == 420) %>% 
  select(Subject, DV, Condition) %>% 
  View