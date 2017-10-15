# One way to implement the hyunji hand-corrected data into the flawed data
# pound the data flat, replace dat$value with hyunji$value

library(tidyverse)
library(readxl)

# Function for overwriting merge conflicts with real values
force.numeric = function(x) {
  x <- x[x != -999] # discard conflicts
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.numeric(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          -999))
  return(output)
}

# function for merging strings and flagging failures to match
force.character = function(x) {
  x <- x[x != "CONFLICT!"] # discard conflicts
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.character(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          "CONFLICT!"))
  return(output)
}

# ok for real now
dat <- read.delim("clean_data.txt", stringsAsFactors = F)
# hyunji labeled irreconcilably bad or missing data as "BAD"
# I don't want to screw up my classes with character data so I'll treat that as NA
hyunji <- read_excel("debug/master_baddata.xlsx", na = c("", "BAD"))

# separate by class and flatten
flatdat.num <- dat %>% 
  group_by(Subject) %>%
  select_if(is.numeric) %>% 
  gather(key, value, -Subject)
flatdat.chr <- dat %>% 
  group_by(Subject) %>% 
  select_if(is.character) %>% 
  gather(key, value, -Subject)
hyunji.num <- hyunji %>% 
  group_by(Subject) %>%
  select_if(is.numeric) %>% 
  gather(key, value, -Subject)
hyunji.chr <- hyunji %>% 
  group_by(Subject) %>% 
  select_if(is.character) %>% 
  gather(key, value, -Subject)

# Bind dataframes & use summarize() to force overwrite bad flatdat w/ hyunji's data
fused.num <- bind_rows(flatdat.num, hyunji.num) %>% 
  group_by(Subject, key) %>% 
  summarize(value = force.numeric(value))
fused.chr <- bind_rows(flatdat.chr, hyunji.chr) %>% 
  group_by(Subject, key) %>% 
  summarize(value = force.character(value))

# turn back into wide data
wide.num <- spread(fused.num, key, value)
wide.chr <- spread(fused.chr, key, value)
fixed.dat <- full_join(wide.num, wide.chr, by = "Subject")

# rearrange as the order it once was
fixed.dat <- fixed.dat %>% 
  select(names(dat))
