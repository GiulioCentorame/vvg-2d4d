# different approach
library(readxl)
library(tidyverse)

# Function for merging numerics and flagging failures to match
merge.numeric = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.numeric(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          -999))
  return(output)
}

# function for merging strings and flagging failures to match
merge.character = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.character(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          "CONFLICT!"))
  return(output)
}

# check double-coding
dbl_code_count <- function(x) {
  group_by(x, Subject) %>% 
    summarize(n = n()) %>% 
    with(., table(n))
}

# unit tests for merge functions
stopifnot(merge.numeric(c(1, 1, 1, 1)) == 1)
stopifnot(merge.numeric(c(1, 1, 1, 3)) == -999)
stopifnot(merge.numeric(c(1, NA, NA, NA)) == 1)
stopifnot(merge.character(c(1, "blue", NA, NA)) == "CONFLICT!")
stopifnot(merge.character(c("blue", "blue", NA, NA)) == "blue")
stopifnot(merge.numeric(1) == 1)
stopifnot(merge.character("blue") == "blue")
stopifnot(class(merge.numeric(NA)) == "numeric") 
stopifnot(class(merge.numeric(c(NA, NA))) == "numeric")
stopifnot(class(merge.character(NA)) == "character")
stopifnot(class(merge.character(c(NA, NA))) == "character")

# data frame test
testframe <- data.frame(ID = c(1, 1, 2, 2, 3, 3, 4, 4),
                        str1 = c("blue", "blue", "blue", "red", "blue", NA, NA, NA ),
                        dbl1 = c(10, 10, 10, 15, 10, NA, NA, NA),
                        stringsAsFactors = F)

testframe %>% 
  group_by(ID) %>% 
  summarize_if(is.numeric, merge.numeric)

testframe %>% 
  group_by(ID) %>% 
  summarize_if(is.character, merge.character)

# Debriefing questionnaires ----
temp1 <- read_excel("./raw-data-prep/raw_data/Debrief_AM.xlsx", na = c("", "NA")) %>% 
  filter(!is.na(Subject))
temp2 <- read_excel("./raw-data-prep/raw_data/Debrief_TG.xlsx", na = c("", "NA")) %>% 
  filter(!is.na(Subject))
temp3 <- read_excel("./raw-data-prep/raw_data/Debrief_HS.xlsx", na = c("", "NA")) %>% 
  filter(!is.na(Subject))
debrief <- bind_rows(temp1, temp2, temp3)

# Aggregate. Double-coded entries will combine, unless they mismatch
debrief.chr <- debrief %>% 
  group_by(Subject) %>% 
  summarise_if(is.character, merge.character)
debrief.num <- debrief %>% 
  group_by(Subject) %>% 
  summarise_if(is.numeric, merge.numeric)
debrief2 <- full_join(debrief.num, debrief.chr, by = "Subject")
# check for double-entry coverage
dbl_code_count(debrief)

# Distraction assignments ----
temp1 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_AM.xlsx", na = c("", "NA"))
temp2 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_CH.xlsx", na = c("", "NA"))
temp3 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_JC.xlsx", na = c("", "NA"))
temp4 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_RP.xlsx", na = c("", "NA"))
temp5 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_TG.xlsx", na = c("", "NA"))
# coerce to character
temp1$Subject = as.character(temp1$Subject)
temp2$Subject = as.character(temp2$Subject)
temp3$Subject = as.character(temp3$Subject)
temp4$Subject = as.character(temp4$Subject)
temp5$Subject = as.character(temp5$Subject)
# bind rows to form full spreadsheet across all RAs
distraction = bind_rows(temp1, temp2, temp3, temp4, temp5)
# Aggregate. Double-coded entries will combine, unless they mismatch
distraction2 = distraction %>% 
  group_by(Subject) %>% 
  summarise_all(funs(merge.numeric)) %>% 
  # change Subject to dbl, discard undefined subject ID
  mutate(Subject = as.numeric(Subject)) %>% 
  filter(!is.na(Subject))
# Check on data w/ conflicts
distraction2 %>% 
  filter(!(Assignment %in% 1:9))
# Check for double-coding
dbl_code_count(distraction)

# Note sheets ----
temp1 = read_excel("./raw-data-prep/raw_data/Note_sheet_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Note_sheet_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Note_sheet_RP.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/Note_sheet_TG.xlsx")
temp5 = read_excel("./raw-data-prep/raw_data/Note_sheet_TH.xlsx")
# bind rows to form full spreadsheet across all RAs
# I'm dropping the Date & Time columns because they're a mess
note_sheet = bind_rows(temp1[,-(1:2)], temp2[,-(1:2)], temp3[,-(1:2)], temp4[,-(1:2)], temp5[,-(1:2)])
# Aggregate. Double-coded entries will combine, unless they mismatch
note_sheet.chr = note_sheet %>% 
  group_by(Subject) %>% 
  summarise_if(is.character, merge.character)
note_sheet.num = note_sheet %>% 
  group_by(Subject) %>% 
  summarise_if(is.numeric, merge.numeric)
note_sheet2 <- full_join(note_sheet.num, note_sheet.chr, by = "Subject")
# Check on data w/ conflicts
note_sheet2 %>% 
  filter(Condition == -999)
# Check for double coding
dbl_code_count(note_sheet)

# Post-questionnaire ----
temp1 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_RP.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_TG.xlsx")
temp5 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_HS.xlsx")
# bind rows to form full spreadsheet across all RAs
postquestionnaire = bind_rows(temp1, temp2, temp3, temp4)
# Aggregate.
postquestionnaire.num <- postquestionnaire %>% 
  group_by(Subject) %>% 
  summarize_if(is.numeric, merge.numeric)
postquestionnaire.chr <- postquestionnaire %>% 
  group_by(Subject) %>% 
  summarize_if(is.character, merge.character)

postquestionnaire2 <- full_join(postquestionnaire.num, postquestionnaire.chr, by = "Subject")
# check coverage of double-entry
dbl_code_count(postquestionnaire)

# Writing Task Evaluation ----
temp1 = read_excel("./raw-data-prep/raw_data/Writing_Task_Evaluation_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Writing_Task_Evaluation_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Writing_Task_Evaluation_TG.xlsx")
temp1$Subject = as.character(temp1$Subject)
temp2$Subject = as.character(temp2$Subject)
temp3$Subject = as.character(temp3$Subject)
# combine RA's sheets
writing = bind_rows(temp1, temp2, temp3)
# Aggregate.
writing2 = writing %>% 
  group_by(Subject) %>% 
  summarise_all(merge.numeric)

# 2d4d aggregation will require a different tester function
temp1 = read_excel("./raw-data-prep/raw_data/digits_JS.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/digits_RP.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/digits_TG.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/digits_CN.xlsx")
# bad subject number
temp4$Subject[temp4$Subject %in% c("225", "225-2")] <- NA
temp4$Subject <- as.numeric(temp4$Subject)
temp5 = read_excel("./raw-data-prep/raw_data/digits_HS.xlsx")
digits = bind_rows(temp1, temp2, temp3, temp4, temp5)
# Aggregate
digits2 = digits %>% 
  select(-Notes_t) %>% 
  group_by(Subject) %>% 
  summarise_all(funs(mean))
digCheck = digits %>% 
  select(-Notes_t) %>% 
  group_by(Subject) %>% 
  summarise_all(funs(sd))
# Look for miscodings
hist(digCheck$L_ring_angle)
hist(digCheck$L_ring_length)
hist(digCheck$L_index_angle)
hist(digCheck$L_index_length)
hist(digCheck$R_ring_angle)
hist(digCheck$R_ring_length)
hist(digCheck$R_index_angle)
hist(digCheck$R_index_length)
# High-variance double-codings
# Neither number seems obviously wrong; maybe a 3rd coding would resolve it
digCheck %>% 
  filter(L_ring_length > 10, L_index_length > 10,
         R_ring_length > 10, R_index_length > 10)
# Check extent of double-coding
dbl_code_count(digits) # I want em all double-coded
# which are not yet double-coded?
group_by(digits, Subject) %>% 
  summarize(n = n()) %>% 
  filter(n < 2)

# Combine all data
dat = data.frame("Subject" = 1:450) # holster dataframe
# coerce subject ID to character for easier joining
dat$Subject = as.character(dat$Subject)
note_sheet2$Subject = as.character(note_sheet2$Subject)
debrief2$Subject = as.character(debrief2$Subject)
distraction2$Subject = as.character(distraction2$Subject)
postquestionnaire2$Subject = as.character(postquestionnaire2$Subject)
writing2$Subject = as.character(writing2$Subject)
digits2$Subject = as.character(digits2$Subject)
# combine all via join
dat = full_join(dat, debrief2, by = "Subject")
dat = full_join(dat, note_sheet2, by = "Subject")
dat = full_join(dat, distraction2, by = "Subject")
dat = full_join(dat, postquestionnaire2, by = "Subject")
dat = full_join(dat, writing2, by = "Subject")
dat = full_join(dat, digits2, by = "Subject")

# TODO: Rearrange columns into useful order!
name_order <- unique(c(names(debrief2), names(note_sheet2),
                names(distraction2), names(postquestionnaire2),
                names(writing2), names(digits2)))
t1 <- select(dat, name_order) %>% names()
t2 <- names(dat)

# Export full thing to Rdata
save(dat, file = "full_data.RData")

# Export full thing sans text columns to .txt
#    (text columns break it)
dat %>% 
  select(-ends_with("_t")) %>% 
  write.table("full_data.txt", sep = "\t", row.names = F)

