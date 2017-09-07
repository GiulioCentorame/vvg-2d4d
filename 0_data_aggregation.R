# different approach
library(readxl)
library(dplyr)
library(tidyr)

# tester() returns identity in case of a match or an unpaired entry,
# returns NA if all inputs NA,
# returns "CONFLICT!" or -999 if cells do not match
# tester = function(x) {
#   ifelse(length(unique(na.omit(x))) == 1, unique(x), 
#          ifelse(length(unique(na.omit(x))) == 0, NA,
#                 ifelse(is.numeric(x), -999, 
#                        ifelse(is.na, NA, "CONFLICT!"))))
# }

merge.numeric = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.numeric(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          -999))
  return(output)
}

merge.character = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.character(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          "CONFLICT!"))
  return(output)
}

# unit tests for tester()
merge.numeric(c(1, 1, 1, 1))
merge.numeric(c(1, 1, 1, 3))
merge.numeric(c(1, NA, NA, NA))
merge.character(c(1, "blue", NA, NA))
merge.character(c("blue", "blue", NA, NA))
merge.numeric(1)
merge.character("blue")
class(merge.numeric(NA))
class(merge.numeric(c(NA, NA)))
class(merge.character(NA))
class(merge.character(c(NA, NA)))

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

# Post-questionnaire ----
temp1 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_RP.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_TG.xlsx")
temp5 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_HS.xlsx")
# bind rows to form full spreadsheet across all RAs
post_questionnaire = bind_rows(temp1, temp2, temp3, temp4)
# Aggregate.
post_questionnaire.num <- post_questionnaire %>% 
  group_by(Subject) %>% 
  summarize_if(is.numeric, merge.numeric)
post_questionnaire.chr <- post_questionnaire %>% 
  group_by(Subject) %>% 
  summarize_if(is.character, merge.character)

post_questionnaire2 <- full_join(post_questionnaire.num, post_questionnaire.chr, by = "Subject")

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
temp5 = read_excel("./raw-data-prep/raw_data/digits_HS.xlsx")
digits = bind_rows(temp1, temp2, temp3)
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

# Combine all data
dat = data.frame("Subject" = 1:450) # holster dataframe
# coerce subject ID to character for easier joining
dat$Subject = as.character(dat$Subject)
note_sheet2$Subject = as.character(note_sheet2$Subject)
debrief2$Subject = as.character(debrief2$Subject)
distraction2$Subject = as.character(distraction2$Subject)
post_questionnaire2$Subject = as.character(post_questionnaire2$Subject)
writing2$Subject = as.character(writing2$Subject)
digits2$Subject = as.character(digits2$Subject)
# combine all via join
dat = full_join(dat, debrief2, by = "Subject")
dat = full_join(dat, note_sheet2, by = "Subject")
dat = full_join(dat, distraction2, by = "Subject")
dat = full_join(dat, post_questionnaire2, by = "Subject")
dat = full_join(dat, writing2, by = "Subject")
dat = full_join(dat, digits2, by = "Subject")

# TODO: Rearrange columns into useful order!
name_order <- unique(c(names(debrief2), names(note_sheet2),
                names(distraction2), names(post_questionnaire2),
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
