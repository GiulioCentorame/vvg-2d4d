# different approach
library(readxl)
library(dplyr)
library(tidyr)

# tester returns identity in case of a match or an unpaired entry,
# returns -999 if cells do not match
tester = function(x) ifelse(length(unique(na.omit(x))) == 1, unique(x), 
                            ifelse(is.numeric(x), -999, "CONFLICT!"))

# Debriefing questionnaires ----
debrief = bind_rows(read_excel("./raw-data-prep/raw_data/Debrief_AM.xlsx"), 
                    read_excel("./raw-data-prep/raw_data/Debrief_TG.xlsx"))
# Aggregate. Double-coded entries will combine, unless they mismatch
debrief2 = debrief %>% 
  select(-ends_with("_t")) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(tester))

# Distraction assignments ----
temp1 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_JC.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_RP.xlsx")
temp5 = read_excel("./raw-data-prep/raw_data/Distraction_Assignment_TG.xlsx")
# coerce to character
temp2$Subject = as.character(temp2$Subject)
temp3$Subject = as.character(temp3$Subject)
temp4$Subject = as.character(temp4$Subject)
temp5$Subject = as.character(temp5$Subject)
# bind rows to form full spreadsheet across all RAs
distraction = bind_rows(temp1, temp2, temp3, temp4, temp5)
# Aggregate. Double-coded entries will combine, unless they mismatch
distraction2 = distraction %>% 
  group_by(Subject) %>% 
  summarise_each(funs(tester))
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
note_sheet2 = note_sheet %>% 
  group_by(Subject) %>% 
  summarise_each(funs(tester))

# Post-questionnaire ----
temp1 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_AM.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_CH.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_RP.xlsx")
temp4 = read_excel("./raw-data-prep/raw_data/Post-Questionnaire_TG.xlsx")
# bind rows to form full spreadsheet across all RAs
post_questionnaire = bind_rows(temp1, temp2, temp3, temp4)
# Aggregate.
post_questionnaire2 = post_questionnaire %>% 
  group_by(Subject) %>% 
  summarise_each(funs(tester))

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
  summarise_each(funs(tester))

# 2d4d aggregation will require a different tester function
temp1 = read_excel("./raw-data-prep/raw_data/digits_JS.xlsx")
temp2 = read_excel("./raw-data-prep/raw_data/digits_RP.xlsx")
temp3 = read_excel("./raw-data-prep/raw_data/digits_TG.xlsx")
digits = bind_rows(temp1, temp2, temp3)
# Aggregate
digits2 = digits %>% 
  select(-Notes_t) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean))
digCheck = digits %>% 
  select(-Notes_t) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(sd))
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
debrief2$Subject = as.character(debrief2$Subject)
distraction2$Subject = as.character(distraction2$Subject)
post_questionnaire2$Subject = as.character(post_questionnaire2$Subject)
writing2$Subject = as.character(writing2$Subject)
digits2$Subject = as.character(digits2$Subject)
# combine all via join
dat = full_join(dat, debrief2, by = "Subject")
dat = full_join(dat, distraction2, by = "Subject")
dat = full_join(dat, post_questionnaire2, by = "Subject")
dat = full_join(dat, writing2, by = "Subject")
dat = full_join(dat, digits2, by = "Subject")

# Export full data sheet for inspection, cleaning, & analysis
write.table(dat, "full_data.txt", sep = "\t", row.names = F)