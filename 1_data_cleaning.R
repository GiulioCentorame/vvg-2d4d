library(dplyr)

dat = read.delim("full_data.txt")
stash.names <- names(dat)

# look for missingness
sapply(dat, function(x) sum(is.na(x)))
# look for merge conflicts
sapply(dat, function(x) sum(x == -999 | x == "CONFLICT!", na.rm = T))


# Create and rename columns ----

# rename Assignment to DV
dat = dat %>% 
  mutate(DV = Assignment)
# Add factor columns for violence & difficulty
dat$Violence = ifelse(dat$Condition == 1 | dat$Condition == 2, "Violent", "Less Violent")
dat$Difficulty = ifelse(dat$Condition == 2 | dat$Condition == 4, "Hard", "Easy")

# Add 2d4d ratios
dat$L2d4d = dat$L_index_length/dat$L_ring_length
dat$R2d4d = dat$R_index_length/dat$R_ring_length

# Count bad subjects ----
# TODO: check that all this is working right
# Missing primary data: Condition, DV
dir.create("debug")
dat %>% 
  filter(is.na(DV) | is.na(Violence) | is.na(Difficulty) | is.na(Condition)) %>%
  write.csv(file = "debug/missing_cond_DV.csv", row.names = F)
# conflicting DV or condition data
dat %>% 
  filter(DV == -999 | Condition == -999) %>% 
  write.csv(file = "debug/conflicting_cond_DV.csv", row.names = F)

# died in easy-game condition
dat %>% 
  filter(Difficulty == "Easy", Game.1 > 0) %>% 
  nrow

# took damage in easy-game condition
dat %>% 
  filter(Difficulty == "Easy", Game.6 > 0) %>% 
  nrow

# called hypothesis w/o wrong guesses
dat %>% 
  filter(Q1.a == 1 & (Q1.b == 0 & Q1.c == 0 & Q1.d == 0 & Q1.e == 0))  %>% 
  nrow

# RAs didn't think session went well
dat %>% 
  filter(Good.Session == "No") %>% 
  nrow

# Discard bad subjects ----
dat.pure = dat %>% 
  # Missing primary data: Condition, DV
  filter(!is.na(DV), !is.na(Violence), !is.na(Difficulty), !is.na(Condition)) %>%
  # conflicting DV or condition data
  filter(DV != -999, Condition != -999) %>% 
  # died in easy-game condition
  filter(is.na(Game.1) | !(Difficulty == "Easy" & Game.1 > 0)) %>% 
  # took damage in easy-game condition
  filter(is.na(Game.6) | !(Difficulty == "Easy" & Game.6 > 0)) %>% 
  # called hypothesis w/o wrong guesses
  filter(is.na(Q1.a) | !(Q1.a == 1 & (Q1.b == 0 & Q1.c == 0 & Q1.d == 0 & Q1.e == 0)))  %>% 
  # RAs didn't think session went well
  filter(is.na(Good.Session) | Good.Session != "No")

# Discard bad 2d4d data ----
dat.pure$R2d4d[dat.pure$R2d4d < .8] = NA
dat.pure$L2d4d[dat.pure$L2d4d < .8] = NA

# TODO: look for more bad data
# Here I gather all columns into one, filter for merge errors, then spread into subjects again
debug.dat <- dat %>% 
  gather(key, value, Q1.a:R2d4d) %>% 
  filter(value %in% c(-999, "CONFLICT!")) %>% 
  filter(!is.na(Subject)) %>% 
  spread(key = key, value = value)
# Add back in the column names with no conflicts for safety's sake
temp <- matrix(nrow = 1, ncol = length(stash.names))
temp <- as.data.frame(temp)
names(temp) <- stash.names
temp <- bind_rows(temp, debug.dat) %>% 
  filter(!is.na(Subject))

# sort into nice column order and export for RAs
write.csv(temp, "debug/master_baddata.csv", row.names = F, na = "")
  

# TODO: explore underlying root causes of conflicting info across RAs,
# TODO: inspect and consider harsher treatment for missing quality-control data

# TODO: Create 2d4d ratios and look for outliers

# TODO: Notes from old cleaning code ----
# may wish to use column dat$Suspected_Debrief
# dat$Game.play.affect.distraction.time_Debrief or dat$Surprise_Debrief
# I probably ruined these questionnaires by having the RA attempt funneled debriefing and then
# give the questionnaire... But then, if the funneled debriefing didn't turn up anything,
# the paper questionnaire should be legit!
# Maybe it's become impossible to collect any damn data in this area...

# It looks like RA's decision of whether it was a good session or not
# had nothing to do with whether or not the subject listed vg & aggression or suspicion of DV

# Export ----
write.table(dat.pure, "clean_data.txt", sep = "\t", row.names = F)
