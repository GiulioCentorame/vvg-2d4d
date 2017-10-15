library(dplyr)

dat = read.delim("full_data.txt")
stash.names <- names(dat)

# remove debug rows
dat <- filter(dat, !(Subject %in% c(666, 900)))

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
# Might want to create codes, make tables for ease of tracking where the data is lost
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
fail.easydie <- dat %>% 
  filter(Difficulty == "Easy", Game.1 > 0) %>% 
  select(Subject)

# took damage in easy-game condition
fail.easyharm <- dat %>% 
  filter(Difficulty == "Easy", Game.6 > 0) %>% 
  select(Subject)

# didn't take damage in hard-game condition???
fail.hard <- dat %>% 
  filter(Difficulty == "Hard", Game.6 == 0) %>% 
  select(Subject)
# 430 kills and no damage is deeply implausible, must be something wrong

# called hypothesis w/o wrong guesses
fail.savvy <- dat %>% 
  filter(Q1.a == 1 & (Q1.b == 0 & Q1.c == 0 & Q1.d == 0 & Q1.e == 0))  %>% 
  select(Subject)

# RAs didn't think session went well
fail.badsesh <- dat %>% 
  filter(Good.Session == "No") %>% 
  select(Subject)

# RA cited exp failure
length(fail.badsesh$Subject)
#   plus took damage / died in easy game / took no damage in hard game
length(setdiff(c(fail.easydie$Subject, fail.easyharm$Subject, fail.hard),
               fail.badsesh$Subject))
#   plus savvy to hypothesis
length(setdiff(fail.savvy$Subject, 
               c(fail.easydie$Subject, fail.easyharm$Subject, fail.hard, 
                 fail.badsesh$Subject)))

# Find and correct merge errors ----
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

# hyunji labeled irreconcilably bad or missing data as "BAD"
# I don't want to screw up my classes with character data so I'll treat that as NA
hyunji <- read_excel("debug/master_baddata.xlsx", na = c("", "BAD"))

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
dat <- fixed.dat


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
  # took no damage in hard-game condition
  filter(is.na(Game.6) | !(Difficulty == "Hard" & Game.6 == 0)) %>% 
  # called hypothesis w/o wrong guesses
  filter(is.na(Q1.a) | !(Q1.a == 1 & (Q1.b == 0 & Q1.c == 0 & Q1.d == 0 & Q1.e == 0)))  %>% 
  # RAs didn't think session went well
  filter(is.na(Good.Session) | Good.Session != "No")

# Discard bad 2d4d data ----
dat.pure$R2d4d[dat.pure$R2d4d < .8] = NA
dat.pure$L2d4d[dat.pure$L2d4d < .8] = NA

# TODO: look for more bad data

# TODO: inspect and consider harsher treatment for missing quality-control data
# TODO: Create 2d4d ratios and look for outliers

# TODO: Notes from old cleaning code ----
# may wish to use debriefing questionnaire columns:
#   Suspected_Debrief Game.play.affect.distraction.time_Debrief or Surprise_Debrief
# I probably ruined these questionnaires by having the RA attempt funneled debriefing and 
# then give the questionnaire... But if the funneled debriefing didn't turn up anything,
# the paper questionnaire should be legit!

# It looks like RA's decision of whether it was a good session or not
# had nothing to do with whether subject listed vg & aggression or suspicion of DV

# Export ----
write.table(dat.pure, "clean_data.txt", sep = "\t", row.names = F)
