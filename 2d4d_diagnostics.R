library(readxl)
library(tidyverse)

# Fetch 2d4d raw data
temp1 = read_excel("./raw-data-prep/raw_data/digits_JS.xlsx")
temp1$coder <- "JS"
temp2 = read_excel("./raw-data-prep/raw_data/digits_RP.xlsx")
temp2$coder <- "RP"
temp3 = read_excel("./raw-data-prep/raw_data/digits_TG.xlsx")
temp3$coder <- "TG"
temp4 = read_excel("./raw-data-prep/raw_data/digits_CN.xlsx")
temp4$coder <- "CN"
temp4$Subject[temp4$Subject %in% c("225", "225-2")] <- NA # fix bad subject number, coercing to NA
temp4$Subject <- as.numeric(temp4$Subject)
temp5 = read_excel("./raw-data-prep/raw_data/digits_HS.xlsx")
temp5$coder <- "HS"

digits = bind_rows(temp1, temp2, temp3, temp4, temp5)

# make 2d4d ratio
digits$L2d4d = digits$L_index_length/digits$L_ring_length
digits$R2d4d = digits$R_index_length/digits$R_ring_length

# flatten across fingers and measures
cordat <- digits %>% 
  select(-Notes_t) %>% 
  filter(!is.na(Subject)) %>% 
  gather(key = measure, value = value, L_ring_length:R_ring_angle, L2d4d, R2d4d) %>% 
  spread(key = coder, value = value)

# check interrater reliability of 2d4d
filter(cordat, measure == "L2d4d") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise")
filter(cordat, measure == "R2d4d") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise")
# rather poor interrater reliability between RP and everyone else
# and between HS and JS

# check interrater reliability of individual digits
filter(cordat, measure == "R_ring_length") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise") # good
filter(cordat, measure == "R_index_length") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise") # some poor correlations between HS and JS & RP; RP & JS
filter(cordat, measure == "L_ring_length") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise") # some poor correlation between HS and JS & RP
filter(cordat, measure == "L_index_length") %>% 
  select(CN:TG) %>% 
  cor(use = "pairwise") # some real bad correlation between HS and RP & JS
# Poor correlation between HS and RP/JS is caused by tiny overlap (5 subjs)

ggplot(cordat, aes(x = RP, y = HS)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~measure)
ggplot(cordat, aes(x = JS, y = HS)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~measure)

arrange(cordat, desc(abs(RP-HS)))
arrange(cordat, desc(abs(RP-JS)))

# check degree of overlap
filter(cordat, measure == "L2d4d", !is.na(HS), !is.na(RP)) %>% dim()
filter(cordat, measure == "L2d4d", !is.na(HS), !is.na(JS)) %>% dim()
