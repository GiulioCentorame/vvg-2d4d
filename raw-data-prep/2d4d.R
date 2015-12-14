#combine 2d4d measurements and establish interrater reliability
#written by HARD KREW on 2/1/2014 on a day of freezing rain

library(readxl)
library(dplyr)
library(tidyr)

# Import data
jul<-read_excel("raw-data-prep/raw_data/digits_JS.xlsx", 1)
rac<-read_excel("raw-data-prep/raw_data/digits_RP.xlsx", 1)
tay<-read_excel("raw-data-prep/raw_data/digits_TG.xlsx", 1)

# Scrape off empty rows
jul = jul[complete.cases(jul[,1:9]),]
rac = rac[complete.cases(rac[,1:9]),]
tay = tay[complete.cases(tay[,1:9]),]

# add coder name to data
jul$coder = "Julian"
rac$coder = "Rachel"
tay$coder = "Taylor"

# check colnames
stopifnot(
  mean(colnames(jul) == colnames(rac) & colnames(jul) == colnames(tay)) == 1
)

# bind data and sort by subno
dat = bind_rows(jul, rac, tay) %>% 
  arrange(Subno) 
# make 2d4d ratio
dat$L2d4d = dat$L_index_length/dat$L_ring_length
dat$R2d4d = dat$R_index_length/dat$R_ring_length

# Check rigor of double-coding
doubleCoding = dat %>% 
  select(Subno, coder) %>% 
  group_by(Subno) %>% 
  summarize(codings = length(coder),
            coders = toString(coder))
# View(doubleCoding)

# How thorough is the double-coding?
table(doubleCoding$coders) # could be more thorough. Need some more staffing.

# Scrape off bad rows
dat = dat %>% 
  filter(L_ring_length > 500,
         L_index_length > 500,
         R_ring_length > 500,
         R_index_length > 500,
         L2d4d < 1.2,
         L2d4d > .8,
         R2d4d < 1.2,
         R2d4d > .8,
         abs(R2d4d - L2d4d) < .25)

# IRR. Couldn't find a more graceful way to do it
# This doesn't include my data cleaning, though. ugh
icc = data.frame("Subno" = 1:450)
rac1 = rac
tay1 = tay
names(rac1) = paste(names(rac1), "_R", sep="")
names(tay1) = paste(names(tay1), "_T", sep="")
icc = cbind(icc, jul[match(icc$Subno, jul$Subno),2:9])
icc = cbind(icc, rac1[match(icc$Subno, rac1$Subno),2:9])
icc = cbind(icc, tay1[match(icc$Subno, tay1$Subno),2:9])
cor(icc[,-1], use="pairwise.complete") %>%
  round(3)

# Create averages
datAvg = dat %>% 
  group_by(Subno) %>% 
  summarise(L2d4d = mean(L2d4d),
            R2d4d = mean(R2d4d),
            L_ring_length = mean(L_ring_length),
            L_ring_angle = mean(L_ring_angle),
            L_index_length = mean(L_index_length),
            L_index_angle = mean(L_index_angle),
            R_ring_length = mean(R_ring_length),
            R_ring_angle = mean(R_ring_angle),
            R_index_length = mean(R_index_length),
            R_index_angle = mean(R_index_angle),
            notes = toString(Notes),
            coder = toString(coder))
  
# check to see if angle influences 2d4d
m1 = lm(L2d4d ~ L_ring_angle, data=datAvg)
m2 = lm(R2d4d ~ R_ring_angle, data=datAvg)
m3 = lm(L2d4d ~ L_index_angle, data=datAvg)
m4 = lm(R2d4d ~ R_index_angle, data=datAvg)
summary(m1); summary(m2); summary(m3); summary(m4)
# doesn't seem to! That's good.

dir.create("cleaned_data")
write.table(datAvg, file="raw-data-prep/cleaned_data/2d4d.txt", sep="\t", row.names=F)
