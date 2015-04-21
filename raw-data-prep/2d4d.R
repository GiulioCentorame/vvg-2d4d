#combine 2d4d measurements and establish interrater reliability
#written by HARD KREW on 2/1/2014 on a day of freezing rain

library(readxl); library(dplyr)
jul<-read_excel("raw-data-prep/raw_data/digits_JS.xlsx", 1)
rac<-read_excel("raw-data-prep/raw_data/digits_RP.xlsx", 1)
tay<-read_excel("raw-data-prep/raw_data/digits_TG.xlsx", 1)
# check colnames
mean(colnames(jul) == colnames(rac) &
       colnames(jul) == colnames(tay)) #should equal 1
# ID coding RA # I'll do it later

# prune missing
jul = jul[complete.cases(jul[,1:9]),]
rac = rac[complete.cases(rac[,1:9]),]
tay = tay[complete.cases(tay[,1:9]),]
# How thorough is the double-coding?
temp = unique(c(jul[,1], rac[,1], tay[,1]))
table(temp %in% jul[,1], temp %in% rac[,1], temp %in% tay[,1])
# rachel's errors: 77 84 148
bad=c(77,84,148)
rac[rac$Subno %in% bad,2:9] = NA
rac$L_index_length[rac$L_index_length < 100] = NA
rac$R_index_length[rac$R_index_length < 100] = NA

# IRR?
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
require(reshape2)
datTemp = rbind(jul, rac, tay); 
datTemp = datTemp[,-10] # setting aside notes for now
molten = melt(datTemp, id.vars = "Subno")
dat = dcast(molten, Subno ~ ... , fun.aggregate=mean, na.rm=T)
dat$julNotes = jul$Notes[match(dat$Subno, jul$Subno)]
dat$racNotes = rac$Notes[match(dat$Subno, rac$Subno)]
dat$tayNotes = tay$Notes[match(dat$Subno, tay$Subno)]
# how many coded? 
t = table(datTemp$Subno)
dat$nCoders = t[match(names(t), dat$Subno)]
# who coded?
# dat$Coders = paste ###

# make 2d4d ratio
dat$L2d4d = dat$L_index_length/dat$L_ring_length
dat$R2d4d = dat$R_index_length/dat$R_ring_length
# rearrange columns
dat = dat[,c(1, 14:15, 2:9, 13, 10:12)]

# check to see if angle influences 2d4d
m1 = lm(L2d4d ~ L_ring_angle, data=dat)
m2 = lm(R2d4d ~ R_ring_angle, data=dat)
m3 = lm(L2d4d ~ L_index_angle, data=dat)
m4 = lm(R2d4d ~ R_index_angle, data=dat)
summary(m1); summary(m2); summary(m3); summary(m4)
# doesn't seem to! That's good.

dir.create("cleaned_data")
write.table(dat, file="raw-data-prep/cleaned_data/2d4d.txt", sep="\t", row.names=F)
