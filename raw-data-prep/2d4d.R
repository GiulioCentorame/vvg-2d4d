#combine 2d4d measurements and establish interrater reliability
#written by HARD KREW on 2/1/2014 on a day of freezing rain

require(xlsx)
jul<-read.xlsx(file="./raw_data/digits_JS.xlsx", 1)
rac<-read.xlsx(file="./raw_data/digits_RP.xlsx", 1)
tay<-read.xlsx(file="./raw_data/digits_TG.xlsx", 1)
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
# jul & rac overlap, tay stands alone
# some records are too exactly alike in jul and rac datasets
temp = jul[,2:9] == rac[,2:9]
temp = apply(temp, 1, FUN=mean)
rac[as.logical(temp),1:9]
# removing rac[1:49,], must've been copy-pasted from julian somehow
# not sure what to make of 57, 59, 75, 108, 140, 142, 154
rac = rac[-1:49,]
# pairwise plots reveal rachel must have made some errors
for (i in c(2,4,6,8)) plot(jul[,i], rac[,i])
# rachel's errors: 77 84 148
bad=c(77,84,148)
rac[rac$Subno %in% bad,2:9] = NA
rac$L_index_length[rac$L_index_length < 100] = NA
rac$R_index_length[rac$R_index_length < 100] = NA

####################
# lets evaluate IRR#
####################
# individual digits IRR
cortable=cor(x=jul[,2:9],y=rac[,2:9], use="pairwise.complete.obs")
IRR=diag(cortable)
IRR

# so let's check histograms
for (i in 2:9) hist(jul[,i], main=colnames(jul)[i])
for (i in 2:9) hist(rac[,i], main=colnames(rac)[i])

# pairwise ratio?
L4 = jul[,2]/rac[,2]
L2 = jul[,4]/rac[,4]
R2 = jul[,6]/rac[,6]
R4 = jul[,8]/rac[,8]
summary(L4); summary(L2); summary(R2); summary(R4)
hist(L4); hist(L2); hist(R2); hist(R4)
# may need to attend to obs where ratio is >5% discrepant
d = .05
tempIndex = L4 > 1+d | L2 > 1+d | R2 > 1+d | R4 > 1+d |
            L4 < 1-d | L2 < 1-d | R2 < 1-d | R4 < 1-d
jul[tempIndex,1:8]
rac[tempIndex,1:8]

#pairwise plots
pairplot=function(i) {
  plot(x=jul[,i], y=rac[,i], 
       main=colnames(jul)[i],
       ylab="Rachel coded",
       xlab="Julian coded")
}
for (i in c(2:8)) pairplot(i)
# rachel underestimates L_ring relative to Julian
# L_index well-agreed
# R_index well-agreed
# rachel underestimates R_ring relative to Julian

# Create averages
require(reshape2)
dat = rbind(jul, rac, tay); 
dat = dat[,-10] # setting aside notes for now
molten = melt(dat, id.vars = "Subno")
dat = dcast(molten, Subno ~ ... , fun.aggregate=mean, na.rm=T)
dat$julNotes = jul$Notes[match(dat$Subno, jul$Subno)]
dat$racNotes = rac$Notes[match(dat$Subno, rac$Subno)]
dat$tayNotes = tay$Notes[match(dat$Subno, tay$Subno)]
# how many coded? # I must've forgotten something in here, this isn't working
dat$nCoders = table(dat$Subno)
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
write.table(dat, file="./cleaned_data/2d4d.txt", sep="\t", row.names=F)
