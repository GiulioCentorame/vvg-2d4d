#combine 2d4d measurements and establish interrater reliability
#written by HARD KREW on 2/1/2014 on a day of freezing rain

jul<-read.xlsx(file="digits_JS.xlsx", 1)
rac<-read.xlsx(file="digits_RP.xlsx", 1)
tay<-read.xlsx(file="digits_TG.xlsx", 1)
mean(colnames(jul) == colnames(rac) &
       colnames(jul) == colnames(tay)) #should equal 1
# pairwise plots reveal rachel must have made some errors
# rachel's errors: 77 84 148
bad=c(77,84,148)
rac[rac$Subno %in% bad,] = NA
rac$L_index_length[rac$L_index_length < 100] = NA
rac$R_index_length[rac$R_index_length < 100] = NA


####################
# lets evaluate IRR#
####################
# individual digits IRR
cortable=cor(x=jul[,1:11],y=rac[,1:11], use="pairwise.complete.obs")
IRR=diag(cortable)
IRR



# something's wrong (IRR = .3) so let's check histograms
for (i in 2:11) hist(jul[,i], main=colnames(jul)[i])
for (i in 2:11) hist(rac[,i], main=colnames(rac)[i])

# pairwise ratio?
L4 = jul[,2]/rac[,2]
L2 = jul[,4]/rac[,4]
R2 = jul[,6]/rac[,6]
R4 = jul[,8]/rac[,8]
summary(L4); summary(L2); summary(R2); summary(R4)
hist(L4); hist(L2); hist(R2); hist(R4)
#pairwise plots
pairplot=function(i) {
  plot(x=jul[,i], y=rac[,i], 
       main=colnames(jul)[i],
       ylab="Rachel coded",
       xlab="Julian coded")
}
for (i in c(2:11)) pairplot(i)
# rachel underestimates L_ring relative to Julian
# L_index well-agreed
# R_index well-agreed
# rachel underestimates R_ring relative to Julian
# maybe exclude obs with disagreement > 5%?

# current IRRs: Left: 0.828, Right: 0.846

# average them together for export
dat = (jul[,1:11] + rac[,1:11])/2
dat$noteJul = jul[,12]; dat$noteRac = rac[,12]

# check to see if angle influences 2d4d
m1 = lm(L_2d4d ~ L_ring_angle, data=dat)
m2 = lm(R_2d4d ~ R_ring_angle, data=dat)
m3 = lm(L_2d4d ~ L_index_angle, data=dat)
m4 = lm(R_2d4d ~ R_index_angle, data=dat)
summary(m1); summary(m2); summary(m3); summary(m4)
# doesn't seem to! That's good.

print("HARD KREW, you have to save manually!")
#write.table(dat, file="2d4d.txt", sep="\t", row.names=F)
