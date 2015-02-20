# pairwise plots reveal rachel must have made some errors
for (i in c(2,4,6,8)) plot(jul[50:nrow(jul),i], rac[,i])
# rachel's errors: 77 84 148
bad=c(77,84,148)
rac[rac$Subno %in% bad,2:9] = NA
rac$L_index_length[rac$L_index_length < 100] = NA
rac$R_index_length[rac$R_index_length < 100] = NA

####################
# lets evaluate IRR#
####################
# individual digits IRR
cortable=cor(x=jul[50:nrow(jul),2:9],y=rac[,2:9], use="pairwise.complete.obs")
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
