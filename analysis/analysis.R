# install.packages(c('rms', 'MBESS', 'censReg', 'magrittr', 'ggplot2'))
library(rms)
library(MBESS)
library(censReg)
library(dplyr)
library(magrittr)
library(ggplot2)
library(BayesFactor)
source("../joe-package/F2R.R")
bigtext =   theme(axis.title = element_text(size=14),
                  plot.title = element_text(size=16))


dat = read.delim("./analysis/aggregated_data.txt", 
               #quote="", 
               sep="\t", 
               stringsAsFactors=F)
names(dat)[names(dat)=="Assignment"] = "DV"
factorList = c("RA", "Subject", "Station","Condition", "Violence", "Difficulty")
for (i in factorList) dat[,i] = as.factor(dat[,i])

# Make sure no one took damage in easy condition
table(dat$Game.6, dat$Difficulty)
dat %>%
  filter(Difficulty == "Easy", Game.1 > 0) %>%
  select(Subject)

# Big list of possible subject exclusion filters
dat.pure =
  dat %>%
  filter(!is.na(DV), !is.na(Violence), !is.na(Difficulty)) %>%
  filter(is.na(Game.1) | !(Difficulty == "Easy" & Game.1 > 0)) %>% # died in easy-game condition
  filter(is.na(Game.6) | !(Difficulty == "Easy" & Game.6 > 0)) %>% # took damage in easy-game condition
  filter(is.na(X1.a) | !(X1.a == 1 & (X1.b == 0 & X1.c == 0 & X1.d == 0 & X1.e == 0)))  %>% # called hypothesis w/o wrong guesses
  filter(is.na(Good.Session) | Good.Session != "No") # RAs didn't think session went well

# may wish to use column dat$Suspected_Debrief
# dat$Game.play.affect.distraction.time_Debrief or dat$Surprise_Debrief
# I probably ruined these questionnaires by having the RA attempt funneled debriefing and then
  # give the questionnaire... But then, if the funneled debriefing didn't turn up anything,
  # the paper questionnaire should be legit!
# Maybe it's become impossible to collect any damn data in this area...

# It looks like RA's decision of whether it was a good session or not
  # had nothing to do with whether or not the subject listed vg & aggression or suspicion of DV

# and as numerics to check point-biserial correlations...
dat$vioNum = 0; dat$vioNum[dat$Violence=="Violent"] = 1
dat$diffNum = 0; dat$diffNum[dat$Difficulty=="Hard"] = 1
cor(dat[,c("vioNum", "diffNum", "L2d4d", "R2d4d")], use="pairwise.complete.obs") # random assignment good
#check 2d4d quality
hist(dat$L2d4d, breaks=15); hist(dat$R2d4d, breaks=15)
# i'm guessing that the one 2d4d at 1.15 is an error.
dat$R2d4d[dat$R2d4d > 1.1] = NA # remove it
#okay that should do it
colnames(dat)

hist(dat$DV); summary(dat$DV) # strong ceiling effect. 11 NAs? 

set = dat.pure # could be "dat", "dat1", "dat2", etc

set = set[!(is.na(set$Violence) | is.na(set$Difficulty) | is.na(set$DV)),]
set$R2d4d[set$R2d4d > 1.1] = NA # R2d4d outlier removal

# histograms w/ facet wraps for conditions
ggplot(dat.pure, aes(x=DV)) + 
  geom_histogram(breaks=c(1:9)) +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14))
ggsave("DV-condition_hist.png", width = 5.5, height = 4, units="in")

ggplot(dat.pure, aes(x=DV, fill=Violence)) + 
  geom_histogram(breaks=c(1:9)) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_fill_hue(h.start=180-15,direction=-1) +
  theme(legend.position="none")

ggplot(dat.pure, aes(x=interaction(Difficulty, Violence), y=DV)) + 
  geom_violin(aes(fill = Violence)) +
  #geom_boxplot(width=.15, notch=T) +
  stat_summary(fun.y="mean", geom="point", shape=20, size=10, col="black") +
  stat_summary(fun.y="median", geom="point", shape=20, size=6, col="white") +
  scale_x_discrete("Condition") +
  theme(legend.position="none",
        axis.text.x = element_text(color="black", size=12)
  )

# Manipulation check
dat.pure$violence = as.integer(dat.pure$violence)
check1 = aov(violence ~ Violence, data=dat.pure)
summary(check1)
temp1 = tapply(dat.pure$violence, dat.pure$Violence, mean, na.rm=T)
temp2 = tapply(dat.pure$violence, dat.pure$Violence, sd, na.rm=T)
temp3 = table(dat.pure$Violence)
denom = pool.sd(temp2, temp3)
num = temp1[2] - temp1[1]
d = num/denom
ci.smd(smd=d, n.1=temp3[1], n.2=temp3[2])
# plot it
ggplot(dat.pure, aes(x=violence)) +
  geom_histogram(breaks=1:7) +
  facet_wrap(~Violence) +
  scale_x_discrete("Rated violent content", limits=1:7) +
  scale_y_continuous("Count") +
  ggtitle("Violent Content Manipulation Check") + 
  bigtext
ggsave("violence-condition_hist.png", width = 5.5, height = 4, units="in")


# Does manip check influence aggression?
set.pca = set[complete.cases(set[,35:40]),]
manip.pca = princomp(set.pca[,35:40], center = T, scale =T)
set.pca$factor1 = manip.pca$scores[,1]
set.pca = select(set.pca, Subject, factor1)

set = left_join(set, set.pca)

check2 = lm(DV ~ factor1, data=set)
summary(check2)
t2R(5.366, 198)
ggplot(set, aes(x=factor1, y = DV)) +
  geom_point(cex=2, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth() +
  scale_x_continuous("Composite irritation (1st principal component)")+
  scale_y_continuous("Coldpressor duration") +
  ggtitle("Dependent Variable is Sensitive") +
  bigtext
ggsave("DV-PCA_scatter.png", width = 5.5, height = 4, units="in")

sink(file="manipcheck_ANOVA.txt")
lm(factor1 ~  Violence*Difficulty, data = set) %>%
  summary %>%
  print
sink()

for (i in 27:32) set[,i] = as.numeric(set[,i])
set %>%
  select(Violence, Difficulty, Game.1:Game.6) %>%
  group_by(Violence, Difficulty) %>%
  dplyr::summarize("Deaths" = mean(Game.1, na.rm=T),
            "Deaths.sd" = sd(Game.1, na.rm=T),
            "Kills" = mean(Game.2, na.rm=T),
            "Kills.sd" = sd(Game.2, na.rm=T),
            "Distance" = mean(Game.3, na.rm=T),
            "Distance.sd" = sd(Game.3, na.rm=T),
            "Chaingun" = mean(Game.4, na.rm=T),
            "Chaingun.sd" = sd(Game.4, na.rm=T),
            "Shotgun" = mean(Game.5, na.rm=T),
            "Shotgun.sd" = sd(Game.5, na.rm=T),
            "Wounds" = mean(Game.6, na.rm=T),
            "Wounds.sd" = sd(Game.6, na.rm=T)
            ) %>%
  write.table("Gamevars.txt", sep="\t", row.names=F)

set %>%
  select(DV, Violence, Difficulty) %>%
  group_by(Violence, Difficulty) %>%
  dplyr::summarize("Coldpressor" = mean(DV, na.rm=T),
            "Coldpressor.sd" = sd(DV, na.rm=T)
            ) %>%
  write.table("DV_means.txt", sep="\t", row.names=F)

for (i in 35:40) dat.pure[,i] = as.numeric(dat.pure[,i])
apply(dat.pure[,35:40], 2, mean, na.rm=T)


# let's go straight to the good stuff

#library(car)
# 3-way w/ left hand. 
#sink(file="ANOVA_results.txt", split=T)
m1 = lm(DV ~ Difficulty * Violence * L2d4d, data=set)
summary(m1); #Anova(m1, type=3)
# 3-way w/ right hand
m2 = lm(DV ~ Difficulty * Violence * R2d4d, data=set)
summary(m2); #Anova(m2, type=3)
# 2-ways, dropping 2d4d & 3-ways.
m3 = lm(DV ~ Difficulty * Violence, data=set)
summary(m3); #Anova(m3, type=3)
# elaboration via simple slopes:
m3.1 = lm(DV ~ Violence, data=set[set$Difficulty == "Easy",])
m3.2 = lm(DV ~ Violence, data=set[set$Difficulty == "Hard",])
summary(m3.1)
t2R(2.101, 111)
summary(m3.2)
t2R(-1.05, 112)
# main effects w/ interaction in
t2R(2.193, 223) # difficulty
t2R(2.044, 223) # Violence
# and adding irritation component as covariate
m3.5 = lm(DV ~ Difficulty * Violence + factor1, data=set)
summary(m3.5)
t2R(1.812, 198) # Difficulty
t2R(1.398, 198) # Violence
t2R(-1.620, 198) # Interaction

# 2d4d?
m4 = lm(DV ~ L2d4d, data=set)
summary(m4)
m5 = lm(DV ~ R2d4d, data=set)
summary(m5)
# compare vs no-violence model?
m6 = lm(DV ~ Difficulty + Violence, data=set)
summary(m6)
# effect sizes in additive model?
t2R(.887, 223) # difficulty
t2R(.673, 223) # Violence
ci.smd(ncp=.673, n.1 = temp3[1], n.2 = temp3[2])
# additive model with covariate?
m6.5 = lm(DV ~ Difficulty + Violence + factor1, data=set)
summary(m6.5)
t2R(.934, 198) # difficulty
t2R(.356, 198) # Violence
# interactive model with covariate?
m6.6 = lm(DV ~ Difficulty*Violence + factor1, data=set)
summary(m6.6)
t2R(1.811, 198) # difficulty
t2R(1.398, 198) # Violence
t2R(-1.543, 198) # interaction

m7 = lm(DV ~ Difficulty, data=set)
m8 = lm(DV ~ Violence, data=set)
m9 = lm(DV ~ 1, data=set)


## Bayesian Analysis

bf1 = anovaBF(DV ~ Violence*Difficulty, data=set, rscaleFixed=.4, iterations=10^5)
bf3 = lmBF(DV ~ Violence*Difficulty*L2d4d, data=set[!is.na(set$L2d4d),], rscaleFixed=.4, iterations=10^5)
bf4 = lmBF(DV ~ Violence*Difficulty*R2d4d, data=set[!is.na(set$R2d4d),], rscaleFixed=.4, iterations=10^5)
bf5 = lmBF(DV ~ L2d4d, data=set[!is.na(set$L2d4d),], rscaleFixed=.4, iterations=10^5)
bf6 = lmBF(DV ~ R2d4d, data=set[!is.na(set$R2d4d),], rscaleFixed=.4, iterations=10^5)
set2 = set[complete.cases(set[,c("Violence", "Difficulty", "factor1")]),]
bf7 = lmBF(DV ~ Violence*Difficulty + factor1, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.1 = lmBF(DV ~ Violence + Difficulty + factor1, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.2 = lmBF(DV ~ Violence + factor1, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.3 = lmBF(DV ~ Difficulty + factor1, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.4 = lmBF(DV ~ factor1, data=set2, rscaleFixed=.4, iterations=10^5)



c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4
1/(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4)
plot(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4)
#names(bf1)$numerator=c("Violence", "Interactive", "Difficulty", "Additive")
plot(bf1, marginExpand=0.28)
plot(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4))

1/bf1
1/bf3
1/bf4
1/bf5
1/bf6

# trying censored-from-above analysis:
# 3-way with left hand
#sink("Censored-from-above_results.txt")
censModel1 = censReg(DV ~ Difficulty*Violence*L2d4d, left=1, right=9, data=set)
summary(censModel1)
# 3-way w/ right hand
censModel2 = censReg(DV ~ Difficulty*Violence*R2d4d, left=1, right=9, data=set)
summary(censModel2)
# 2-ways, dropping 2d4d & 3-ways
censModel3 = censReg(DV ~ Difficulty*Violence, left=1, right=9, data=set)
summary(censModel3)
t2R(2.41, 223)
t2R(1.84, 223)
# simple slopes
censModel3.1 = censReg(DV ~ Violence, left=1, right=9, data=set[set$Difficulty=="Easy",])
censModel3.2 = censReg(DV ~ Violence, left=1, right=9, data=set[set$Difficulty=="Hard",])
summary(censModel3.1); t2R(1.945, 111)
summary(censModel3.2); t2R(-1.321, 112)
# additive
censModel4 = censReg(DV ~ Difficulty+Violence, left=1, right=9, data=set)
summary(censModel4)
t2R(1.15, 223)
t2R(.344, 223)
# 2d4d?
censModel5 = censReg(DV ~ L2d4d, dat=set)
summary(censModel5); t2R(-.194, 153)
censModel6 = censReg(DV ~ R2d4d, dat=set)
summary(censModel6); t2R(.13, 153)


# how about binning?
# logistic regression
set$DVbin=NA
set$DVbin[set$DV==9] = 1
set$DVbin[set$DV<9] = 0
#sink("Binomial_results.txt", split=T)
model1 = glm(DVbin ~ Difficulty*Violence*L2d4d, family=binomial(link="logit"), data=set)
summary(model1)
model1.1 = glm(DVbin ~ Difficulty*Violence*L2d4d + factor1, family=binomial(link="logit"), data=set)
summary(model1.1)
model2 = glm(DVbin ~ Difficulty*Violence*R2d4d, family=binomial(link="logit"), data=set)
summary(model2)
model2.1 = glm(DVbin ~ Difficulty*Violence*R2d4d + factor1, family=binomial(link="logit"), data=set)
summary(model2.1)
# 2x2
model3 = glm(DVbin ~ Difficulty*Violence, family=binomial, data=set)
summary(model3)
t2R(-1.599, 223)
t2R(2.212, 223) # Difficulty
t2R(.716, 223) # Violence
  # simple slopes
model3.1 = glm(DVbin ~ Violence, family=binomial, data=set[dat$Difficulty=="Easy",])
model3.2 = glm(DVbin ~ Violence, family=binomial, data=set[dat$Difficulty=="Hard",])
summary(model3.1); t2R(-.626, 105)
summary(model3.2); t2R(-.284, 105)
  # covariate
model3.5 = glm(DVbin ~ Difficulty*Violence + factor1, family=binomial, data=set)
summary(model3.5)

model4 = glm(DVbin ~ L2d4d, family=binomial, data=set)
summary(model4); t2R(-.149, 153)
model4.1 = glm(DVbin ~ L2d4d + factor1, family=binomial, data=set); summary(model4.1)
model5 = glm(DVbin ~ R2d4d, family=binomial, data=set)
summary(model5); t2R(-.659, 153)
model5.1 = glm(DVbin ~ L2d4d, family=binomial, data=set); summary(model5.1)
model6 = glm(DVbin ~ Difficulty + Violence, family=binomial, data=set)
summary(model6)
t2R(1.577, 223) # difficulty
t2R(-.704, 223) # violence
model7 = glm(DVbin ~ Difficulty, family=binomial, data=set)
model8 = glm(DVbin ~ Violence, family=binomial, data=set)
model9 = glm(DVbin ~ 1, family=binomial, data=set)
# one more, with feeling
model5a = lrm(DVbin ~ Difficulty * Violence, data=set)
model6a = lrm(DVbin ~ Difficulty + Violence, data=set)
model7a = lrm(DVbin ~ Difficulty, data=set)
model8a = lrm(DVbin ~ Violence, data=set)
model9a = lrm(DVbin ~ 1, data=set)




qplot(data=set, x=L2d4d, y=DV, col=Violence, geom=c("point", "smooth"))
qplot(data=set, x=R2d4d, y=DV, col=Violence, geom=c("point", "smooth"))

# the best of the best:
postertext = theme(text = element_text(size=16),
                   axis.title = element_text(size=24),
                   strip.text = element_text(size=28),
                   plot.title = element_text(size=32)
)


# scatterplot w/ left-hand 2d4d:
ggplot(data=set, aes(x=L2d4d, y=DV, col=Violence)) +
  geom_point(cex=1, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Left-hand 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration") +
  scale_y_discrete(limits=1:9) +
  bigtext
  # make the text huge
  # postertext +
  # remove the background
  #theme(panel.background=element_blank(),
        # this part attempts to adjust the axis distance but i'm having trouble
  #      axis.title.y=element_text(vjust=.2),
  #      axis.title.x=element_text(vjust=.5)
  #)

# scatterplot w/ right-hand 2d4d:
ggplot(data=set, aes(x=R2d4d, y=DV, col=Violence)) +
  geom_point(cex=1, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Right-hand 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration") +
  scale_y_discrete(limits=1:9) +
  # make the text huge
  bigtext #+
# remove the background
theme(panel.background=element_blank(),
      # this part attempts to adjust the axis distance but i'm having trouble
      axis.title.y=element_text(vjust=.2),
      axis.title.x=element_text(vjust=.5)
)

# faceted scatterplot w/ right-hand 2d4d:
ggplot(data=set, aes(x=R2d4d, y=DV)) +
  geom_point(cex=1, alpha=.75) +
  geom_smooth(method="lm")+
  labs(title="Null effects of 2d4d ratio (right hand)") +
  xlab("Right-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  bigtext
  # make the text huge
#  postertext  #+
#   # remove the background
#   theme(panel.background=element_blank(),
#         # this part attempts to adjust the axis distance but i'm having trouble
#         axis.title.y=element_text(vjust=.2),
#         axis.title.x=element_text(vjust=.5)
#         )
ggsave("r2d4d_x_2x2.png", width=4, height=3, units="in")

# faceted scatterplot w/ left-hand 2d4d:
ggplot(data=set, aes(x=L2d4d, y=DV)) +
  geom_point(cex=1, alpha=.75) +
  geom_smooth(method="lm")+
  labs(title="Null effects of 2d4d ratio (left hand)") +
  xlab("Left-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  bigtext  #+
#   # remove the background
#   theme(panel.background=element_blank(),
#         # this part attempts to adjust the axis distance but i'm having trouble
#         axis.title.y=element_text(vjust=.2),
#         axis.title.x=element_text(vjust=.5)
#         )
ggsave("l2d4d_x_2x2.png", width=4, height=3, units="in")

# # jitter points
# ggplot(data=set, aes(x=interaction(set$Violence, set$Difficulty), y=DV)) +
#   geom_point(position=position_jitter(width=.25, height=.05), cex=3, alpha=.8) +
#   theme(text = element_text(size=32)) +
#   xlab("Game Condition") +
#   ylab("Coldpressor duration assigned")
# boxplot

