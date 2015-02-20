dat = read.delim("./analysis/aggregated_data.txt", 
               #quote="", 
               sep="\t", 
               stringsAsFactors=F)

# may wish to use column dat$Suspected_Debrief
# dat$Game.play.affect.distraction.time_Debrief or dat$Surprise_Debrief
# I probably ruined these questionnaires by having the RA attempt funneled debriefing and then
  # give the questionnaire... But then, if the funneled debriefing didn't turn up anything,
  # the paper questionnaire should be legit!
# Maybe it's become impossible to collect any damn data in this area...

# how bad is it?

table(dat$Suspected, dat$Good.Session)
table(dat$Game.play.affect.distraction.time_Debrief, dat$Good.Session_Note_sheet)
table(dat$Surprise_Debrief, dat$Good.Session_Note_sheet)
table(dat$X1.a_Debrief, dat$Good.Session_Note_sheet)

# People who guessed "effects of games on aggression" also made more guesses overall
dat[149, 4:5] = 0; for (i in 2:6) dat[,i] = as.numeric(dat[,i])
table(dat$X1.a_Debrief, apply(dat[,2:6], 1, FUN=sum, na.rm=T))

# It looks like RA's decision of whether it was a good session or not
  # had nothing to do with whether or not the subject listed vg & aggression or suspicion of DV

dat1 = dat[dat$X1.a_Debrief == 0,]
dat2 = dat[dat$Good.Session_Note_sheet != "No",]

# Gunk below from previous analysis script.

# and as numerics to check point-biserial correlations...
dat$vioNum = 0; dat$vioNum[dat$Violence=="Violent"] = 1
dat$diffNum = 0; dat$diffNum[dat$Difficulty=="Hard"] = 1
#check 2d4d quality
hist(dat$L2d4d, breaks=15); hist(dat$R2d4d, breaks=15)
# i'm guessing that the one 2d4d at 1.15 is an error.
dat$R2d4d[dat$R2d4d > 1.1] = NA # remove it
#okay that should do it
colnames(dat)

names(dat)[grep("Assignment", names(dat))] = "DV"
hist(dat$DV); summary(dat$DV) # strong ceiling effect. 11 NAs? 
# histograms w/ facet wraps for conditions
require(ggplot2)
ggplot(dat, aes(x=DV)) + 
  geom_histogram(breaks=c(1:9)) +
  facet_wrap(~Good.Session, nrow=2)
hist(dat$DV[dat$Good.Session == "Yes"]); summary(dat$DV[dat$Good.Session == "Yes"])
hist(dat$DV[dat$Good.Session == "No"], breaks=9); summary(dat$DV[dat$Good.Session == "No"])
hist(dat$DV[dat$Good.Session == "Maybe"], breaks=9); summary(dat$DV[dat$Good.Session == "Maybe"])

factorList = c("RA", "Subject", "Station","Condition", "Violence", "Difficulty")
for (i in factorList) dat[,i] = as.factor(dat[,i])

# let's go straight to the good stuff
set = dat # could be "dat", "dat1", "dat2", etc

set = set[!(is.na(set$Violence) | is.na(set$Difficulty) | is.na(set$DV)),]
set$R2d4d[set$R2d4d > 1.1] = NA # R2d4d outlier removal

cor(set[,c("vioNum", "diffNum", "L2d4d", "R2d4d")], use="pairwise.complete.obs") # random assignment good

require(car)
# 3-way w/ left hand. 
#sink(file="ANOVA_results.txt", split=T)
m1 = lm(DV ~ Difficulty * Violence * L2d4d, data=set)
summary(m1); Anova(m1, type=3)
# 3-way w/ right hand
m2 = lm(DV ~ Difficulty * Violence * R2d4d, data=set)
summary(m2); Anova(m2, type=3)
# 2-ways, dropping 2d4d & 3-ways.
m3 = lm(DV ~ Difficulty * Violence, data=set)
summary(m3); Anova(m3, type=3)
#sink()
# 2d4d?
m4 = lm(DV ~ L2d4d, data=set)
summary(m4)
m5 = lm(DV ~ R2d4d, data=set)
summary(m5)
# compare vs no-violence model?
m6 = lm(DV ~ Difficulty + Violence, data=set)
m7 = lm(DV ~ Difficulty, data=set)
m8 = lm(DV ~ Violence, data=set)
m9 = lm(DV ~ 1, data=set)

# trying censored-from-above analysis:
require(censReg)
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
#sink()

# how about binning?
# logistic regression
require(rms)
set$DVbin=NA
set$DVbin[set$DV==9] = 1
set$DVbin[set$DV<9] = 0
#sink("Binomial_results.txt", split=T)
model1 = glm(DVbin ~ Difficulty*Violence*L2d4d, family=binomial(link="logit"), data=set)
summary(model1)
model2 = glm(DVbin ~ Difficulty*Violence*R2d4d, family=binomial(link="logit"), data=set)
summary(model2)
model3 = glm(DVbin ~ Difficulty*Violence, family=binomial, data=set)
summary(model3)
model4 = glm(DVbin ~ L2d4d, family=binomial, data=set)
model5 = glm(DVbin ~ R2d4d, family=binomial, data=set)
model6 = glm(DVbin ~ Difficulty + Violence, family=binomial, data=set)
model7 = glm(DVbin ~ Difficulty, family=binomial, data=set)
model8 = glm(DVbin ~ Violence, family=binomial, data=set)
model9 = glm(DVbin ~ 1, family=binomial, data=set)
# one more, with feeling
model5a = lrm(DVbin ~ Difficulty * Violence, data=set)
model6a = lrm(DVbin ~ Difficulty + Violence, data=set)
model7a = lrm(DVbin ~ Difficulty, data=set)
model8a = lrm(DVbin ~ Violence, data=set)
model9a = lrm(DVbin ~ 1, data=set)

# consider using confint()

sink()

qplot(data=set, x=DV, geom="histogram", facets=(~Violence*Difficulty), 
      main="Histograms of aggression per condition")
qplot(data=set, x=interaction(set$Violence, set$Difficulty), y=DV, notch=T, geom="boxplot", 
      xlab = "Condition assigned", ylab="Aggression")
qplot(data=set, x=interaction(set$Violence, set$Difficulty), y=DV, notch=T, geom="point"
      ,position=position_jitter(w=.25, h=.1), cex=2, alpha=.9
      ,xlab = "Condition assigned", ylab="Aggression")
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
  geom_point(cex=4, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Left-hand 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration assigned (aggression)") +
  # make the text huge
  postertext +
  # remove the background
  theme(panel.background=element_blank(),
        # this part attempts to adjust the axis distance but i'm having trouble
        axis.title.y=element_text(vjust=.2),
        axis.title.x=element_text(vjust=.5)
  )

# scatterplot w/ right-hand 2d4d:
ggplot(data=set, aes(x=R2d4d, y=DV, col=Violence)) +
  geom_point(cex=4, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Right-hand 2d4d ratio") +
  ylab("Coldpressor duration assigned (aggression)") +
  scale_y_discrete(limits=1:9) +
  # make the text huge
  postertext #+
# remove the background
theme(panel.background=element_blank(),
      # this part attempts to adjust the axis distance but i'm having trouble
      axis.title.y=element_text(vjust=.2),
      axis.title.x=element_text(vjust=.5)
)

# scatterplot w/ right-hand 2d4d:
ggplot(data=set, aes(x=R2d4d, y=DV)) +
  geom_point(cex=4, alpha=.75) +
  geom_smooth(method="lm")+
  labs(title="Null effects of 2d4d ratio (right hand)") +
  xlab("Right-hand 2d4d ratio") +
  ylab("Coldpressor duration assigned (level)") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  # make the text huge
  postertext  #+
#   # remove the background
#   theme(panel.background=element_blank(),
#         # this part attempts to adjust the axis distance but i'm having trouble
#         axis.title.y=element_text(vjust=.2),
#         axis.title.x=element_text(vjust=.5)
#         )

# scatterplot w/ left-hand 2d4d:
ggplot(data=set, aes(x=L2d4d, y=DV)) +
  geom_point(cex=4, alpha=.75) +
  geom_smooth(method="lm")+
  labs(title="Null effects of 2d4d ratio (left hand)") +
  xlab("Left-hand 2d4d ratio") +
  ylab("Coldpressor duration assigned (level)") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  # make the text huge
  postertext

# univariate histograms:
ggplot(data=set, aes(x=DV)) +
  geom_histogram() +
  facet_wrap(~Difficulty*Violence) +
  xlab("Coldpressor duration assigned (level)") +
  scale_x_discrete(limits=c(1:9)) +
  ylab("Count") + 
  postertext
# # jitter points
# ggplot(data=set, aes(x=interaction(set$Violence, set$Difficulty), y=DV)) +
#   geom_point(position=position_jitter(width=.25, height=.05), cex=3, alpha=.8) +
#   theme(text = element_text(size=32)) +
#   xlab("Game Condition") +
#   ylab("Coldpressor duration assigned")
# boxplot
ggplot(data=set, aes(x=interaction(set$Violence, set$Difficulty), y=DV)) +
  geom_boxplot(notch=T) +
  postertext +
  theme(axis.text = element_text(size=30)) +
  scale_y_continuous(limits=c(1,9), breaks=c(1,3,5,7,9)) +
  xlab("Game Condition") +
  ylab("Coldpressor duration assigned (level)")


ggplot(data=set, aes(x=L2d4d, y=DV, col=Violence)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Left 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration assigned (aggression)")

ggplot(data=set, aes(x=R2d4d, y=DV, col=Difficulty)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game difficulty?") +
  xlab("Right 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration assigned (aggression)")


require(BayesFactor)

bf1 = anovaBF(DV ~ Violence*Difficulty, data=set, rscaleFixed=.4, iterations=10^5)
# bf2 = anovaBF(DV ~ Violence*Difficulty, data=good1, rscaleFixed=.21, iterations=10^5)
bf3 = lmBF(DV ~ Violence*Difficulty*L2d4d, data=set[!is.na(set$L2d4d),], rscaleFixed=.4, iterations=10^5)
bf4 = lmBF(DV ~ Violence*Difficulty*R2d4d, data=set[!is.na(set$R2d4d),], rscaleFixed=.4, iterations=10^5)
bf5 = lmBF(DV ~ L2d4d, data=set[!is.na(set$L2d4d),], rscaleFixed=.4, iterations=10^5)
bf6 = lmBF(DV ~ R2d4d, data=set[!is.na(set$R2d4d),], rscaleFixed=.4, iterations=10^5)


bf1
bf2
bf3
bf4
bf5
bf6

anovaBF(DV ~ Violence*Difficulty, data=dat1, iterations=10^5)
anovaBF(DV ~ Violence*Difficulty, data=good1, iterations=10^5)

# inspect the shape of these dang ol' priors
r = seq(-1,1,.01)
plot(x=r, y=dcauchy(r, scale=.21), type='l', col='red')
lines(x=r, y=dcauchy(r, scale=.1), type='l')
lines(x=r, y=dcauchy(r, scale=.5), type='l', col='blue')
lines(x=r, y=dcauchy(r, scale=sqrt(2)/2), type='l', col='green')
lines(x=r, y=dcauchy(r, scale=1), type='l', col='orange')

pcauchy(.21, scale=.21, lower.tail=F) #
