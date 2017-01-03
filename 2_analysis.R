# install.packages(c('rms', 'MBESS', 'censReg', 'dplyr', 'ggplot2', 'BayesFactor'))
library(rms)
library(MBESS)
library(censReg)
library(dplyr)
library(ggplot2)
library(BayesFactor)
source("../joe-package/joe-package.R")

# Data import ----
dat = read.delim("clean_data.txt", stringsAsFactors = F)
# Convert relevant columns to factors
factorList = c("RA", "Subject", "Station","Condition", "Violence", "Difficulty")
for (i in factorList) dat[,i] = as.factor(dat[,i])
# Contrast code violence and difficulty (-1, 1)
dat$Violence = factor(dat$Violence) %>% C(sum)
dat$Difficulty = factor(dat$Difficulty) %>% C(sum)
# discard conflicts
dat[dat == -999] = NA
# Make centered 2d4ds
dat$L2d4d_c = dat$L2d4d - mean(dat$L2d4d, na.rm = T)
dat$R2d4d_c = dat$R2d4d - mean(dat$R2d4d, na.rm = T)

# Create composites: irritation, challenge ----

# Irritation
# Get complete cases for PCA
set.pca = dat %>% 
  select(Subject, irritated:annoyed) %>% 
  filter(complete.cases(.))
# Perform PCA
manip.pca = set.pca %>% 
  select(irritated:annoyed) %>% 
  princomp(center = T, scale = T)
print(manip.pca$loadings)
# Append PCA1 scores to set.pca
set.pca$composite_irritation = manip.pca$scores[,1]
# Drop redundant columns
set.pca = select(set.pca, Subject, composite_irritation)
# Append those scores to full dataset
dat = left_join(dat, set.pca)

# Challenge
# Get complete cases for PCA
set.pca2 = dat %>% 
  select(Subject, 
         easy.nav, challenging, stressful, diff.nav, reflexes, difficult, 
         good.fight, hard.control, mental.effort, comfort.control, exhausting) %>% 
  filter(complete.cases(.)) 
# Perform PCA
manip.pca2 = set.pca2 %>% 
  select(easy.nav:exhausting) %>% 
  princomp(center = T, scale = T)
# TODO: Consider these loadings. Component 1 doesn't seem to catch it all. Factor rotation more appropriate?
print(manip.pca2$loadings)
# Append PCA1 scores to set.pca
set.pca2$composite_challenge = manip.pca2$scores[,1]
# Drop redundant columns
set.pca2 = select(set.pca2, Subject, composite_challenge)
# Append those scores to full dataset
dat = left_join(dat, set.pca2)

# Make means and sds ----
means = sapply(dat, mean, na.rm = T)
sds = sapply(dat, sd, na.rm = T)

# Make correlation table ----
# Make numerics to check point-biserial correlations...
dat$vioNum = ifelse(dat$Violence == "Violent", 1, 0)
dat$diffNum = ifelse(dat$Difficulty == "Hard", 1, 0)
# Make corrleation table to check random assignment
cor(dat[,c("vioNum", "diffNum", "L2d4d", "R2d4d")], use="pairwise.complete.obs") 
# Could make further cor tables from here.

# Manipulation checks ----
# Violent content manipulation?
check1 = aov(violence ~ Violence*Difficulty, data=dat)
summary(check1)
# effect size d
vioMeans = tapply(dat$violence, dat$Violence, mean, na.rm=T)
vioSDs = tapply(dat$violence, dat$Violence, sd, na.rm=T)
vioN = table(dat$Violence)
difN = table(dat$Difficulty)
denom = pool.sd(vioSDs, vioN)
num = vioMeans[2] - vioMeans[1]
d = num/denom
ci.smd(smd = (vioMeans[2] - vioMeans[1]) / pool.sd(vioSDs, vioN), 
       n.1 = vioN[1], n.2 = vioN[2])

# Difficulty manipulation?
# manip check
manipCheckDifficulty = lm(composite_challenge ~ Violence*Difficulty, data=dat) %>% summary()
manipCheckDifficulty
# ncp is "generally the observed t-statistic from comparing the two groups
# see ?ci.smd
ci.smd(ncp = manipCheckDifficulty$coefficients["Difficulty1", 3],
       n.1 = difN[1], n.2 = difN[2])

# Irritation and DV
check2 = lm(DV ~ composite_irritation, data=dat)
summary(check2)
t2R(tstat = summary(check2)$coefficients[2,3], 
    N     = summary(check2)$df[2])

# Irritation not fostered by game violence
lm(composite_irritation ~  Violence * Difficulty, data = dat) %>%
  summary

# Gameplay variables
dat %>%
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

# ANOVA models of primary outcome ----
# Full model, left hand
m1 = lm(DV ~ Difficulty * Violence * L2d4d_c, data = dat)
summary(m1)
# Full model, right hand
m2 = lm(DV ~ Difficulty * Violence * R2d4d_c, data = dat)
summary(m2)
# 2x2 ANOVA model
m3 = lm(DV ~ Difficulty * Violence, data = dat)
summary(m3)
t2R(.963, 291)
t2R(1.13, 291)
# I wonder: how do I generate a p-value against H0: r = .21?
# (z.obs - z.H0) / se(z) ~ z
(atanh(.05655639) - atanh(.21)) / (1/sqrt(294-3))
pnorm(-2.67, lower.tail=T)
# elaboration via simple slopes:
m3.1 = lm(DV ~ Violence, data=dat[dat$Difficulty == "Easy",])
m3.2 = lm(DV ~ Violence, data=dat[dat$Difficulty == "Hard",])
summary(m3.1)
t2R(1.788, 145)
summary(m3.2)
t2R(.422, 148)

# Adding composite irritation component as covariate
m3.5 = lm(DV ~ Difficulty * Violence + composite_irritation, data=dat)
summary(m3.5) # soaks up a lot of variance but effect of violence still not significant
t2R(.735, 269)

# 2d4d alone:
m4 = lm(DV ~ L2d4d, data = dat)
summary(m4)
t2R(-.729, 272)
m5 = lm(DV ~ R2d4d, data = dat)
summary(m5)
t2R(-.35, 273)

# Bayesian models of primary outcome ----
# ANOVA
bf1 = anovaBF(DV ~ Violence*Difficulty, data=dat, rscaleFixed=.4, iterations=10^5)
bf1
1/bf1
bfList = 1/exp(bf1@bayesFactor$bf)

# linear models considering L2d4d
set = dat %>% filter(!is.na(dat$L2d4d))
bf3 = lmBF(DV ~ Violence*Difficulty*L2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf3.1 = lmBF(DV ~ Difficulty*L2d4d + Violence*L2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf3.2 = lmBF(DV ~ Difficulty*L2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf3.3 = lmBF(DV ~ Violence*L2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf3.4 = lmBF(DV ~ L2d4d, data=set, rscaleFixed=.4, iterations=10^5)
plot(c(bf3, bf3.1, bf3.2, bf3.3, bf3.4), marginExpand = .3)

# linear models considering R2d4d
set = dat %>% filter(!is.na(dat$R2d4d))
bf4 = lmBF(DV ~ Violence*Difficulty*R2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf4.1 = lmBF(DV ~ Difficulty*R2d4d + Violence*R2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf4.2 = lmBF(DV ~ Difficulty*R2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf4.3 = lmBF(DV ~ Violence*R2d4d, data=set, rscaleFixed=.4, iterations=10^5)
bf4.4 = lmBF(DV ~ R2d4d, data=set, rscaleFixed=.4, iterations=10^5)
plot(c(bf4, bf4.1, bf4.2, bf4.3, bf4.4), marginExpand = .3)


# linear models considering composite irritation covariate
set2 = dat %>% 
  select(DV, Violence, Difficulty, composite_irritation) %>% 
  filter(complete.cases(.))
bf7 = lmBF(DV ~ Violence*Difficulty + composite_irritation, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.1 = lmBF(DV ~ Violence + Difficulty + composite_irritation, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.2 = lmBF(DV ~ Violence + composite_irritation, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.3 = lmBF(DV ~ Difficulty + composite_irritation, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.4 = lmBF(DV ~ composite_irritation, data=set2, rscaleFixed=.4, iterations=10^5)
# Results
c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)
plot(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4))
# Results relative to covariate-only model
c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4
1/(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4)
plot(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4)

# TODO: add code for Dienes calculator. 
# Compare observed effect size (w/ and w/o covariate) against H3: r = .21 +/- some change


# Censored-from-above analysis ----
censModel1 = censReg(DV ~ Difficulty*Violence*L2d4d, left=1, right=9, data=dat)
summary(censModel1)
# adding covariate
censModel1.1 = censReg(DV ~ Difficulty*Violence*L2d4d + composite_irritation,
                       left=1, right=9, data=dat)
summary(censModel1.1)
# 3-way w/ right hand
censModel2 = censReg(DV ~ Difficulty*Violence*R2d4d, left=1, right=9, data=dat)
summary(censModel2)
# adding covariate
censModel2.1 = censReg(DV ~ Difficulty*Violence*R2d4d + composite_irritation,
                       left=1, right=9, data=dat)
summary(censModel2.1)
# 2-ways, dropping 2d4d & 3-ways
censModel3 = censReg(DV ~ Difficulty*Violence, left=1, right=9, data=dat)
summary(censModel3)
# adding covariate
censModel3.1 = censReg(DV ~ Difficulty*Violence + composite_irritation,
                       left=1, right=9, data=dat)
summary(censModel3.1)

# Logistic regression w/ binned DV ----
dat$DVbin = ifelse(dat$DV == 9, 1, 0)
# With L2d4d
model1 = glm(DVbin ~ Difficulty*Violence*L2d4d, family=binomial(link="logit"), data=dat)
summary(model1)
# With L2d4d + covariate
model1.1 = glm(DVbin ~ Difficulty*Violence*L2d4d + composite_irritation, 
               family=binomial(link="logit"), data=dat)
summary(model1.1)
# With R2d4d
model2 = glm(DVbin ~ Difficulty*Violence*R2d4d, family=binomial(link="logit"), data=dat)
summary(model2)
# With R2d4d + covariate
model2.1 = glm(DVbin ~ Difficulty*Violence*R2d4d + composite_irritation,
               family=binomial(link="logit"), data=dat)
summary(model2.1)
# 2x2 ANOVA
model3 = glm(DVbin ~ Difficulty*Violence, family=binomial(link = "logit"), data=dat)
summary(model3)
# 2x2 ANCOVA (irritation covariate)
model3.5 = glm(DVbin ~ Difficulty*Violence + composite_irritation,
               family=binomial(link = "logit"), data=dat)
summary(model3.5)

save.image()