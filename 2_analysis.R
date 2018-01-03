library(rms)
library(MBESS)
library(censReg)
library(tidyverse)
library(BayesFactor)
library(psych)
# install.packages('devtools'); library(devtools); install_github("Joe-Hilgard/hilgard")
library(hilgard)

# Data import ----
dat = read.delim("clean_data.txt", stringsAsFactors = F)
# Convert relevant columns to factors
factorList = c("RA", "Subject", "Station","Condition", "Violence", "Difficulty")
for (i in factorList) dat[,i] = as.factor(dat[,i])
# Contrast code violence and difficulty (-1, 1)

dat$Violence = factor(dat$Violence) %>% relevel("Violent") %>% C(sum)
dat$Difficulty = factor(dat$Difficulty) %>% relevel("Hard") %>% C(sum)
# discard conflicts
dat[dat == -999] <- NA
dat[dat == "CONFLICT!"] <- NA
# Make standardized 2d4ds
dat$L2d4d_std = dat$L2d4d - mean(dat$L2d4d, na.rm = T) / sd(dat$L2d4d, na.rm = T)
dat$R2d4d_std = dat$R2d4d - mean(dat$R2d4d, na.rm = T) / sd(dat$R2d4d, na.rm = T)

# Create composites: irritation, challenge ----

# Irritation
# Get complete cases for EFA
set.efa <- dat %>% 
  select(Subject, irritated:annoyed) %>% 
  filter(complete.cases(.))
# Perform PCA
set.efa %>% 
  select(-Subject) %>% 
  fa.parallel() # 2 factors / 1 component
efa <- select(set.efa, -Subject) %>% 
  fa(nfactors = 2) #factor 1 is negative affect, factor 2 is positive affect
# append factor scores to set.efa and rename to PA and NA
set.efa <- cbind(set.efa, efa$scores) %>% 
  rename("feedback.NA" = MR1, "feedback.PA" = MR2)
 
# Append those scores to full dataset
dat = left_join(dat, set.efa)

# Challenge
# Get complete cases for EFA
set.efa2 = dat %>% 
  select(Subject, 
         easy.nav, challenging, stressful, diff.nav, reflexes, difficult, 
         good.fight, hard.control, mental.effort, comfort.control, exhausting) %>% 
  filter(complete.cases(.)) 
# Perform parallel analysis
set.efa2 %>% 
  select(easy.nav:exhausting) %>% 
  fa.parallel(fm = 'pa') # because complained about minres default # 5 factors??
# TODO: Consider these loadings. Component 1 doesn't seem to catch it all. Factor rotation more appropriate?
efa2 <- select(set.efa2, -Subject) %>% 
  fa(nfactor = 5) 
# stressful & exhausting, navigation&controls, difficulty/fighting, 
# reflexes/mental effort, and crud
set.efa2 <- cbind(set.efa2, efa2$scores) %>% 
  rename("stress" = MR3, "navigation" = MR2, "fighting" = MR4, 
         "effort" = MR4, "crud" = MR1)
dat <- left_join(dat, set.efa2)

# Make grand means and sds ----
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
ci.smd(smd = (vioMeans[2] - vioMeans[1]) / pool.sd(vioSDs, vioN), 
       n.1 = vioN[1], n.2 = vioN[2])

# Difficulty manipulation?
# manip check
# manipCheckDifficulty = lm(composite_challenge ~ Violence*Difficulty, data=dat) %>% summary()
# manipCheckDifficulty
# # ncp is "generally the observed t-statistic from comparing the two groups
# # see ?ci.smd
# ci.smd(ncp = manipCheckDifficulty$coefficients["Difficulty1", 3],
#        n.1 = difN[1], n.2 = difN[2])

# Irritation and DV
check2 <- lm(DV ~ feedback.NA, data = dat)
summary(check2)

check2.1 = lm(DV ~ feedback.PA + feedback.NA, data=dat)
summary(check2.1)
# compute.es::tes(tstat = 3.226, 
#     N     = summary(check2)$df[2]+3)

# Irritation not fostered by game violence
lm(feedback.NA ~  Violence * Difficulty, data = dat) %>%
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
m1 = lm(DV ~ Difficulty * Violence * L2d4d_std, data = dat,
        contrasts = list(Difficulty = "contr.sum",
                         Violence = "contr.sum"))
summary(m1)
# Full model, right hand
m2 = lm(DV ~ Difficulty * Violence * R2d4d_std, data = dat,
        contrasts = list(Difficulty = "contr.sum",
                         Violence = "contr.sum"))
summary(m2)
# 2x2 ANOVA model
m3 = lm(DV ~ Difficulty * Violence, data = dat,
        contrasts = list(Difficulty = "contr.sum",
                         Violence = "contr.sum"))
summary(m3)
#compute.es::tes(.963, 291)
#compute.es::tes(1.13, 291)
# I wonder: how do I generate a p-value against H0: r = .21?
# (z.obs - z.H0) / se(z) ~ z
(atanh(.05655639) - atanh(.21)) / (1/sqrt(294-3))
pnorm(-2.67, lower.tail=T)
# elaboration via simple slopes:
m3.1 = lm(DV ~ Violence, data=dat[dat$Difficulty == "Easy",])
m3.2 = lm(DV ~ Violence, data=dat[dat$Difficulty == "Hard",])
summary(m3.1)
# compute.es::tes(1.788, 145)
summary(m3.2)
# compute.es::tes(.422, 148)

# Adding composite irritation component as covariate
m3.5 = lm(DV ~ Difficulty * Violence + feedback.NA, data=dat,
          contrasts = list(Difficulty = "contr.sum",
                           Violence = "contr.sum"))
summary(m3.5) # soaks up a lot of variance but effect of violence still not significant
# compute.es::tes(.735, 269)

# 2d4d alone:
m4 = lm(DV ~ L2d4d, data = dat)
summary(m4)
# compute.es::tes(-.729, 272)
m5 = lm(DV ~ R2d4d, data = dat)
summary(m5)
# compute.es::tes(-.35, 273)

# Gameplay variables ----
dat %>% 
  select(Game.1:Game.6, DV) %>% 
  cor(use = 'pairwise')
select(dat, Game.1:Game.6) %>% 
  fa.parallel()
select(dat, Game.1:Game.6) %>% 
  fa(nfactors = 3)
dat %>% 
  select(Game.1:Game.6, DV) %>% 
  filter(complete.cases(.)) %>% 
  regressionBF(DV ~ Game.1 + Game.2 + Game.3 + Game.4 + Game.5 + Game.6, data = .) %>% 
  sort()

# Bayesian models of primary outcome ----
# ANOVA
bf_2x2 = anovaBF(DV ~ Violence*Difficulty, data=dat, rscaleFixed=.4, iterations=10^5)
bf_2x2
1/bf_2x2
bfList = 1/exp(bf_2x2@bayesFactor$bf)
# 2d4d
bf3.4 = filter(dat, !is.na(L2d4d)) %>% 
  lmBF(DV ~ L2d4d, data=., rscaleFixed=.4, iterations=10^5)
bf4.4 = filter(dat, !is.na(R2d4d)) %>% 
  lmBF(DV ~ R2d4d, data=., rscaleFixed=.4, iterations=10^5)

# test for support of interactions. l2d4d
set = dat %>% filter(!is.na(L2d4d))
additive <- lmBF(DV ~ Violence + Difficulty + L2d4d_std, 
                 data = set, rscaleFixed = .4)
int_vio_l2d4d <- lmBF(DV ~ Violence + Difficulty + L2d4d_std + Violence*L2d4d_std, 
     data = set, rscaleFixed = .4)
int_dif_l2d4d <- lmBF(DV ~ Violence + Difficulty + L2d4d_std + Difficulty*L2d4d_std, 
                      data = set, rscaleFixed = .4)
all_2s <- lmBF(DV ~ Violence*Difficulty + Violence*L2d4d_std + Difficulty*L2d4d_std, 
               data = set, rscaleFixed = .4)
full <- lmBF(DV ~ Violence*Difficulty*L2d4d_std, data = set, rscaleFixed = .4)
bf01_vio_l2d4d <- additive/int_vio_l2d4d # evidence against viox2d4d, 3.76
bf01_diff_l2d4d <- additive/int_dif_l2d4d # evidence against difx2d4d, 4.50
bf01_3way_l2d4d <- all_2s/full # evidence against 3-way, 3.40

# linear models considering R2d4d
set = dat %>% filter(!is.na(dat$R2d4d))
additive <- lmBF(DV ~ Violence + Difficulty + R2d4d_std, 
                 data = set, rscaleFixed = .4)
int_vio_R2d4d <- lmBF(DV ~ Violence + Difficulty + R2d4d_std + Violence*R2d4d_std, 
                      data = set, rscaleFixed = .4)
int_dif_R2d4d <- lmBF(DV ~ Violence + Difficulty + R2d4d_std + Difficulty*R2d4d_std, 
                      data = set, rscaleFixed = .4)
all_2s <- lmBF(DV ~ Violence*Difficulty + Violence*R2d4d_std + Difficulty*R2d4d_std, 
               data = set, rscaleFixed = .4)
full <- lmBF(DV ~ Violence*Difficulty*R2d4d_std, data = set, rscaleFixed = .4)
bf01_vio_r2d4d <- additive/int_vio_R2d4d # evidence against viox2d4d, 4.91
bf01_diff_r2d4d <- additive/int_dif_R2d4d # evidence against difx2d4d, 4.38
bf01_3way_r2d4d <- all_2s/full # evidence against 3-way, 3.21

# linear models considering composite irritation covariate
set2 = dat %>% 
  select(DV, Violence, Difficulty, feedback.NA, L2d4d_std) %>% 
  filter(complete.cases(.))
bf7 = lmBF(DV ~ Violence*Difficulty + feedback.NA, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.1 = lmBF(DV ~ Violence + Difficulty + feedback.NA, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.2 = lmBF(DV ~ Violence + feedback.NA, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.3 = lmBF(DV ~ Difficulty + feedback.NA, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.4 = lmBF(DV ~ feedback.NA, data=set2, rscaleFixed=.4, iterations=10^5)
bf7.5 = lmBF(DV ~ feedback.NA + L2d4d_std, data=set2, rscaleFixed=.4, iterations=10^5)

# right-hand
set3 = dat %>% 
  select(DV, Violence, Difficulty, feedback.NA, R2d4d_std) %>% 
  filter(complete.cases(.))
bf7.7 = lmBF(DV ~ feedback.NA, data=set3, rscaleFixed=.4, iterations=10^5)
bf7.6 = lmBF(DV ~ feedback.NA + R2d4d_std, data=set3, rscaleFixed=.4, iterations=10^5)

# Results
c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)
plot(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4))
# Results relative to covariate-only model
c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4
1/(c(bf7, bf7.1, bf7.2, bf7.3, bf7.4)/bf7.4)
plot(c(bf7, bf7.1, bf7.2, bf7.3)/bf7.4)

bf01_vio_cov <- bf7.4/bf7.2
bf01_diff_cov <- bf7.4/bf7.3
bf01_l2d4d_cov <- bf7.4/bf7.5
bf01_2way_cov <- bf7.1/bf7
bf01_r2d4d_cov <- bf7.7/bf7.6

# Censored-from-above analysis ----
library(censReg)
censModel1 = censReg(DV ~ Difficulty*Violence*L2d4d, left=1, right=9, data=dat)
summary(censModel1)
# adding covariate
censModel1.1 = censReg(DV ~ Difficulty*Violence*L2d4d + feedback.NA,
                       left=1, right=9, data=dat)
summary(censModel1.1)
# 3-way w/ right hand
censModel2 = censReg(DV ~ Difficulty*Violence*R2d4d, left=1, right=9, data=dat)
summary(censModel2)
# adding covariate
censModel2.1 = censReg(DV ~ Difficulty*Violence*R2d4d + feedback.NA,
                       left=1, right=9, data=dat)
summary(censModel2.1) # some p = .04 interactions
# 2x2 ANOVA, dropping 2d4d & 3-ways
censModel3 = censReg(DV ~ Difficulty*Violence, left=1, right=9, data=dat)
summary(censModel3)
# adding covariate
censModel3.1 = censReg(DV ~ Difficulty*Violence + feedback.NA,
                       left=1, right=9, data=dat)
summary(censModel3.1)

# Logistic regression w/ binned DV ----
dat$DVbin = ifelse(dat$DV == 9, 1, 0)
# With L2d4d
model1 = glm(DVbin ~ Difficulty*Violence*L2d4d_std, family=binomial(link="logit"), data=dat)
summary(model1)
# With L2d4d + covariate
model1.1 = glm(DVbin ~ Difficulty*Violence*L2d4d_std + feedback.NA, 
               family=binomial(link="logit"), data=dat)
summary(model1.1)
# With R2d4d
model2 = glm(DVbin ~ Difficulty*Violence*R2d4d_std, family=binomial(link="logit"), data=dat)
summary(model2)
# With R2d4d + covariate
model2.1 = glm(DVbin ~ Difficulty*Violence*R2d4d_std + feedback.NA,
               family=binomial(link="logit"), data=dat)
summary(model2.1)
# 2x2 ANOVA
model3 = glm(DVbin ~ Difficulty*Violence, family=binomial(link = "logit"), data=dat)
summary(model3)
# 2x2 ANCOVA (irritation covariate)
model3.5 = glm(DVbin ~ Difficulty*Violence + feedback.NA,
               family=binomial(link = "logit"), data=dat)
summary(model3.5)

save.image()

# Kruskal-wallace tests aren't significant ----
kruskal.test(DV ~ Violence, data = dat)
kruskal.test(DV ~ Difficulty, data = dat)
kruskal.test(DV ~ interaction(Violence, Difficulty), data = dat)
