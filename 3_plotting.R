library(readxl)
library(rms)
library(ordinal)
library(tidyverse)
library(lubridate)
library(MBESS)
library(censReg)
library(BayesFactor)
library(psych)
# install.packages('devtools'); library(devtools); install_github("Joe-Hilgard/hilgard")
library(hilgard)
library(lsmeans)
load(".RData")

bigtext <- theme(axis.title = element_text(size=14),
                 plot.title = element_text(size=16))

# make means, SEs, CIs per cell
datSum <- dat %>% 
  group_by(Violence, Difficulty) %>% 
  summarise(mean = mean(DV, na.rm = T), 
            sd = sd(DV, na.rm = T),
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         ll = mean - 1.96*se,
         ul = mean + 1.96*se)

# or get it from the model with lsmeans
datLS <- lsmeans(m1, specs = c("Violence", "Difficulty"))

# Figure 1: histograms w/ facet wraps for conditions
ggplot(dat, aes(x=DV)) + 
  geom_bar() +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14))
ggsave("Figure1.png", width = 5.5, height = 4, units="in")

# Figure 2: feedback negative affect influences aggression
ggplot(dat, aes(x=feedback.NA, y = DV)) +
  geom_point(cex=2, alpha=.5, position=position_jitter(height=.1)) +
  geom_smooth() +
  scale_x_continuous("Experienced provocation")+
  scale_y_continuous("Coldpressor duration") +
  #ggtitle("Dependent Variable is Sensitive") +
  bigtext
ggsave("Figure2.png", width = 5.5, height = 4, units="in")

ggplot(data = dat, aes(x = feedback.NA, y = DV)) +
  geom_jitter(height = .1, width = .03, alpha = .5) +
  geom_smooth() +
  xlab("Experienced provocation") +
  ylab("Coldpressor duration")
ggsave("Figure2_alt.png", width = 4, height = 3, units = 'in')

# R2 was asking about QQ plots. i'm not sure how much this reveals though
qqplot(x = dat$DV[dat$Violence == "Less Violent"], y = dat$DV[dat$Violence == "Violent"],
       xlab = "Chex Quest", ylab = "Brutal Doom",
       main = "QQ-plot")

# Figure 3: violence and difficulty on DV, jittered points with mean & CI
ggplot(datSum, aes(x = Violence)) +
  geom_jitter(aes(y = DV), width = .15, height = .175, alpha = .30, dat) +
  geom_pointrange(aes(y = mean, ymin = ll, ymax = ul), size = 1.25, alpha = 1) +
  facet_wrap(~Difficulty) +
  scale_y_continuous("Aggression") +
  theme_bw()
ggsave("Figure3.png", width = 5.5, height = 4, units="in")

ggplot(tidy(datLS), aes(x = Violence)) +
  geom_jitter(aes(y = DV), width = .15, height = .175, alpha = .30, dat) +
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high), size = 1.25, alpha = 1) +
  facet_wrap(~Difficulty) +
  scale_y_continuous("Aggression") +
  theme_bw() # numbers from lsmeans are very similar

# Figure 4: no effect of 2d4d ratio
# faceted scatterplot w/ right-hand 2d4d:
ggplot(data=dat, aes(x=R2d4d, y=DV)) +
  geom_jitter(width = 0, height = .1, cex=1, alpha=.5) +
  geom_smooth(method="lm")+
  #labs(title="Null effects of 2d4d ratio (right hand)") +
  xlab("Right-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  scale_x_continuous(limits = c(.85, 1.07))
ggsave("Figure4a.png", width=6, height=3.7, units="in")

# faceted scatterplot w/ left-hand 2d4d:
ggplot(data=dat, aes(x=L2d4d, y=DV)) +
  geom_jitter(width = 0, height = .1, cex=1, alpha=.5) +
  geom_smooth(method="lm")+
  #labs(title="Null effects of 2d4d ratio (left hand)") +
  xlab("Left-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  scale_x_continuous(limits = c(.85, 1.07))#+
ggsave("Figure4b.png", width=6, height=3.7, units="in")

# Manipulation checks ----
# Violent content
dat %>% 
  filter(dat$violence != -999, !is.na(dat$Condition)) %>% 
  ggplot(aes(x=violence)) +
  geom_bar(stat="count") +
  facet_wrap(~Violence) +
  scale_x_discrete("Rated violent content", limits=1:7) +
  scale_y_continuous("Count") +
  ggtitle("Violent Content Manipulation Check") + 
  bigtext
ggsave("violence-condition_hist.png", width = 5.5, height = 4, units="in")

# Difficulty affects composite challenge
ggplot(dat, aes(x=challenge)) +
  geom_histogram() +
  facet_wrap(~Difficulty) +
  scale_x_continuous("Ratings of Difficulty") +
  scale_y_continuous("Count") +
  ggtitle("Difficulty Manipulation Check") +
  bigtext
ggsave("Difficulty-EFA_hist.png", width = 5.5, height = 4, units="in")

# Did players feel their in-game behavior was aggressive?
ggplot(dat, aes(x = Violence, y = aggressed)) +
  geom_boxplot(notch = T) +
  #geom_jitter(height = .2, width = .3) +
  facet_grid(~Difficulty)


