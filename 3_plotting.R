library(ggplot2)

bigtext =   theme(axis.title = element_text(size=14),
                  plot.title = element_text(size=16))

# histograms w/ facet wraps for conditions
ggplot(dat, aes(x=DV)) + 
  geom_bar() +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14))
ggsave("DV-condition_hist.png", width = 5.5, height = 4, units="in")

ggplot(dat, aes(x=DV, fill=Violence)) + 
  geom_bar() +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_fill_hue(h.start=180-15,direction=-1) +
  theme(legend.position="none")

# Boxplot with overplotted mean
# TODO: Possible to add error bars to the mean?
ggplot(dat, aes(x=Violence, y=DV)) + 
  facet_grid(~Difficulty) +
  #geom_violin(aes(fill = Violence)) +
  geom_boxplot(width=.5, notch=T) +
  stat_summary(fun.y="mean", geom="point", shape=20, size=10, col="grey50") +
  #stat_summary(fun.y="median", geom="point", shape=20, size=6, col="black") +
  scale_x_discrete("Condition") +
  scale_y_continuous("Aggression")

# TODO: R2 was asking about QQ plots. i'm not sure how much this reveals.
qqplot(x = dat$DV[dat$Violence == "Less Violent"], y = dat$DV[dat$Violence == "Violent"])


datSum <- dat %>% 
  group_by(Violence, Difficulty) %>% 
  summarise(mean = mean(DV, na.rm = T), 
            sd = sd(DV, na.rm = T),
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         ll = mean - 1.96*se,
         ul = mean + 1.96*se)

ggplot(datSum, aes(x = Violence)) +
  geom_boxplot(aes(y = DV), dat) +
  geom_pointrange(aes(y = mean, ymin = ll, ymax = ul)) +
  facet_wrap(~Difficulty)

ggplot(datSum, aes(x = Violence)) +
  geom_jitter(aes(y = DV), width = .2, height = .1, alpha = .40, dat, col = "#669999") +
  geom_pointrange(aes(y = mean, ymin = ll, ymax = ul), size = 1.25, col = "#ff0033", alpha = 1) +
  facet_wrap(~Difficulty) +
  scale_y_continuous("Aggression")

# # Adding mean and CI to histogram is hideous
# ggplot(dat, aes(x=DV)) + 
#   geom_bar() +
#   facet_wrap(~Violence+Difficulty, nrow=2) +
#   scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
#   theme(strip.text.x = element_text(size = 12),
#         axis.title.x = element_text(size = 14)) +
#   geom_vline(aes(xintercept = mean), data = datSum) +
#   geom_vline(aes(xintercept = ll), data = datSum) +
#   geom_vline(aes(xintercept = ul), data = datSum)

# Manipulation check ----
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
#ggsave("violence-condition_hist.png", width = 5.5, height = 4, units="in")

# Difficulty affect composite challenge? (Is this reverse-scored?)
ggplot(dat, aes(x=stress)) +
  geom_histogram() +
  facet_wrap(~Difficulty) +
  scale_x_continuous("Ratings of Difficulty") +
  scale_y_continuous("Count") +
  ggtitle("Difficulty Manipulation Check") +
  bigtext
#ggsave("Difficulty-PCA_hist.png", width = 5.5, height = 4, units="in")

# feedback negative affect influence aggression?
ggplot(dat, aes(x=feedback.NA, y = DV)) +
  geom_point(cex=2, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth() +
  scale_x_continuous("Composite irritation (1st principal component)")+
  scale_y_continuous("Coldpressor duration") +
  ggtitle("Dependent Variable is Sensitive") +
  bigtext
#ggsave("DV-PCA_scatter.png", width = 5.5, height = 4, units="in")

# Here be old plots. Lots to clean up:
# TODO: Clean up all this junk
qplot(data=dat, x=L2d4d, y=DV, col=Violence, geom=c("point", "smooth"))
qplot(data=dat, x=R2d4d, y=DV, col=Violence, geom=c("point", "smooth"))

# the best of the best:
postertext = theme(text = element_text(size=16),
                   axis.title = element_text(size=24),
                   strip.text = element_text(size=28),
                   plot.title = element_text(size=32)
)


# scatterplot w/ left-hand 2d4d:
ggplot(data=dat, aes(x=L2d4d, y=DV, col=Violence)) +
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
ggplot(data=dat, aes(x=R2d4d, y=DV, col=Violence)) +
  geom_point(cex=1, alpha=.8, position=position_jitter(height=.1)) +
  geom_smooth(method="lm")+
  labs(title="Does prenatal testosterone interact with game violence?") +
  xlab("Right-hand 2d4d ratio \n Smaller ratio implies greater testosterone") +
  ylab("Coldpressor duration") +
  scale_y_discrete(limits=1:9) +
  # make the text huge
  bigtext #+
# remove the background
# theme(panel.background=element_blank(),
#       # this part attempts to adjust the axis distance but i'm having trouble
#       axis.title.y=element_text(vjust=.2),
#       axis.title.x=element_text(vjust=.5)
# )

# faceted scatterplot w/ right-hand 2d4d:
ggplot(data=dat, aes(x=R2d4d, y=DV)) +
  geom_jitter(width = 0, height = .05,cex=1, alpha=.75) +
  geom_smooth(method="lm")+
  #labs(title="Null effects of 2d4d ratio (right hand)") +
  xlab("Right-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  scale_x_continuous(limits = c(.85, 1.07))
ggsave("r2d4d_x_2x2.png", width=6, height=3.7, units="in")

# faceted scatterplot w/ left-hand 2d4d:
ggplot(data=dat, aes(x=L2d4d, y=DV)) +
  geom_jitter(width = 0, height = .05, cex=1, alpha=.5) +
  geom_smooth(method="lm")+
  #labs(title="Null effects of 2d4d ratio (left hand)") +
  xlab("Left-hand 2d4d ratio") +
  ylab("Coldpressor duration") +
  # break it out into each game condition
  facet_wrap(~Violence*Difficulty) +
  scale_y_discrete(limits=1:9, breaks=c(1,3,5,7,9)) +
  scale_x_continuous(limits = c(.85, 1.07))#+
ggsave("l2d4d_x_2x2.png", width=6, height=3.7, units="in")

ggplot(data = dat, aes(x = feedback.NA, y = DV)) +
  geom_jitter(height = .03, width = .03, alpha = .5) +
  geom_smooth() +
  xlab("Experienced provocation") +
  ylab("Coldpressor duration")
ggsave("Provocation.png", width = 4, height = 3, units = 'in')

# # jitter points
# ggplot(data=dat, aes(x=interaction(dat$Violence, dat$Difficulty), y=DV)) +
#   geom_point(position=position_jitter(width=.25, height=.05), cex=3, alpha=.8) +
#   theme(text = element_text(size=32)) +
#   xlab("Game Condition") +
#   ylab("Coldpressor duration assigned")
# boxplot