library(ggplot2)

bigtext =   theme(axis.title = element_text(size=14),
                  plot.title = element_text(size=16))

# histograms w/ facet wraps for conditions
ggplot(dat, aes(x=DV)) + 
  geom_histogram(breaks=c(1:9)) +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14))
ggsave("DV-condition_hist.png", width = 5.5, height = 4, units="in")

ggplot(dat, aes(x=DV, fill=Violence)) + 
  geom_histogram(breaks=c(1:9)) +
  scale_x_discrete("Coldpressor Assignment", limits = c(1:9)) +
  facet_wrap(~Violence+Difficulty, nrow=2) +
  scale_fill_hue(h.start=180-15,direction=-1) +
  theme(legend.position="none")

ggplot(dat, aes(x=interaction(Difficulty, Violence), y=DV)) + 
  geom_violin(aes(fill = Violence)) +
  #geom_boxplot(width=.15, notch=T) +
  stat_summary(fun.y="mean", geom="point", shape=20, size=10, col="black") +
  stat_summary(fun.y="median", geom="point", shape=20, size=6, col="white") +
  scale_x_discrete("Condition") +
  theme(legend.position="none",
        axis.text.x = element_text(color="black", size=12)
  )

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

# Difficulty affect composite challenge?
ggplot(dat, aes(x=diffFactor1)) +
  geom_histogram() +
  facet_wrap(~Difficulty) +
  scale_x_continuous("Ratings of Difficulty") +
  scale_y_continuous("Count") +
  ggtitle("Difficulty Manipulation Check") +
  bigtext
#ggsave("Difficulty-PCA_hist.png", width = 5.5, height = 4, units="in")

# Composite irritation influence aggression?
ggplot(dat, aes(x=factor1, y = DV)) +
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
theme(panel.background=element_blank(),
      # this part attempts to adjust the axis distance but i'm having trouble
      axis.title.y=element_text(vjust=.2),
      axis.title.x=element_text(vjust=.5)
)

# faceted scatterplot w/ right-hand 2d4d:
ggplot(data=dat, aes(x=R2d4d, y=DV)) +
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
ggplot(data=dat, aes(x=L2d4d, y=DV)) +
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
# ggplot(data=dat, aes(x=interaction(dat$Violence, dat$Difficulty), y=DV)) +
#   geom_point(position=position_jitter(width=.25, height=.05), cex=3, alpha=.8) +
#   theme(text = element_text(size=32)) +
#   xlab("Game Condition") +
#   ylab("Coldpressor duration assigned")
# boxplot