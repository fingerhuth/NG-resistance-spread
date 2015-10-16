# set seeds, population and condition names
pop1 <- "het"
seed1 <- 993734
cond1 <- "het"
pop2 <- "msm"
seed2 <- 312774
cond2 <- "msm"

# load libraries
library(ggplot2)
library(ggthemes)

# load data sets
## propresT: proportion of infections that are resistant, for all simulations within set conditions
## times: time points (in years) at which those proportions reported
load(paste("../data/propresT_", pop1, "_", seed1, ".data", sep=""))
pT.het <- propresT
load(paste("../data/times_", pop1, "_", seed1, ".data", sep=""))
t.het <- timesRes
load(paste("../data/propresT_", pop2, "_", seed2, ".data", sep=""))
pT.msm <- propresT
load(paste("../data/times_", pop2, "_", seed2, ".data", sep=""))
t.msm <- timesRes

# heterosexual men and women
# get median 
mT.het <- apply(pT.het, 1, median)
# get lower (low) and upper (up) bounds that include 95% or 50% of all simulation values
qts <- apply(pT.het, 1, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
low95.het <- qts[1,]
low50.het <- qts[2,]
up50.het <- qts[3,]
up95.het <- qts[4,]

# men who have sex with men
## for comments see 'heterosexual men and women'
mT.msm <- apply(propresT, 1, median)
qts <- apply(pT.msm, 1, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
low95.msm <- qts[1,]
low50.msm <- qts[2,]
up50.msm <- qts[3,]
up95.msm <- qts[4,]

# set number of years that should be plotted
het.max <- 30
msm.max <- 30

# combine timelines for heterosexual men and women in one data frame
df.het <- data.frame(times=t.het[which(t.het<het.max)], 
                     mT=mT.het[which(t.het<het.max)], 
                     low95=low95.het[which(t.het<het.max)], 
                     low50=low50.het[which(t.het<het.max)], 
                     up50=up50.het[which(t.het<het.max)], 
                     up95=up95.het[which(t.het<het.max)], 
                     Group="HMW")
# combine timelines for men who have sex with men in one data frame
df.msm <- data.frame(times=t.msm[which(t.msm<msm.max)], 
                     mT=mT.msm[which(t.msm<msm.max)], 
                     low95=low95.msm[which(t.msm<msm.max)], 
                     low50=low50.msm[which(t.msm<msm.max)], 
                     up50=up50.msm[which(t.msm<msm.max)], 
                     up95=up95.msm[which(t.msm<msm.max)], 
                     Group="MSM")
# merge data frames
df <- rbind(df.msm, df.het)

# plot timelines
p <- ggplot(df, aes(x=times)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=mT*100, colour=Group), alpha=1) + # plot median
  geom_ribbon(aes(ymin=low50*100, ymax=up50*100, fill=Group), alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95*100, ymax=up95*100,fill=Group), alpha=0.2) + # plot 95% area
  geom_line(aes(x=times, y=0.05*100), linetype="dotted", size=0.5) + # plot 5% threshold
  scale_colour_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
  scale_fill_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
  labs(x="years", y="proportion infections\ncaused by resistant strain (in %)") # rename axis labels

# # output as .eps file
# cairo_ps("../figures/Fig4.eps", width=11, height=7)
# p
# dev.off()

tiff("../figures/Fig4.tiff", width=5.2, height=2.5, unit="in", res=400, compression="lzw", family="Arial")
p
dev.off()
