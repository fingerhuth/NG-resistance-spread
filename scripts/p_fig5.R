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
library(grid)

# load data sets
## outros: parameter sets that are within the calibration rates
load(paste("../data/outros_", pop1, "-", seed1, "-", cond1, ".data", sep=""))
outros1 <- outros
load(paste("../data/outros_", pop2, "-", seed2, "-", cond2, ".data", sep=""))
outros2 <- outros

# calculate treatment rates
## treatment rate t=f/D (fraction of diagnosed and treated divided by average duration of infection)
## f is in column 11, D is in column 10 of outros
tr1 <- data.frame(t=outros1[,11]/outros1[,10]) # tr1: treatment rates for hetersexual men and women
tr2 <- data.frame(t=outros2[,11]/outros2[,10]) # tr2: treatment rates for men who have sex with men

# calculate means and medians (not used further in this script)
mean.het <- mean(tr1[,1])
mean.msm <- mean(tr2[,1])
median.het <- median(tr1[,1])
median.msm <- median(tr2[,1])

# generate plot
p <- ggplot() +
  theme_hc()+ # set theme
  theme(text=element_text(size = 8),
        legend.position = "right",
        plot.margin = unit(c(2,0,0,0), "pt")) + # set text size
  geom_histogram(data=tr1, aes(x=t, y=..density.., col="Het", fill="Het"), size=0.5, alpha=0.5, binwidth=0.2) + # plot histogram of heterosexual men and women treatment rates
  geom_histogram(data=tr2, aes(x=t, y=..density.., col="MSM", fill="MSM"), size=0.5, alpha=0.5, binwidth=0.2) + # plot histogram of men who have sex with men treatmen rates
  scale_colour_manual(guide_legend(title=""), values=c("dodgerblue3","darkolivegreen3"), labels=c("HMW", "MSM"))+ # set colours and label names in legend
  scale_fill_manual(guide_legend(title=""), values=c("dodgerblue3","darkolivegreen3"), labels=c("HMW", "MSM"))+ # set fill (colours) and label names in legend
  labs(x="treatment rate (per year)") # rename x axis

# generate .eps file
# cairo_ps("../figures/Fig5.eps", width=7, height=7)
# p
# dev.off()

tiff("../figures/Fig5.tiff", width=5.2, height=2, unit="in", res=450, compression="lzw", family="Arial")
p
dev.off()
