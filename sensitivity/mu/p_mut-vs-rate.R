args <- commandArgs(trailingOnly=T)

# pn <- as.numeric(args[3])
# ins <- as.numeric(args[4])

setwd("~/PhD/manuscripts/01_rates/00_revision/scripts/07_mutation/scripts/")

library(reshape2)
library(ggplot2)
library(ggthemes)
library(grid)

ins <- 6
pn <- 4
source(paste("instructions/i_", ins, ".R", sep=""))
source(paste("pn_", pn, "/s_d-init-mutation.R", sep=""))

for (popno in 1:2){
  if(popno == 1){
    seed <- seed.msm
    pop <- "msm"
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmut.data", sep=""))
    allmut.msm <- allmut
  }else{
    seed <- seed.het
    pop <- "het" 
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmut.data", sep=""))
    allmut.het <- allmut
  }
}

tempall <- as.data.frame(rbind(cbind(melt(allmut.msm[,,3,1]), pop="MSM"),
                               cbind(melt(allmut.het[,,3,1]), pop="HMW")))
all <- tempall[complete.cases(tempall),]
colnames(all) <- c("Set", "Mutation", "Rate", "Group")

for (i in 1:length(vec_mu_ta)){
  all$Mutation[which(all$Mutation==i)] <- vec_mu_ta[i]
}
all$Mutation <- as.factor(all$Mutation)


col.msm <- "darkolivegreen3"
col.het <- "dodgerblue3"
bg.col <- "white"

tiff("../figures/S4Fig.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
# pdf("../figures/S4Fig.pdf", width=10/1.93, height=7/1.93)
bp <-   ggplot(all, aes(x=Group, y=Rate))+
  geom_boxplot(outlier.colour="goldenrod1", outlier.size=0.7, aes(colour=Group, fill=Group), size=0.5, alpha=0.5)+
  theme_hc()+
  theme(text= element_text(size=8),
        panel.margin = (unit(0.1, "cm")),
        strip.background=element_rect(colour="white", fill="white"),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(vjust=-29, colour="#7F7F7FFF"), 
        legend.position="right"
  ) +
  scale_colour_manual(values=c(col.msm, col.het)) + 
  scale_fill_manual(values=c(col.msm, col.het)) + 
  labs(x="probability of resistance during treatment", y="rate of spread (per year)") +
  scale_colour_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW"))+ # set colours and label names in legend
  scale_fill_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW"))+ # set fill (colours) and label names in legend
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7), limits=c(0,7))
  
  #   scale_x_discrete(breaks=c("       ", "men who have\nsex with men", "     ", "heterosexuals", "      ")) +
#   stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })

bp + facet_wrap( ~ Mutation, nrow=1)
dev.off()  
