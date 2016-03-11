args <- commandArgs(trailingOnly=T)

setwd("~/PhD/manuscripts/01_rates/00_revision/scripts/02_costs/scripts/")
#### !!! LOOP DOESN'T WORK. Generated plots manually.


# pn <- as.numeric(args[3])
# ins <- as.numeric(args[4])

library(reshape2)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)


ins <- 2
source(paste("instructions/i_", ins, ".R", sep=""))


pn <- 7
popno <- 1
seed <- seed.msm
pop <- "msm"
col <- "darkolivegreen3"
load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]

# popno <- 2
# seed <- seed.het
# pop <- "het"
# col <- "darkolivegreen3"
# load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
# sets <- outros[1:k.max, 7:11]
# }

source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

if (pn==7){
  vcostsL <- melt(allcost[,1,,1,1])
  vcostsH <- melt(allcost[,1,,2,1])
  vcostsT <- melt(allcost[,1,,3,1])
  xlabel <- "fitness costs in duration of infection"
  vec_cost <- vec_cost_v
}else if (pn==8){
  vcostsL <- melt(allcost[,,1,1,1])
  vcostsH <- melt(allcost[,,1,2,1])
  vcostsT <- melt(allcost[,,1,3,1])
  xlabel <- "fitness costs in transmission probability per partnership"  
  vec_cost <- vec_cost_b
}else{
  print(paste("Danger, danger!"))
}
vcostsdiff <- as.data.frame(cbind(Set=vcostsL[,1], Costs=vcostsL[,2], Difference=vcostsL[,3]-vcostsH[,3]))
names(vcostsT) <- c("Set", "Costs", "Rate")

# # vcostsH and vcostsL equivalent?
#   vcostsdiff2 <- cbind(vcostsH[,1], vcostsH[,2], vcostsL[,3]-vcostsH[,3])
#   which(vcostsdiff[,1:2]!=vcostsdiff2[,1:2])


ssT <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)
isna<- numeric()
for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  isna[i] <- sum(is.na(subsetT))
  subsetT[is.na(subsetT)] <- 0
  
  
  ssT[i,] <- quantile(subsetT, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
}

percentSpreading.msm.7 <- (2000-isna)/20

ssTdf <- as.data.frame(cbind(ssT, vec_cost))
names(ssTdf) <- c("low95", "low50", "median", "up50", "up95")

    pT.msm.7 <-      ggplot(ssTdf, aes(x=vec_cost)) +
    theme_hc() + # set plot theme
    theme(text = element_text(size=8),
          plot.margin = unit(c(0,0,0,0), "pt"),
          legend.position = "right")+
    geom_line(aes(y=median), alpha=1, colour=col) + # plot median
    geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
    geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
    #     scale_colour_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
    #     scale_fill_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
    labs(x=xlabel, 
         y="rate of spread")+
  xlim(0,1)


pn <- 7
# popno <- 1
# seed <- seed.msm
# pop <- "msm"
# col <- "dodgerblue3"
# load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
# load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
# sets <- outros[1:k.max, 7:11]

popno <- 2
seed <- seed.het
pop <- "het"
col <- "dodgerblue3"
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]
# }

source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

if (pn==7){
  vcostsL <- melt(allcost[,1,,1,1])
  vcostsH <- melt(allcost[,1,,2,1])
  vcostsT <- melt(allcost[,1,,3,1])
  xlabel <- "fitness costs in duration of infection"
  vec_cost <- vec_cost_v
}else if (pn==8){
  vcostsL <- melt(allcost[,,1,1,1])
  vcostsH <- melt(allcost[,,1,2,1])
  vcostsT <- melt(allcost[,,1,3,1])
  xlabel <- "fitness costs in transmission probability per partnership"  
  vec_cost <- vec_cost_b
}else{
  print(paste("Danger, danger!"))
}
vcostsdiff <- as.data.frame(cbind(Set=vcostsL[,1], Costs=vcostsL[,2], Difference=vcostsL[,3]-vcostsH[,3]))
names(vcostsT) <- c("Set", "Costs", "Rate")

# # vcostsH and vcostsL equivalent?
#   vcostsdiff2 <- cbind(vcostsH[,1], vcostsH[,2], vcostsL[,3]-vcostsH[,3])
#   which(vcostsdiff[,1:2]!=vcostsdiff2[,1:2])


ssT.het.7 <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)
isna <- numeric()
for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  isna[i] <- sum(is.na(subsetT))
  subsetT[is.na(subsetT)] <- 0
  
  
  ssT[i,] <- quantile(subsetT, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
}

percentSpreading.het.7 <- (2000-isna)/20

ssTdf <- as.data.frame(cbind(ssT, vec_cost))
names(ssTdf) <- c("low95", "low50", "median", "up50", "up95")

pT.het.7 <-      ggplot(ssTdf, aes(x=vec_cost)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=median), alpha=1, colour=col) + # plot median
  geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
  #     scale_colour_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
  #     scale_fill_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
  labs(x=xlabel, 
       y="rate of spread")+
  xlim(0,1)


pn <- 8
popno <- 1
seed <- seed.msm
pop <- "msm"
col <- "darkolivegreen3"
load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]

# popno <- 2
# seed <- seed.het
# pop <- "het"
# col <- "darkolivegreen3"
# load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
# sets <- outros[1:k.max, 7:11]
# }

source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

if (pn==7){
  vcostsL <- melt(allcost[,1,,1,1])
  vcostsH <- melt(allcost[,1,,2,1])
  vcostsT <- melt(allcost[,1,,3,1])
  xlabel <- "fitness costs in duration of infection"
  vec_cost <- vec_cost_v
}else if (pn==8){
  vcostsL <- melt(allcost[,,1,1,1])
  vcostsH <- melt(allcost[,,1,2,1])
  vcostsT <- melt(allcost[,,1,3,1])
  xlabel <- "fitness costs in transmission probability per partnership"  
  vec_cost <- vec_cost_b
}else{
  print(paste("Danger, danger!"))
}
vcostsdiff <- as.data.frame(cbind(Set=vcostsL[,1], Costs=vcostsL[,2], Difference=vcostsL[,3]-vcostsH[,3]))
names(vcostsT) <- c("Set", "Costs", "Rate")

# # vcostsH and vcostsL equivalent?
#   vcostsdiff2 <- cbind(vcostsH[,1], vcostsH[,2], vcostsL[,3]-vcostsH[,3])
#   which(vcostsdiff[,1:2]!=vcostsdiff2[,1:2])


ssT <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)
isna <- numeric()
for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  isna[i] <- sum(is.na(subsetT))
  subsetT[is.na(subsetT)] <- 0
  
  
  ssT[i,] <- quantile(subsetT, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
}

percentSpreading.msm.8 <- (2000-isna)/20

ssTdf <- as.data.frame(cbind(ssT, vec_cost))
names(ssTdf) <- c("low95", "low50", "median", "up50", "up95")

pT.msm.8 <-      ggplot(ssTdf, aes(x=vec_cost)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=median), alpha=1, colour=col) + # plot median
  geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
  #     scale_colour_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
  #     scale_fill_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
  labs(x=xlabel, 
       y="rate of spread")+
  xlim(0,1)


pn <- 8
# popno <- 1
# seed <- seed.msm
# pop <- "msm"
# col <- "dodgerblue3"
# load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
# load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
# sets <- outros[1:k.max, 7:11]

popno <- 2
seed <- seed.het
pop <- "het"
col <- "dodgerblue3"
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]
# }

source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

if (pn==7){
  vcostsL <- melt(allcost[,1,,1,1])
  vcostsH <- melt(allcost[,1,,2,1])
  vcostsT <- melt(allcost[,1,,3,1])
  xlabel <- "fitness costs in duration of infection"
  vec_cost <- vec_cost_v
}else if (pn==8){
  vcostsL <- melt(allcost[,,1,1,1])
  vcostsH <- melt(allcost[,,1,2,1])
  vcostsT <- melt(allcost[,,1,3,1])
  xlabel <- "fitness costs in transmission probability per partnership"  
  vec_cost <- vec_cost_b
}else{
  print(paste("Danger, danger!"))
}
vcostsdiff <- as.data.frame(cbind(Set=vcostsL[,1], Costs=vcostsL[,2], Difference=vcostsL[,3]-vcostsH[,3]))
names(vcostsT) <- c("Set", "Costs", "Rate")

# # vcostsH and vcostsL equivalent?
#   vcostsdiff2 <- cbind(vcostsH[,1], vcostsH[,2], vcostsL[,3]-vcostsH[,3])
#   which(vcostsdiff[,1:2]!=vcostsdiff2[,1:2])


ssT <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)
isna <- numeric()
for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  isna[i] <- sum(is.na(subsetT))
  subsetT[is.na(subsetT)] <- 0
  
  
  ssT[i,] <- quantile(subsetT, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
}

percentSpreading.het.8 <- (2000-isna)/20

ssTdf <- as.data.frame(cbind(ssT, vec_cost))
names(ssTdf) <- c("low95", "low50", "median", "up50", "up95")



pT.het.8 <-      ggplot(ssTdf, aes(x=vec_cost)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=median), alpha=1, colour=col) + # plot median
  geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
  #     scale_colour_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
  #     scale_fill_manual(values=c("darkolivegreen3","dodgerblue3"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
  labs(x=xlabel, 
       y="rate of spread")+
  xlim(0,1)

# empty <- data.frame(x=1:2, y=c(4,3), col=letters[1:2])
# pdummy <- ggplot(ssTdf, aes(x=vec_cost)) +
# #   theme_hc() + # set plot theme
#   theme(
#     panel.background = element_rect(fill="white", colour="white"),
#     panel.grid.major = element_line(colour="white"),
#     panel.grid.minor = element_line(colour="white"),
#     axis.text = element_text(colour="white"),
#     axis.ticks = element_line(colour="white"), 
#     axis.title = element_text(colour="white")
#   ) +
#   geom_line(aes(y=median), alpha=1, colour="white") + # plot median
# #   geom_ribbon(aes(ymin=low50, ymax=up50), fill="white") + # plot 50% area
# #   geom_ribbon(aes(ymin=low95, ymax=up95), fill="white") +  # plot 95% area
#   xlab("")+
#   ylab("")
# 
# pl <- pdummy + 
#   theme(
#     panel.background = element_rect(fill="white", colour="white"),
#     panel.grid.major = element_line(colour="white"),
#     panel.grid.minor = element_line(colour="white"),
#     axis.text = element_text(colour="white"),
#     axis.ticks = element_line(colour="white"), 
#     axis.title = element_text(colour="white")
#   ) +
#   theme(legend.position = c(.5, .4))+
#   geom_tile(data=empty, aes(x=x, y=y, fill=col), linetype="blank", alpha=0.6) +
#   scale_fill_manual(values=c("dodgerblue3","darkolivegreen3"),
#                     labels=c("HMW", "MSM"))+
#   guides(fill=guide_legend(title=NULL, 
#                            direction = "vertical", 
#                            label.position="right",
#                            keywidth = 1, 
#                            keyheight = 1, 
#                            label.hjust = 0.5,
#                            label.theme=element_text(size=8, angle=0)))

source("f_multipanel.R")
pdf("../figures/S1_appendixB.pdf", width=7, height=5.2)
multipanel(pT.msm.7, pT.msm.8, pT.het.7, pT.het.8, cols=2)
dev.off()

vec_cost_v <- vec_cost_b
PS <- as.data.frame(rbind(
      cbind(PerSp = as.numeric(as.character(percentSpreading.msm.7)), para="v", costs=vec_cost_v, pop="MSM", group=1),
      cbind(PerSp = as.numeric(as.character(percentSpreading.het.7)), para="v", costs=vec_cost_v, pop="HMW", group=2),
      cbind(PerSp = as.numeric(as.character(percentSpreading.msm.8)), para="b", costs=vec_cost_b, pop="MSM", group=3),
      cbind(PerSp = as.numeric(as.character(percentSpreading.het.8)), para="b", costs=vec_cost_b, pop="HMW", group=4)))



pps <- ggplot(PS, aes(x=as.numeric(as.character(costs))))+
  geom_line(aes(y=as.numeric(as.character(PerSp)), group=group, col=pop, linetype=para))+
  xlim(0,1)+
  ylim(0,100)+
  theme_hc()+
  theme(text=element_text(size=8), 
        plot.margin=unit(c(0,0,0,0), "pt"),
        legend.position="right")+
  scale_linetype_manual(values=c("dashed", "dotted"), labels=c("transmission probability\nper partnership", "duration of infection"), guide=guide_legend(title=""))+
  scale_colour_manual(values=c("dodgerblue3", "darkolivegreen3"), labels=c("HMW", "MSM"), guide=guide_legend(title="", reverse=T)) +
  xlab("fitness costs")+
  ylab("proportion of simulations with successful resistance spread (in %)")

pdf("../figures/S1_appendixA.pdf", width=7/1.5, height=5.2/1.4)
pps
dev.off()
