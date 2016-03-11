args <- commandArgs(trailingOnly=T)


#### !!! LOOP DOESN'T WORK. Generated plots manually.


# pn <- as.numeric(args[3])
# ins <- as.numeric(args[4])

library(reshape2)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)

ins <- 2
pn <- 7
source(paste("instructions/i_", ins, ".R", sep=""))

popno <- 1
seed <- seed.msm
pop <- "msm"
col <- "darkolivegreen3"
load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]

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


ssr <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)

for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subset <- vcostsdiff[which(vcostsdiff$Costs==levels(as.factor(vcostsdiff$Costs))[i]),3]
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  #       subsetH <- vcostsL[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  
  relative <- subset/subsetT
  
  
  ssr[i,] <- quantile(relative, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
  
}

ssrdf <- as.data.frame(cbind(ssr, vec_cost))
names(ssrdf) <- c("low95", "low50", "median", "up50", "up95")

pdiffr.msm.7 <- ggplot(ssrdf, aes(x=vec_cost)) +
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
       y="relative difference in rate of spread\nbetween activity groups")+
  xlim(0,1)



popno <- 2
seed <- seed.het
pop <- "het"
col <- "dodgerblue3"
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]
#     }

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


ssr <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)

for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subset <- vcostsdiff[which(vcostsdiff$Costs==levels(as.factor(vcostsdiff$Costs))[i]),3]
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  #       subsetH <- vcostsL[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  
  relative <- subset/subsetT
  
  
  ssr[i,] <- quantile(relative, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
  
}

ssrdf <- as.data.frame(cbind(ssr, vec_cost))
names(ssrdf) <- c("low95", "low50", "median", "up50", "up95")

pdiffr.het.7 <- ggplot(ssrdf, aes(x=vec_cost)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=median), alpha=1, colour=col) + # plot median
  geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
  labs(x=xlabel, 
       y="relative difference in rate of spread\nbetween activity groups")+
  xlim(0,1)
 


ins <- 2
pn <- 8
source(paste("instructions/i_", ins, ".R", sep=""))

popno <- 1
seed <- seed.msm
pop <- "msm"
col <- "darkolivegreen3"
load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]

source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

if (pn==7){
  vcostsL <- melt(allcost[,1,,1,1])
  vcostsH <- melt(allcost[,1,,2,1])
  vcostsT <- melt(allcost[,1,,3,1])
  xlabel <- "fitness cost in nduration of infection"
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


ssr <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)

for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subset <- vcostsdiff[which(vcostsdiff$Costs==levels(as.factor(vcostsdiff$Costs))[i]),3]
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  #       subsetH <- vcostsL[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  
  relative <- subset/subsetT
  
  
  ssr[i,] <- quantile(relative, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
  
}

ssrdf <- as.data.frame(cbind(ssr, vec_cost))
names(ssrdf) <- c("low95", "low50", "median", "up50", "up95")

pdiffr.msm.8 <- ggplot(ssrdf, aes(x=vec_cost)) +
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
       y="relative difference in rate of spread\nbetween activity groups")+
  xlim(0,1)



popno <- 2
seed <- seed.het
pop <- "het"
col <- "dodgerblue3"
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
sets <- outros[1:k.max, 7:11]
#     }

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
  print(paste("wrong pn"))
}
vcostsdiff <- as.data.frame(cbind(Set=vcostsL[,1], Costs=vcostsL[,2], Difference=vcostsL[,3]-vcostsH[,3]))
names(vcostsT) <- c("Set", "Costs", "Rate")


ssr <- matrix(nrow=length(levels(as.factor(vcostsdiff$Costs))), ncol=5)

for (i in 1:length(levels(as.factor(vcostsdiff$Costs)))){
  subset <- vcostsdiff[which(vcostsdiff$Costs==levels(as.factor(vcostsdiff$Costs))[i]),3]
  subsetT <- vcostsT[which(vcostsT$Costs==levels(as.factor(vcostsT$Costs))[i]),3]
  
  relative <- subset/subsetT
  
  
  ssr[i,] <- quantile(relative, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=T)
  
}

ssrdf <- as.data.frame(cbind(ssr, vec_cost))
names(ssrdf) <- c("low95", "low50", "median", "up50", "up95")

pdiffr.het.8 <- ggplot(ssrdf, aes(x=vec_cost)) +
  theme_hc() + # set plot theme
  theme(text = element_text(size=8),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "right")+
  geom_line(aes(y=median), alpha=1, colour=col) + # plot median
  geom_ribbon(aes(ymin=low50, ymax=up50), fill=col, alpha=0.4) + # plot 50% area
  geom_ribbon(aes(ymin=low95, ymax=up95), fill=col, alpha=0.2) + # plot 95% area
  labs(x=xlabel, 
       y="relative difference in rate of spread\nbetween activity groups")+
  xlim(0,1)


source("f_multipanel.R")
pdf("../figures/S1_appendixC.pdf", width=7, height=5.2)
multipanel(pdiffr.msm.7, pdiffr.msm.8, pdiffr.het.7, pdiffr.het.8, cols=2)
dev.off()
