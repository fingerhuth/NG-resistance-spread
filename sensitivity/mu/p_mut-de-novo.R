#  load libraries
library(ggplot2)
library(ggthemes)

source(paste("instructions/i_", ins, ".R", sep=""))
pn <- pnv
source(paste("pn_", pn, "/s_d-init-mutation.R", sep=""))

for (popno in 1:2){
  if(popno == 1){
    seed <- seed.msm
    pop <- "msm"
    col <- "darkolivegreen3"
    t.max <- 15
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmutno.data", sep=""))
    cummu.msm <- apply(allmutno[,,], c(1,2), sum) # calculate cumulative de novo mutations over entire timespan simulated for each mutation rate
    sum.msm <- c()
    for(k in 1:k.max){
      for(l in 1:length(vec_mu_ta)){
        sum.msm <- rbind(sum.msm, cbind(Set=k, Mutation=vec_mu_ta[l], denovo=cummu.msm[k,l], pop=1))
      }
    }
    sum.msm <- as.data.frame(sum.msm)
  }else{
    seed <- seed.het
    pop <- "het" 
    col <- "dodgerblue3"
    t.max <- 75
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmutno.data", sep=""))
    cummu.het <- apply(allmutno[,,], c(1,2), sum) # calculate cumulative de novo mutations over entire timespan simulated for each mutation rate
    sum.het <- c()
    for(k in 1:k.max){
      for(l in 1:length(vec_mu_ta)){
        sum.het <- rbind(sum.het, cbind(Set=k, Mutation=vec_mu_ta[l], denovo=cummu.het[k,l], pop=2))
      }
    }
    sum.het <- as.data.frame(sum.het)
  }
}
  
  

pTdf <- rbind(sum.msm, sum.het)
names(pTdf) <- c("Set", "Mutation", "Denovo", "Group")

pTdf$Mutation <- as.factor(pTdf$Mutation)
pTdf$Group <- as.factor(pTdf$Group)




col.msm <- "darkolivegreen3"
col.het <- "dodgerblue3"
bg.col <- "white"

# tiff("../figures/S4Fig2.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
pdf("../figures/RFig1.pdf", width=10/1.93, height=7/1.93)
bp <-   ggplot(pTdf, aes(x=Group, y=Denovo))+
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
  scale_y_continuous(trans="log10")+
  scale_colour_manual(values=c(col.msm, col.het)) + 
  scale_fill_manual(values=c(col.msm, col.het)) + 
  labs(x="probability of resistance during treatment", y="number of de novo resistance mutations") +
  scale_colour_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW"))+ # set colours and label names in legend
  scale_fill_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW")) # set fill (colours) and label names in legend
#   scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7), limits=c(0,7))

#   scale_x_discrete(breaks=c("       ", "men who have\nsex with men", "     ", "heterosexuals", "      ")) +
#   stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })

bp + facet_wrap( ~ Mutation, nrow=1)
dev.off()  
