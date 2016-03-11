#  load libraries
library(ggplot2)
library(ggthemes)

ins <- 6
source(paste("instructions/i_", ins, ".R", sep=""))
pn <- pnv
source(paste("pn_", pn, "/s_d-init-mutation.R", sep=""))

yearsRes <- 100
timesRes <- seq(0,yearsRes,1/12)

for (popno in 1:2){
  if(popno == 1){
    seed <- seed.msm
    pop <- "msm"
#     col <- "dodgerblue3"
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allpropresT.data", sep=""))
    sum.msm <- c()
    for(k in 1:k.max){
      for(l in 1:length(vec_mu_ta)){
        sum.msm <- rbind(sum.msm, cbind(Set=k, Mutation=vec_mu_ta[l], Time5=timesRes[which(allpropresT[k,l,]>=0.05)[1]], pop=1))
      }
    }
    sum.msm <- as.data.frame(sum.msm)
  }else{
    seed <- seed.het
    pop <- "het" 
#     col <- "darkolivegreen3"
    load(paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allpropresT.data", sep=""))
    sum.het <- c()
    for(k in 1:k.max){
      for(l in 1:length(vec_mu_ta)){
        sum.het <- rbind(sum.het, cbind(Set=k, Mutation=vec_mu_ta[l], Time5=timesRes[which(allpropresT[k,l,]>=0.05)[1]], pop=2))
      }
    }
    sum.het <- as.data.frame(sum.het)
  }
}

pTdf <- rbind(sum.msm, sum.het)
names(pTdf) <- c("Set", "Mutation", "Time5", "Group")

pTdf$Mutation <- as.factor(pTdf$Mutation)
pTdf$Group <- as.factor(pTdf$Group)




col.msm <- "darkolivegreen3"
col.het <- "dodgerblue3"
bg.col <- "white"

tiff("../figures/S3Fig.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
# pdf("../figures/S4Fig2.pdf", width=10/1.93, height=7/1.93)
bp <-   ggplot(pTdf, aes(x=Group, y=Time5))+
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
  labs(x="probability of resistance during treatment", y="time to 5% resistance (in years)") +
  scale_colour_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW"))+ # set colours and label names in legend
  scale_fill_manual(guide_legend(title=""), values=c(col.msm, col.het), labels=c("MSM", "HMW")) # set fill (colours) and label names in legend
#   scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7), limits=c(0,7))

#   scale_x_discrete(breaks=c("       ", "men who have\nsex with men", "     ", "heterosexuals", "      ")) +
#   stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })

bp + facet_wrap( ~ Mutation, nrow=1)
dev.off()  
