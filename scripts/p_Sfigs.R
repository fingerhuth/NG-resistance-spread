# set seeds
seed.het <- 993734
seed.msm <- 312774

# load data
load(paste("../data/outros_het-", seed.het, "-het.data", sep=""))
outros.het <- outros
load(paste("../data/outros_msm-", seed.msm, "-msm.data", sep=""))
outros.msm <- outros


# load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(grDevices)
library(ggthemes)

## Function
# Multiple plot function with alphabetical panel labelling
# based on mutiplo
multipanel <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # function for adding panel labels
  # adapted form http://www.r-bloggers.com/r-good-practice-%E2%80%93-adding-footnotes-to-graphics/, accessed 14/05/2015
  panel.label <- function(text="",size= 1, color="black", vp=viewport()){
    require(grid)
    pushViewport(vp)
    grid.text(label= text ,
              x = unit(0.02,"npc"),
              y= unit(0.95,"npc"),
              just=c("left", "bottom"),
              gp=gpar(fontsize=8, col=color))
    popViewport()
  }
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow=T)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      # Print alphabetical panel label
      panel.label(paste("(", letters[i], ")", sep=""), vp=viewport(layout.pos.row = matchidx$row,
                                                                   layout.pos.col = matchidx$col))
    }
  }
}

# convert outros to data frames, add incidence of diagnosed infections for both activity groups ("visIncT"), name data frame columns
df.het <- as.data.frame(outros.het)
df.het[,12] <- outros.het[,6]*outros.het[,11]
colnames(df.het) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f", "visIncT")

# see previous comment
df.msm <- as.data.frame(outros.msm)
df.msm[,12] <- outros.msm[,6]*outros.msm[,11]
colnames(df.msm) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f", "visIncT")


# generate one data frame with prevalences of heterosexual men and women
df.het.prevL <- data.frame(Value=as.numeric(df.het$prevL), Group="low activity group")
df.het.prevH <- data.frame(Value=as.numeric(df.het$prevH), Group="high activity group")
df.het.prevT <- data.frame(Value=as.numeric(df.het$prevT), Group="total population")
df.het.prev <- as.data.frame(rbind(df.het.prevL, df.het.prevH, df.het.prevT))

# plot prevalences for heterosexual men and women
p.het.prev <- ggplot()+
  theme_hc()+
  theme(text = element_text(size=8))+
  theme(strip.background = element_rect(fill = "khaki1",size = 1))+
  geom_histogram(data=df.het.prev, aes(x=Value*100, y=..density..), fill="dodgerblue3", size=0.1, colour="dodgerblue3", alpha=0.5)+
  xlab("prevalence (in %)")

# seperate previous plot into three panels (according to group)
multi.het.prev <- p.het.prev + facet_wrap( ~ Group, ncol=3, scales="free")

# generate one data frame with incidences of heterosexual men and women
df.het.incL <- data.frame(Value=as.numeric(df.het$incL*df.het$f), Group="low activity group")
df.het.incH <- data.frame(Value=as.numeric(df.het$incH*df.het$f), Group="high activity group")
df.het.incT <- data.frame(Value=as.numeric(df.het$incT*df.het$f), Group="total population")
df.het.inc <- as.data.frame(rbind(df.het.incL, df.het.incH, df.het.incT))

# plot incidences for heterosexual men and women
p.het.inc <- ggplot()+
  theme_hc()+
  theme(text = element_text(size=8))+
  theme(strip.background = element_rect(fill = "khaki1",size = 1))+
  geom_histogram(data=df.het.inc, aes(x=Value*100000, y=..density..), fill="dodgerblue3", size=0.1, colour="dodgerblue3", alpha=0.5)+
  xlab("incidence of diagnosed infections (per 100 000 persons per year)")

# seperate previous plot into three panels (according to group)
multi.het.inc <- p.het.inc + facet_wrap( ~ Group, ncol=3, scales="free")

# cairo_ps("../figures/S2Fig.eps", width=10, height=7)
# multipanel(multi.het.prev, multi.het.inc)
# dev.off()

tiff("../figures/S2Fig.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
multipanel(multi.het.prev, multi.het.inc)
dev.off()

# generate one data frame with prevalences of men who have sex with men
df.msm.prevL <- data.frame(Value=as.numeric(df.msm$prevL), Group="low activity group")
df.msm.prevH <- data.frame(Value=as.numeric(df.msm$prevH), Group="high activity group")
df.msm.prevT <- data.frame(Value=as.numeric(df.msm$prevT), Group="total population")
df.msm.prev <- as.data.frame(rbind(df.msm.prevL, df.msm.prevH, df.msm.prevT))

# plot prevalences for men who have sex with men
p.msm.prev <- ggplot()+
  theme_hc()+
  theme(text = element_text(size=8))+
  theme(strip.background = element_rect(fill = "khaki1",size = 1))+
  geom_histogram(data=df.msm.prev, aes(x=Value*100, y=..density..), fill="darkolivegreen3", size=0.1, colour="darkolivegreen3", alpha=0.5)+
  xlab("prevalence (in %)")

# seperate previous plot into three panels (according to group)
multi.msm.prev <- p.msm.prev + facet_wrap( ~ Group, ncol=3, scales="free")

# generate one data frame with incidences of men who have sex with men
df.msm.incL <- data.frame(Value=as.numeric(df.msm$incL*df.msm$f), Group="low activity group")
df.msm.incH <- data.frame(Value=as.numeric(df.msm$incH*df.msm$f), Group="high activity group")
df.msm.incT <- data.frame(Value=as.numeric(df.msm$incT*df.msm$f), Group="total population")
df.msm.inc <- as.data.frame(rbind(df.msm.incL, df.msm.incH, df.msm.incT))

# plot incidences for men who have sex with men
p.msm.inc <- ggplot()+
  theme_hc()+
  theme(text = element_text(size=8))+
  theme(strip.background = element_rect(fill = "khaki1",size = 1))+
  geom_histogram(data=df.msm.inc, aes(x=Value*100000, y=..density..), fill="darkolivegreen3", size=0.1, colour="darkolivegreen3", alpha=0.5)+
  scale_x_continuous(breaks=c(0,1000,2000,3000,6000,6400,6800,7200,70000,100000,130000))+ # set nice brekas for x axis labels
  xlab("incidence of diagnosed infections (per 100 000 persons per year)")

# seperate previous plot into three panels (according to group)
multi.msm.inc <- p.msm.inc + facet_wrap( ~ Group, ncol=3, scales="free")

# cairo_ps("../figures/S1Fig.eps", width=10, height=7)
# multipanel(multi.msm.prev, multi.msm.inc)
# dev.off()

tiff("../figures/S1Fig.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
multipanel(multi.msm.prev, multi.msm.inc)
dev.off()
