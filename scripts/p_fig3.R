# set seeds
seed.het <- 993734
seed.msm <- 312774

# load data sets
## outros: parameter sets that are within the calibration rates
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


# Function multipanel
## Multiple plot function with alphabetical panel labelling
## based on mutiplot (http://www.r-bloggers.com/r-good-practice-%E2%80%93-adding-footnotes-to-graphics/, accessed 14/05/2015)
## SPECIFIC FOR 2x3!!!! BECAUSE OF LEGEND, GENERAL FUNCTION IN f_multipanel.R
multipanel <- function(..., plotlist=NULL, file, legend, cols=1, layout=NULL) {
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
      panel.label(paste("(", letters[i], ")", sep=""), vp=viewport(layout.pos.row = matchidx$row,
                                                                   layout.pos.col = matchidx$col))
    }
    i <- 6
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    print(legend, vp = viewport(layout.pos.row = matchidx$row,
                                    layout.pos.col = matchidx$col))
  }
}


# make data frames (hetersexual men and women)
df.het <- as.data.frame(outros.het)
# calculate incidence of diagnosed and treated infections
df.het[,12] <- outros.het[,6]*outros.het[,11] 
# rename columns: 
# prev = prevalence, inc = incidence, visInc = incidence of diagnosed infections, 
# L = low actvitiy group, H = high actvitity group, T = total of both activity groups
# epsilon = sexual mixing coefficient, beta = transmission probability within (low/high) activity group, D = average duration of infection, f = fraction diagnosed and treated infections
colnames(df.het) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f", "visIncT")

# make data.frames (men who have sex with men)
# comments see previous section
df.msm <- as.data.frame(outros.msm)
df.msm[,12] <- outros.msm[,6]*outros.msm[,11]
colnames(df.msm) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f", "visIncT")

# calculate means, medians, IQR
means.het <- apply(df.het, 2, mean)
means.msm <- apply(df.msm, 2, mean)
medians.het <- apply(df.het, 2, median)
medians.msm <- apply(df.msm, 2, median)
Q3.het <- apply(df.het, 2, function(x) quantile(x, 3/4))
Q3.msm <- apply(df.msm, 2, function(x) quantile(x, 3/4))
Q1.het <- apply(df.het, 2, function(x) quantile(x, 1/4))
Q1.msm <- apply(df.msm, 2, function(x) quantile(x, 1/4))

# generate prior distribution (for plotting)
## epsilon, betaL, f: random uniform distribution between 0 and 1
## betaH: random uniform distribution between 0 and maximum of betaL; because betaL is drawn from random uniform distribution between 0 and 1, an equivilant sample is drawn to give vector of upper limits
## D: gamma distribution with mean 3 months and scale parameter 2, in years
epsilon <- as.data.frame(cbind(x=seq(0,1,1/100),y=dunif(seq(0,1,0.01), 0,1)))
betaL <- as.data.frame(cbind(x=seq(0,1,1/100), y=dunif(seq(0,1,0.01), 0,1)))
betaH <- data.frame(betaH=runif(10^7,0,runif(10^7, 0, 1)))
D <- data.frame(D=rgamma(seq(0,24,24/(10^6)), shape=2, scale=3/2)/12)
f <- as.data.frame(cbind(x=seq(0,1,1/100), y=dunif(seq(0,1,0.01), 0,1)))

# generate plots
# epsilon
pepsilon <- ggplot() +
  theme_hc()+ # set plot theme
  theme(text=element_text(size=8)) + # set text size
  geom_histogram(data=df.het, aes(x=epsilon, y=..density..), fill="dodgerblue3", size=0.1,  colour="dodgerblue3", alpha=0.6, binwidth=0.02) + # plot heterosexual men and women posterior histogram
  geom_histogram(data=df.msm, aes(x=epsilon, y=..density..), fill="darkolivegreen3", size=0.1,  colour="darkolivegreen3", alpha=0.6, binwidth=0.02) + # plot men who have sex with men posterior histogram
  geom_density(data=epsilon, aes(x=x, y=y), fill="khaki1", size=0.5, colour="khaki1", alpha=0.3)  + # plot prior density distribution
  ylim(c(0,5)) + # set y limits
  xlim(c(0,1)) + # set x limits
  labs(x="sexual mixing coefficient \n") # rename x axis

# betaL
# comments see 'pepsilon'
pbetaL <- ggplot()+
  theme_hc()+
  theme(text=element_text(size=8)) +
  geom_histogram(data=df.het, aes(x=betaL, y=..density..), fill="dodgerblue3", size=0.1,  colour="dodgerblue3", alpha=0.6, binwidth=0.02) +
  geom_histogram(data=df.msm, aes(x=betaL, y=..density..), fill="darkolivegreen3", size=0.1,  colour="darkolivegreen3", alpha=0.6, binwidth=0.02) +
  geom_density(data=betaL, aes(x=x, y=y), fill="khaki1", size=0.5, colour="khaki1", alpha=0.3)  +
  ylim(c(0,5)) +
  xlim(c(0,1)) +
  labs(x="transmission probability within \n low activity group")

# betaH
pbetaH <- ggplot()+
  theme_hc()+
  theme(text=element_text(size=8)) +
  geom_histogram(data=df.het, aes(x=betaH, y=..density..), fill="dodgerblue3", size=0.1,  colour="dodgerblue3", alpha=0.6, binwidth=0.02)+
  geom_histogram(data=df.msm, aes(x=betaH, y=..density..), fill="darkolivegreen3", size=0.1,  colour="darkolivegreen3", alpha=0.6, binwidth=0.02)+
  geom_density(data=betaH, aes(x=betaH), fill="khaki1", size=0.5, colour="khaki1", alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,1)) +
  labs(x="transmission probability within \n high activity group")

# D
## for plotting converted to months (*12)
## comments see 'pepsilon'
pD <- ggplot()+
  theme_hc()+
  theme(text=element_text(size=8)) +
  geom_histogram(data=df.het, aes(x=D*12, y=..density..), fill="dodgerblue3", size=0.1,  colour="dodgerblue3", alpha=0.6, binwidth=0.5)+
  geom_histogram(data=df.msm, aes(x=D*12, y=..density..), fill="darkolivegreen3", size=0.1,  colour="darkolivegreen3", alpha=0.6, binwidth=0.5)+
  geom_density(data=D, aes(x=D*12), fill="khaki1", size=0.5, colour="khaki1", alpha=0.3)+
  xlim(0,max(df.het$D*12, df.msm$D*12, unlist(D)*12))+
  scale_x_continuous(breaks=c(0,6,12,18,24))+ # set nice brekas for x axis labels
  labs(x="average duration of \n infection (months)")

# f
## comments see 'pepsilon'
pf <- ggplot()+
  theme_hc()+
  theme(text=element_text(size=8)) +
  geom_histogram(data=df.het, aes(x=f, y=..density..), fill="dodgerblue3", size=0.1, colour="dodgerblue3", alpha=0.5, binwidth=0.02) +
  geom_histogram(data=df.msm, aes(x=f, y=..density..), fill="darkolivegreen3", size=0.1, colour="darkolivegreen3", alpha=0.5, binwidth=0.02) +
  geom_density(data=f, aes(x=x, y=y), fill="khaki1", size=0.5, colour="khaki1", alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,1)) +
  labs(x="fraction of diagnosed\nand treated infections") 

# dummy plot for legend
## since I only want one legend for the entire 2x3 plot, I generate an empty plot over which I plot the legend
## in this way I can include the legend as a panel for function multipanel
empty <- data.frame(x=1:3, y=c(4,3,7), col=letters[1:3])

pdummy <- ggplot()+
  geom_histogram(data=df.het, aes(x=betaL, y=..density..), fill="white", colour="white", binwidth=0.02) +
  geom_histogram(data=df.msm, aes(x=betaL, y=..density..), fill="white", colour="white", binwidth=0.02) +
  geom_density(data=betaL, aes(x=x, y=y), fill="white", colour="white")  +
  ylim(c(0,5)) +
  xlim(c(0,1)) +
  labs(x="transmission probability within \n low activity group")

pl <- pdummy + 
  theme(
    panel.background = element_rect(fill="white", colour="white"),
    panel.grid.major = element_line(colour="white"),
    panel.grid.minor = element_line(colour="white"),
    axis.text = element_text(colour="white"),
    axis.ticks = element_line(colour="white"), 
    axis.title = element_text(colour="white")
  ) +
  theme(legend.position = c(.5, .4))+
  geom_tile(data=empty, aes(x=x, y=y, fill=col), linetype="blank", alpha=0.6) +
  scale_fill_manual(values=c("dodgerblue3","darkolivegreen3","khaki1"),
                    labels=c("HMW", "MSM", "Prior"))+
  guides(fill=guide_legend(title=NULL, 
                           direction = "vertical", 
                           label.position="right",
                           keywidth = 1, 
                           keyheight = 1, 
                           label.hjust = 0.5,
                           label.theme=element_text(size=8, angle=0)))

# generate plot (with 5 plot panels + 1 legend panel)
# cairo_ps("../figures/Fig3.eps", width=14, height=10)
# multipanel(pepsilon, pf, pD, pbetaL, pbetaH, legend=pl, cols=3)
# dev.off()

tiff("../figures/Fig3.tiff", width=14/2.7, height=10/2.7, unit="in", res=350, compression="lzw", family="Arial")
multipanel(pepsilon, pf, pD, pbetaL, pbetaH, legend=pl, cols=3)
dev.off()
