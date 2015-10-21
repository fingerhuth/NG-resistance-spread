load("../data/surveillance.data")
library(ggplot2)
library(ggthemes)
library(stats)
library(grid)
library(MASS)

# replace "MSW" with "HetM"
levels(df$Group)[2] <- "HetM"

# set functions that should be fit to data
# a: ratio wildtype/resistant at start of simulation, b: exponential growth rate, c: capacity
# exponential function without capacity 'c' set beforehand 
fc <- function(time,a,b,c) {
  c/(1+a*exp(-b*time))
}
# exponential function with capacity 'c=1' (hypothetical value reached when resistant strain fixates in population)
f1 <- function(time,a,b) {
  1/(1+a*exp(-b*time))
}

# set x values needed for plotting (20 years)
xval <- seq(0, 20, 0.1)

# set initial parameters
init2 <- c(a=300,b=0.8)
init3 <- c(a=300,b=0.8, c=5)
init4 <- c(a=300,b=0.8, c=1)

# initialize empty data frames for plotting and rates
df.plot <- data.frame()
rates <- data.frame()

########################
# GISP Cipro MSM log-c #
########################
# combine relevant data into 'set'
report <- "GISP"
drug <- "Ciprofloxacin"
cutoff <- 2006
pop <- "MSM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

# convert percent resistance into proportion resistant
y <- set$Resistance/100
# convert years (e.g. 2000, 2001, 2003, ...) into years from start (0, 1, 2, ...)
x <- set$Year - set$Year[1]

# fit model to data
lsqf <- nls(y ~ fc(x, a, b, c), start=init3, control = list(maxiter = 500))
# extract parameter estimates
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
estc <- summary(lsqf)$coef[3]
# estimate confidence intervals around parameter estimates
conf <- confint.default(lsqf)

# summarize estimates in 'rates'
rates <- rbind(rates,
               data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                          asymptote=estc, conf.asymp.lb=conf[3], conf.asymp.ub=conf[6],
                          rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[5],
                          ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[4]))
# generate data points with estimates (used for plotting of curve later on)
dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=fc(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb, estc)*100)
# combine generates data points with other information in 'df.plot'
df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))



#########################
# GISP Cipro HetM log-1 #
#########################
# for comments see section 'GISP Cipro MSM log-c'
report <- "GISP"
drug <- "Ciprofloxacin"
cutoff <- 2006
pop <- "HetM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

y <- set$Resistance/100
x <- set$Year - set$Year[1]

lsqf <- nls(y ~ f1(x, a, b), start=c(a=300,b=0.1), control = list(maxiter = 500))
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
conf <- confint.default(lsqf)

rates <- rbind(rates, data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                                 asymptote=1, conf.asymp.lb=NA, conf.asymp.ub=NA,
                                 rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[4],
                                 ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[3]))

dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=f1(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb)*100)

df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))



#############################
# GRASP Cefixime HetM log-1 #
#############################
# for comments see section 'GISP Cipro MSM log-c'
report <- "GRASP"
drug <- "Cefixime"
cutoff <- 2010
pop <- "HetM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

y <- set$Resistance/100
x <- set$Year - set$Year[1]

lsqf <- nls(y ~ f1(x, a, b), start=c(a=300,b=0.1), control = list(maxiter = 500))
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
conf <- confint.default(lsqf)

rates <- rbind(rates, data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                                 asymptote=1, conf.asymp.lb=NA, conf.asymp.ub=NA,
                                 rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[4],
                                 ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[3]))

dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=f1(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb)*100)

df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))


############################
# GRASP Cefixime MSM log-c #
############################
# for comments see section 'GISP Cipro MSM log-c'
report <- "GRASP"
drug <- "Cefixime"
cutoff <- 2010
pop <- "MSM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

y <- set$Resistance/100
x <- set$Year - set$Year[1]

lsqf <- nls(y ~ fc(x, a, b, c), start=init4, control = list(maxiter = 500))
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
estc <- summary(lsqf)$coef[3]
conf <- confint.default(lsqf)

rates <- rbind(rates,data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                                asymptote=estc, conf.asymp.lb=conf[3], conf.asymp.ub=conf[6],
                                rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[5],
                                ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[4]))

dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=fc(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb, estc)*100)

df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))




############################
# GRASP Cefixime HetM log-c #
############################
# for comments see section 'GISP Cipro MSM log-c'
report <- "GRASP"
drug <- "Ciprofloxacin"
cutoff <- 2009
pop <- "HetM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

y <- set$Resistance/100
x <- set$Year - set$Year[1]

lsqf <- nls(y ~ fc(x, a, b, c), start=init3, control = list(maxiter = 500))
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
estc <- summary(lsqf)$coef[3]
conf <- confint.default(lsqf)

rates <- rbind(rates,data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                                asymptote=estc, conf.asymp.lb=conf[3], conf.asymp.ub=conf[6],
                                rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[5],
                                ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[4]))

dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=fc(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb, estc)*100)

df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))




#########################
# GRASP Cipro MSM log-c #
#########################
# for comments see section 'GISP Cipro MSM log-c'
report <- "GRASP"
drug <- "Ciprofloxacin"
cutoff <- 2009
pop <- "MSM"
set <- df[which(df[,4]==pop 
                & df[,3]==report
                & df[,5]==drug
                & df[,1]<=cutoff), ]

y <- set$Resistance/100
x <- set$Year - set$Year[1]

lsqf <- nls(y ~ fc(x, a, b, c), start=init4, control = list(maxiter = 500))
esta <- summary(lsqf)$coef[1]
estb <- summary(lsqf)$coef[2]
estc <- summary(lsqf)$coef[3]
conf <- confint.default(lsqf)

rates <- rbind(rates,data.frame(study=report, drug=drug, pop=pop, years=paste(set$Year[1],"-",max(set$Year), sep=""), 
                                asymptote=estc, conf.asymp.lb=conf[3], conf.asymp.ub=conf[6],
                                rate=estb, conf.rate.lb=conf[2], conf.rate.ub=conf[5],
                                ratio=esta, conf.ratio.lb=conf[1], conf.ratio.ub=conf[4]))

dset <- data.frame(Year=seq(min(x), max(x), (max(x)-min(x))/100)+set$Year[1], Resistance=fc(seq(min(x), max(x), (max(x)-min(x))/100), esta, estb, estc)*100)

df.plot <- rbind(df.plot, cbind(set, Sim=0), cbind(dset, Dataset=report, Group=pop, Drug=drug, Sim=1))

df.plot$Group <- factor(df.plot$Group, levels=c("HetM", "MSM"))

all <- ggplot() +
  theme_hc()+
  theme(
    plot.margin = unit(c(2,2,0,0), "pt"),
    line = element_line(size=0.5),
    panel.grid = element_line(colour="black"),
    plot.title = element_text(vjust=2),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
#     legend.text = element_text(size=16),
    axis.text.x  = element_text(size=8),
    legend.key.size = unit(1, "cm"),
    legend.title = element_text(size=8),
    legend.position = "none",
    strip.background = element_rect(fill = "khaki1",size = 1)
  ) +
  geom_line(data=df.plot[which(df.plot$Sim==1),], size=0.5, linetype="dashed", aes(x=Year, y=Resistance, colour=Group)) +
  geom_point(data=df.plot[which(df.plot$Sim==0),], size=2.5, aes(x=Year, y=Resistance, colour=Group)) +
  scale_colour_manual(values=c("dodgerblue3","darkolivegreen3"))+
  labs(y=expression(atop("proportion antibiotic-resistant", italic("N. gonorrhoeae") *" (in %)")), x="Year") +
  xlim(1995,2010) + 
  ylim(0,55)



# mean of rates
mean.msm <- mean(rates[c(1,4,6), 8])
mean.het <- mean(rates[c(2,3,5), 8])

# doubling times
doubling <- log(2)/rates$rate
doubling.mean.msm <- log(2)/mean.msm
doubling.mean.het <- log(2)/mean.het


# cairo_ps("../figures/Fig2.eps", width=10, height=7)
# all + facet_wrap( ~ Dataset + Drug + Group, ncol=2) +
#   theme(text = element_text(size = 18))
# dev.off()
 
save("rates", file="../data/rates.data")

tiff("../figures/Fig2.tiff", width=10/1.93, height=7/1.93, unit="in", compression="lzw", res=350, family="Arial")
all + facet_wrap( ~ Dataset + Drug + Group, ncol=2) +
  theme(text = element_text(size = 10))
dev.off()
