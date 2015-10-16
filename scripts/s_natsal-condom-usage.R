library(stats4)

load("../data/natsal.RData")
nat <- natsal

### MEN AGED 16 - 44 YEARS
from <- 16
to <- 44


### SUBSET OF DATA
cond.msm <- nat$rsex == 1 & nat$homnonew >= 0 & nat$homnonew < 995 & nat$dage >= from & nat$dage <= to &  nat$homever==1
dat.msm <- nat$homco4w[cond.msm]
wei.msm <- nat$total_wt[cond.msm]

### SUBSET OF DATA
# variable 'cond4wk': condom use for het. sex in last 4 weeks
# natsal documentation states that 99 means 'missing' in hetnonew, but depending variables indicate 999 means 'missing'
cond.het <- (nat$rsex == 1 || nat$rsex == 2) & nat$hetnonew >= 0 & nat$hetnonew < 999 & nat$dage >= from & nat$dage <= to &  nat$homever==0
dat.het <- nat$cond4wk[cond.het]
wei.het <- nat$total_wt[cond.het]


## answer possibilities (Natsal Documentation page 126 (page number 33))
# 1: yes, used on every occasion
# 2: yes, used on some occasions
# 3: not used in last 4 weeks
# 4: not had vaginal/anal sex in last 4 weeks
# 9: not answered
# -1: not applicable

# unweighted
condom.yes.msm <- length(c(which(dat.msm==1), which(dat.msm ==2)))
condom.no.msm <- length(dat.msm[which(dat.msm==3)])
fraction.using.msm <- condom.yes.msm/(condom.yes.msm+condom.no.msm)

condom.yes.het <- length(c(which(dat.het==1), which(dat.het ==2)))
condom.no.het <- length(dat.het[which(dat.het==3)])
fraction.using.het <- condom.yes.het/(condom.yes.het+condom.no.het)

# weighted
ind.condom.yes.msm <- c(which(dat.msm==1), which(dat.msm ==2))
weight.yes.msm <- wei.msm[ind.condom.yes.msm]
ind.condom.no.msm <- dat.msm[which(dat.msm==3)]
weight.no.msm <- wei.msm[ind.condom.no.msm]
sum.yes.msm <- sum(weight.yes.msm)
sum.no.msm <- sum(weight.no.msm)
weight.using.msm <- sum.yes.msm/(sum.yes.msm+sum.no.msm)

ind.condom.yes.het <- c(which(dat.het==1), which(dat.het ==2))
weight.yes.het <- wei.het[ind.condom.yes.het]
ind.condom.no.het <- dat.het[which(dat.het==3)]
weight.no.het <- wei.het[ind.condom.no.het]
sum.yes.het <- sum(weight.yes.het)
sum.no.het <- sum(weight.no.het)
weight.using.het <- sum.yes.het/(sum.yes.het+sum.no.het)


# more condoms used in msm than in het?
weight.using.msm > weight.using.het



# always used condom: more in msm than in het?
sum(wei.msm[which(dat.msm==1)])/(sum(wei.msm[which(dat.msm==1)])+sum(wei.msm[which(dat.msm==2)])+sum(wei.msm[which(dat.msm==3)])) >
sum(wei.het[which(dat.het==1)])/(sum(wei.het[which(dat.het==1)])+sum(wei.het[which(dat.het==2)])+sum(wei.het[which(dat.het==3)])) 