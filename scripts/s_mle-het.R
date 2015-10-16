library(stats4)

load("../data/natsal.RData")
nat <- natsal

### MEN & WOMEN AGED 16 - 44 YEARS
from <- 16
to <- 44

### SUBSET OF DATA
# natsal documentation states that 99 means 'missing' in hetnonew, but depending variables indicate 999 means 'missing'
da <- nat$hetnonew[(nat$rsex == 1 || nat$rsex == 2) & nat$hetnonew >= 0 & nat$hetnonew < 999 & nat$dage >= from & nat$dage <= to &  nat$homever==0]
data <- as.numeric(na.omit(da))
w <- nat$total_wt[(nat$rsex == 1 || nat$rsex == 2) & nat$hetnonew >= 0 & nat$hetnonew < 999 & nat$dage >= from & nat$dage <= to &  nat$homever==0]
wt <- as.numeric(na.omit(w))

##### MLE of two poisson processes #####
f <- function(x,a,m1,m2) {
	a*dpois(x,m1)+(1-a)*dpois(x,m2)
}
nll <- function(a,m1,m2) {
	-sum(wt*log(f(data,a,m1,m2)))
}
(est2 <- mle(nll,start=list(a=0.9,m1=1,m2=10)))

behav <- coef(est2)
save("behav", file="../data/behav_het.data")
