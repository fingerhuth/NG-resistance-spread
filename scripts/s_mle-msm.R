library(stats4)

load("../data/natsal.RData")
nat <- natsal

### MEN AGED 16 - 44 YEARS
from <- 16
to <- 44

### SUBSET OF DATA
data <- nat$homnonew[nat$rsex == 1 & nat$homnonew >= 0 & nat$homnonew < 995 & nat$dage >= from & nat$dage <= to &  nat$homever==1]
wt <- nat$total_wt[nat$rsex == 1 & nat$homnonew >= 0 & nat$homnonew < 995 & nat$dage >= from & nat$dage <= to &  nat$homever==1]

##### MLE of two poisson processes #####
f <- function(x,a,m1,m2) {
	a*dpois(x,m1)+(1-a)*dpois(x,m2)
}
nll <- function(a,m1,m2) {
	-sum(wt*log(f(data,a,m1,m2)))
}
(est2 <- mle(nll,start=list(a=0.9,m1=1,m2=10)))

behav <- coef(est2)
save("behav", file="../data/behav_msm.data")
