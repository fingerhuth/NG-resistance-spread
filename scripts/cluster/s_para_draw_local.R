args = commandArgs(trailingOnly=T)
pop <- args[1] # population to be simulated (MSM or MSW)
n<- args[2] # number of initial parameter sets to be drawn
seed4seeds <- args[3] # seed for random numbers

## 1a) Draw theta from prior distributions 
source("s_para_sampling.R")
set.seed(seed4seeds)
seeds <- round(runif(5)*10^6)

theta <- sample.theta(n=n, seeds=seeds)
save(theta, file=paste("../../data/theta", seed4seeds,".data", sep=""))

  
