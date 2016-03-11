# inititalize parameters, simulate and fit

args <- commandArgs(trailingOnly=T)

pop <- as.character(args[1])
seed <- as.numeric(args[2])
theta.batch <- as.numeric(args[3])
batch.no <- as.numeric(args[4])
pn <- as.numeric(args[5])
ins <- as.numeric(args[6])

library(deSolve)
library(rootSolve)
# library(rootSolve, lib.loc="../lib/")

source("f_mixing.R")
source(paste("pn_", pn, "/f_model.R", sep=""))

load(paste("../data/behav_", pop, ".data", sep=""))
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
theta <- outros[,7:11]

source(paste("pn_", pn, "/s_c-init-fixed.R", sep=""))
source(paste("pn_", pn, "/s_d-init-mutation.R", sep=""))
source(paste("pn_", pn, "/s_e-init-structures.R", sep=""))

mci <- 1 #mutcomparison index, increases with k
for(k in (1+batch.no*theta.batch):((1+batch.no)*theta.batch)){
  
  source(paste("pn_", pn, "/s_f-init-varying-free.R", sep=""))
  free.out <- runsteady(init, c(0,years), model, parms)
  
  for (l in 1:length(vec_mu_ta)){
      mu_ta <- vec_mu_ta[l]
      
      source(paste("pn_", pn, "/s_g-init-res.R", sep=""))
      res.out <- lsoda(init, timesRes, model, parms)
      source(paste("pn_", pn, "/s_h-proportion-res.R", sep=""))
      source(paste("pn_", pn, "/s_i-fit.R", sep=""))
      
    } # end l
  mci <- mci+1
} # end k

source(paste("pn_", pn, "/s_j-save.R", sep=""))





