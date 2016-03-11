# initialize necessary data structures

mutcomp <- array(data=NA,dim=c(theta.batch, length(vec_mu_ta), 3, 3)) # cost comparison
mutno <- array(data=NA,dim=c(theta.batch, length(vec_mu_ta), length(timesRes))) # number of mutations

propresT<- array(data=NA,dim=c(theta.batch, length(vec_mu_ta), length(timesRes)))

nofit <- c()
corrfit <- c()
