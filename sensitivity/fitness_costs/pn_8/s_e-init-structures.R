# initialize necessary data structures
costcomp <- array(data=NA,dim=c(theta.batch, length(vec_cost_b), length(vec_cost_v),3,3)) # cost comparison

nofit <- c()
corrfit <- c()
