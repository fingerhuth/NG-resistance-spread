args = commandArgs(trailingOnly=T)

pop <- as.character(args[1])
seed <- as.numeric(args[2])
cond <- as.character(args[3])

source("f_model.R")
source("f_mixing.R")
load(paste("../data/outros_", pop, "-",seed, "-", cond, ".data", sep=""))
library(deSolve)
library(rootSolve)

noG <- 2
years <- 500
yearsRes <- 50
times <- seq(0,years,1)
timesRes <- seq(0,yearsRes,1/12)

load(paste("../data/behav_", pop, ".data", sep=""))
N <- behav[1]
N[2] <- 1-N
c <- behav[2:3]
propresL<- matrix(data=NA, nrow=length(timesRes), ncol=nrow(outros))
propresH<- matrix(data=NA, nrow=length(timesRes), ncol=nrow(outros))
propresT<- matrix(data=NA, nrow=length(timesRes), ncol=nrow(outros))

eff <- 1
part <- 0

# efficacies: ex_y: efficacy of treamtent y in strain x
e0_a <- eff 
  
# partial resistances: 1-efficacy
ea_a <- part
  
# mutation rates (SPONTANEOUS MUTATIONS)
mu_a0 <- 0
mu_0a <- 0
  
# mutation rates (treatment induced)
# mu_ta <- 0 # see below

# parameters currently fixed
alpha <- 1/(44-16+1)
gamma <- 1

theta <- outros[,7:11]    
for (k in 1:nrow(theta)){
  epsilon <- theta[k,1]
  betaL <- theta[k,2]
  betaH <- theta[k,3]
  D <- theta[k,4]
  f <- theta[k,5]
  
  # initialize populations
  init <- c(N[1]*0.995,N[2]*0.90,N[1]*0.005, N[2]*0.1, 0, 0)
  
  # dependent parameters
  tt <- f/D
  ta <- c(tt,tt)
  
  v <- 1/D - tt
  rho <- mixing(epsilon, noG, c, N)
  beta <- matrix(c(betaL,sqrt(betaL*betaH),sqrt(betaL*betaH),betaH),nrow=noG,ncol=noG)
  
  # run model into resistant-free equilibrium
  mu_ta <- 0
  parms  <- c(c=c, noG=noG, alpha=alpha, v=v, gamma=gamma, rho=rho, 
              ta=ta, 
              e0_a=e0_a, 
              ea_a=ea_a, 
              mu_ta=mu_ta,
              mu_0a=mu_0a, 
              mu_a0=mu_a0
  )
  temp.out <- runsteady(init, c(0,years), model, parms)

  # run model into resistant equilibrium
  mu_ta <- 10^(-3)
  parms  <- c(c=c, noG=noG, alpha=alpha, v=v, gamma=gamma, rho=rho, 
              ta=ta, 
              e0_a=e0_a, 
              ea_a=ea_a, 
              mu_ta=mu_ta,
              mu_0a=mu_0a, 
              mu_a0=mu_a0
  )
  init <- unlist(temp.out[1])
  out <- lsoda(init, timesRes, model, parms)

  # proportion resistance
  propresL[,k] <- out[,6]/rowSums(out[,c(4,6)]) # proportion resistant in low risk: RES_L/(RES_L+WT_L)
  propresH[,k] <- out[,7]/rowSums(out[,c(5,7)]) # proportion resistant in high risk: RES_H/(RES_H+WT_H)
  propresT[,k] <- rowSums(out[,6:7])/rowSums(out[,4:7]) # proportion resistant in total pop: (RES_L+RES_H)/(RES_L+RES_H+WT_L+WT_H)
}

save("timesRes", file=paste("../data/times_", pop, "_", seed, ".data", sep=""))
save("propresL", file=paste("../data/propresL_", pop, "_", seed, ".data", sep=""))
save("propresH", file=paste("../data/propresH_", pop, "_", seed, ".data", sep=""))
save("propresT", file=paste("../data/propresT_", pop, "_", seed, ".data", sep=""))
