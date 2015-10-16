args = commandArgs(trailingOnly=T)

j <- as.numeric(args[1])
pop <- as.character(args[2])
seed <- as.numeric(args[3])

source("../f_model.R")
source("../f_mixing.R")
load(paste("../../data/theta",seed,".data", sep=""))
library(deSolve)
library(rootSolve)

noG <- 2
years <- 500
times <- seq(0,years,1)

load(paste("../../data/behav_", pop, ".data", sep=""))
N <- behav[1]
N[2] <- 1-N
c <- behav[2:3]
printh <- matrix(data=NA, nrow=1000, ncol=11)

eff <- 1
part <- 0

# efficacies: ex_y: efficacy of treamtent y in strain x
e0_a <- eff 
  
# partial resistances: 1-efficacy
ea_a <- part
  
# mutation rates (SPONTANEOUS MUTATIONS)
mu_0a <- 0
mu_a0 <- 0
  
# mutation rates (treatment induced)
mu_ta <- 0

# parameters currently fixed
alpha <- 1/(44-16+1)
gamma <- 1
    
for (k in (1+j*(10^3)):(j*(10^3)+(10^3))){
  epsilon <- theta[k,1]
  betaL <- theta[k,2]
  betaH <- theta[k,3]
  D <- theta[k,4] #from months to years
  f <- theta[k,5]
  
  # initialize populations
  init <- c(N[1]*0.995,N[2]*0.90,N[1]*0.005, N[2]*0.1, 0, 0)
  
  # dependent parameters
  tt <- f/D
  ta <- c(tt,tt)
  
  v <- 1/D - tt
  rho <- mixing(epsilon, noG, c, N)
  beta <- matrix(c(betaL,sqrt(betaL*betaH),sqrt(betaL*betaH),betaH),nrow=noG,ncol=noG)
  
  # run model
  parms  <- c(c=c, noG=noG, alpha=alpha, v=v, gamma=gamma, rho=rho, 
              ta=ta, 
              e0_a=e0_a, 
              ea_a=ea_a, 
              mu_0a=mu_0a, 
              mu_a0=mu_a0
  )
  temp.out <- runsteady(init, c(0,years), model, parms)
  out <- unlist(c(attr(temp.out, "time"), temp.out[1], temp.out[2]))
  
  # prevalence & incidence
  printh[k-j*(10^3),] <- c(sum(out[c(4,6)])/N[1], #prevL
                  sum(out[c(5,7)])/N[2], #prevH
                  sum(out[4:7]), #prevT
                  sum(out[c(8,10)])/N[1], #incL
                  sum(out[c(9,11)])/N[2], #incH
                  sum(out[8:11]), #incT
                  theta[k,])
}
names(printh) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f") # needs change for noG!=2
save("printh", file=paste("../../data/free-eq_", pop, "_", j, "_", seed, ".data", sep=""))
