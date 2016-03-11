# initialize varying parameters for resistance free equilibrium

epsilon <- theta[k,1]
betaL <- theta[k,2]
betaH <- theta[k,3]
D <- theta[k,4] 
f <- theta[k,5]

# initialize populations
init <- c(N[1]*0.995,N[2]*0.90,N[1]*0.005, N[2]*0.1, 0, 0)

tt <- f/D
ta <- c(tt,tt)

v <- 1/D - tt
rho <- mixing(epsilon, noG, c, N)
beta <- matrix(c(betaL,sqrt(betaL*betaH),sqrt(betaL*betaH),betaH),nrow=noG,ncol=noG)

cost_b <- 0
cost_v <- 0
mu_ta <- 0

parms  <- c(c=c, noG=noG, alpha=alpha, v=v, gamma=gamma, rho=rho, 
            ta=ta, 
            e0_a=e0_a, 
            ea_a=ea_a, 
            mu_0a=mu_0a, 
            mu_a0=mu_a0, 
            cost_b=cost_b,
            cost_v=cost_v
)