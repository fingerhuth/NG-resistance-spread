sample.theta <- function(n, seeds){
  
  ### sample epsilon
  set.seed(seeds[1])
  epsilon <- runif(n, 0, 1)
  
  ### sample betaL
  set.seed(seeds[2])
  betaL <- runif(n,0,1)
  
  ### sample betaH
  set.seed(seeds[3])
  betaH <- runif(n, 0, betaL)
  
  ### sample D
  set.seed(seeds[4])
  # gamma distribution: shape k, scale theta, rate beta = 1/theta, mean = k/beta = k*theta => mean/k = theta
  D <- rgamma(n, shape=2, scale=3/2)/12 # we assume mean duration of 3 months
  
  ### sample f
  set.seed(seeds[5])
  f <- runif(n, 0, 1)
  
  theta <- cbind(epsilon, betaL, betaH, D, f)

}
