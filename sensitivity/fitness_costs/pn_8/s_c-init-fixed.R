# initialize fixed parameters

noG <- 2
years <- 500
yearsRes <- 100
times <- seq(0,years,1)
timesRes <- seq(0,yearsRes,1/12)

N <- behav[1]
N[2] <- 1-N
c <- behav[2:3]

# efficacy and partial resistance
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
# mu_ta <- 0

# parameters currently fixed
alpha <- 1/(44-16+1)
gamma <- 1