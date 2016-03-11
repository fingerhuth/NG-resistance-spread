cost_b <- vec_cost_b[l]
cost_v <- vec_cost_v[m]

parms  <- c(c=c, noG=noG, alpha=alpha, v=v, gamma=gamma, rho=rho, 
            ta=ta, 
            e0_a=e0_a, 
            ea_a=ea_a, 
            mu_0a=mu_0a, 
            mu_a0=mu_a0,
            cost_b=cost_b,
            cost_v=cost_v
)

init <- unlist(free.out[1])