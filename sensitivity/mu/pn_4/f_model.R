no.pop <- 5
pp <- no.pop-1
# ODE model
model <- function(t, x, parms) {
  with(as.list(parms),{
    dx <- numeric()
      # prevalences
      dx[1] <- # susceptible
        ( 
          - x[1]*c[1]*sum(rho[1,]*beta[1,]*c(x[3:4], x[5:6])/N) # new infections
          + sum(v*x[c(3,5)]) # spontaneous clearance
          + x[3]*(ta[1]*e0_a*(1-mu_ta)) # from WT: efficaciously treated AND not mutated during treatment
          + x[5]*(ta[1]*ea_a) # from A resistant: efficaciously treated AND not mutated during treatment (ea_a: partial resistance)
          - alpha*x[1] 
          + sum(alpha*x[c(1,3,5)])
          - gamma*x[1]
          + gamma*N[1]*sum(x[1:2])
        )
    dx[2] <- # susceptible
      ( 
        - x[2]*c[2]*sum(rho[2,]*beta[2,]*c(x[3:4], x[5:6])/N) # new infections
        + sum(v*x[c(4,6)]) # spontaneous clearance
        + x[4]*(ta[2]*e0_a*(1-mu_ta)) # from WT: efficaciously treated AND not mutated during treatment
        + x[6]*(ta[2]*ea_a) # from A resistant: efficaciously treated AND not mutated during treatment (ea_a: partial resistance)
        - alpha*x[2] 
        + sum(alpha*x[c(2,4,6)])
        - gamma*x[2]
        + gamma*N[2]*sum(x[1:2])
      )
      dx[3] <- # infected WT
        (
          x[1]*c[1]*sum(rho[1,]*beta[1,]*x[3:4]/N) 
          - v*x[3]
          - x[3]*(ta[1]*e0_a) # treated leave pop with efficacy e
          - x[3]*mu_0a # treatment-independent mutations leave pop
          + x[5]*mu_a0 # treatment-independent mutations enter pop
          - alpha*x[3]
          - gamma*x[3]
          + gamma*N[1]*sum(x[3:4])
        )
    dx[4] <- # infected WT
      (
        x[2]*c[2]*sum(rho[2,]*beta[2,]*x[3:4]/N) 
        - v*x[4]
        - x[4]*(ta[2]*e0_a) # treated leave pop with efficacy e
        - x[4]*mu_0a # treatment-independent mutations leave pop
        + x[6]*mu_a0 # treatment-independent mutations enter pop
        - alpha*x[4]
        - gamma*x[4]
        + gamma*N[2]*sum(x[3:4])
      )
      dx[5] <- # infected A resistant
        (
          x[1]*c[1]*sum(rho[1,]*beta[1,]*x[5:6]/N) 
          - v*x[5] 
          - x[5]*(ta[1]*ea_a) # treated leave pop with efficacy e (ea_a: partial resistance)
          - x[5]*mu_a0 # treatment-independent mutations leave pop
          + x[3]*mu_0a # treatment-independent mutations enter pop
          + x[3]*mu_ta*ta[1]*e0_a # treated that are treated efficaciously but do mutate enter
          - alpha*x[5]
          - gamma*x[5]
          + gamma*N[1]*sum(x[5:6])
        )
      dx[6] <- # infected A resistant
        (
          x[2]*c[2]*sum(rho[2,]*beta[2,]*x[5:6]/N) 
          - v*x[6] 
          - x[6]*(ta[2]*ea_a) # treated leave pop with efficacy e (ea_a: partial resistance)
          - x[6]*mu_a0 # treatment-independent mutations leave pop
          + x[4]*mu_0a # treatment-independent mutations enter pop
          + x[4]*mu_ta*ta[2]*e0_a # treated that are treated efficaciously but do mutate enter
          - alpha*x[6]
          - gamma*x[6]
          + gamma*N[2]*sum(x[5:6])
        )
    list(dx, 
         # incidence; needs manual adjustment if noG changes:
         c(x[1]*c[1]*sum(rho[1,]*beta[1,]*x[3:4]/N),
           x[2]*c[2]*sum(rho[2,]*beta[2,]*x[3:4]/N),
           x[1]*c[1]*sum(rho[1,]*beta[1,]*x[5:6]/N),
           x[2]*c[2]*sum(rho[2,]*beta[2,]*x[5:6]/N), 
           x[3]*mu_ta*ta[1]*e0_a + x[4]*mu_ta*ta[2]*e0_a # and number of successful mutations per year
               ))
  })
}