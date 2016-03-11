# instructions for cluster run
# aim: 
## - simulate SM with fitness costs for duration and transmission
## - fit proportion resistant of Low, High, Total population

k.max <- 2000 # should be dividable by theta.batch without rest (else problem with merging - could be easily expanded to allow for this but I currently see not neet to do this)
theta.batch <- 10

pnv <- 4

seed.msm <- 312774
seed.het <- 993734
