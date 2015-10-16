args = commandArgs(trailingOnly=T)
pop <- as.character(args[1])
n <- as.numeric(args[2])
seed <- as.numeric(args[3])


output <- matrix(data=NA, nrow=n, ncol=11)


for (j in 0:((n/1000)-1)){
  load(paste("output/free-eq_", pop, "_", j, "_", seed, ".data", sep=""))
  output[(1+j*(10^3)):(j*(10^3)+(10^3)),] <- printh
}

save("output", file=paste("output/output_", pop, "-", seed, ".data", sep=""))

