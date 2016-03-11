args <- commandArgs(trailingOnly=T)

pop <- as.character(args[1])
seed <- as.numeric(args[2])
theta.batch <- as.numeric(args[3])
k.max <- as.numeric(args[4])
pn <- as.numeric(args[5])
ins <- as.numeric(args[6])


source(paste("pn_", pn, "/s_d-init-costs.R", sep=""))

allcost <- array(data=NA,dim=c(k.max, length(vec_cost_b), length(vec_cost_v),3,3))


for (batch.no in 0:((k.max/theta.batch)-1)){
  load(paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_costcomp.data", sep=""))
  allcost[(1+batch.no*theta.batch):(batch.no*(theta.batch)+(theta.batch)),,,,] <- costcomp
}

save("allcost", file=paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allcost.data", sep=""))
