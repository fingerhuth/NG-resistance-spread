args <- commandArgs(trailingOnly=T)

pop <- as.character(args[1])
seed <- as.numeric(args[2])
theta.batch <- as.numeric(args[3])
k.max <- as.numeric(args[4])
pn <- as.numeric(args[5])
ins <- as.numeric(args[6])


source(paste("pn_", pn, "/s_d-init-mutation.R", sep=""))

yearsRes <- 100
timesRes <- seq(0,yearsRes,1/12)

allmut <- array(data=NA,dim=c(k.max, length(vec_mu_ta), 3, 3)) # mutation comparison
allmutno <- array(data=NA,dim=c(k.max, length(vec_mu_ta), length(timesRes))) # number of mutations
allpropresT <- array(data=NA,dim=c(k.max, length(vec_mu_ta), length(timesRes))) # timeline, proportion resistant in total population


for (batch.no in 0:((k.max/theta.batch)-1)){
  load(paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_mutcomp.data", sep=""))
  load(paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_mutno.data", sep=""))
  load(paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_propresT.data", sep=""))
  allmut[(1+batch.no*theta.batch):(batch.no*(theta.batch)+(theta.batch)),,,] <- mutcomp
  allmutno[(1+batch.no*theta.batch):(batch.no*(theta.batch)+(theta.batch)),,] <- mutno
  allpropresT[(1+batch.no*theta.batch):(batch.no*(theta.batch)+(theta.batch)),,] <- propresT
}

save("allmut", file=paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmut.data", sep=""))
save("allmutno", file=paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allmutno.data", sep=""))
save("allpropresT", file=paste("../data/", ins, "-", pn, "-", pop, "-", seed, "_allpropresT.data", sep=""))
