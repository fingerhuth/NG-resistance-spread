# main script combining 12a (callings 12b), 12c (calling 12d), 12e (calling 12f).

# send simulation batches to cluster nodes

args = commandArgs(trailingOnly=T)

## PASSNUMBER ##
ins <- as.numeric(args[1])

source(paste("instructions/i_", ins, ".R", sep=""))

for(pn in pnv){
  for (popno in c(1,2)){ # population loop
    if(popno == 1){
      seed <- seed.msm
      pop <- "msm"
    }else{
      seed <- seed.het
      pop <- "het" 
    }

    for (batch.no in 0:floor((k.max-1)/theta.batch)){ # parallel loop
      arguments <- paste(pop, seed, theta.batch, batch.no, pn, ins, sep=" ")
#       system(paste("bsub -J run.sims-", pop, "-", pn, " Rscript s_run-model.R ", arguments, sep=""))
    system(paste("Rscript s_run-model.R ", arguments, sep=""))
    } 

  }  # end popno loop
} # end pn loop