# WORKFLOW
# 1a) Sample from priors, theta
# 1b) Run Simulations, output{1-9}
# 2) Merge output, output
# 3) Truncate Output, determine truncated parameter priors = posterior, outros

args = commandArgs(trailingOnly=T)
pop <- as.character(args[1]) # population to be simulated (MSM or MSW)
n <- as.numeric(args[2]) # number of initial parameter sets to be drawn
cond <- as.character(args[3]) # conditions to be applied (MSM or MSW)
# resyn <- as.character(args[4])

seed <- round(runif(1)*10^6)
# seed <- 266047
options(scipen=100)

# 1a) Sample from priors (estimated from literature) & Run Simulations
system(paste("bsub -J sample-",pop," Rscript s_para_draw.R ", pop, " ",n, " ", seed, sep=""))

# 1) Send Simulations
nor <- n/(10^3)
### Run model and get prevalences
for (i in 0:(nor-1)){
  system(paste("bsub -J run.sims-", pop, " -w \"done(sample-",pop,")\" Rscript s_para_run-free-eq.R ", i, " ", pop, " ", seed, sep=""))
}
# simulations are send in bunches of 1000; jobs have title "run.sims-POP" (where POP is the msm/het)

# 2) Merge output
# sep <- seq(500,nor, 500)
# from <- sep-500
# to <- sep-1
# for (i in 1:length(from)){
#   system(paste("bsub -J merge.temp-", pop, " -w \"done(run.sims-",pop,")\" Rscript s_para_merge-to-temp.R ", pop, " ", seed, " ", from[i], " ", to[i], sep=""))
# }
# system(paste("bsub -J merge-all-",pop, " -w \"done(merge.temp-",pop,")\" Rscript s_para_merge-to-all.R ", pop, " ", n, " ", seed, sep=""))

# 2) Merge output
system(paste("bsub -J merge-", pop, " -w \"done(run.sims-",pop,")\" Rscript s_para_merge.R ", pop, " ", n, " ", seed, " ", sep=""))


# 3) Truncate output
# system(paste("bsub -J condition-",pop," -w \"done(merge-all-",pop,")\" Rscript s_para_condition.R ",pop, " ", seed, " ", cond, sep=""))
system(paste("bsub -J condition-",pop," -w \"done(merge-",pop,")\" Rscript s_para_condition.R ",pop, " ", seed, " ", cond, sep=""))

# 4) create .pdf with control plots
system(paste("bsub -J plot-",pop," -w \"done(condition-",pop,")\" Rscript s_plot_para.R ",pop, " ", seed, " ", cond, sep=""))

# # 4) run resistance runs if asked for
# if(resyn == "yes"){
#   system(paste("bsub -J res.run-",pop," -w \"done(condition-",pop,")\" Rscript s_res_main.R ",pop, " ", seed, " ", cond, sep=""))
# }