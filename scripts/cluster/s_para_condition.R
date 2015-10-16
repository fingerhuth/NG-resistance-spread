# select those parameter sets that lie within relevant prevalence and incidence ranges

# intialize variables with arguments from terminal call
args = commandArgs(trailingOnly=T)
pop <- as.character(args[1])
seed <- as.numeric(args[2])
cond <- as.character(args[3])

# define prevalence and incidence ranges
# if condition is 'msm', use prevalences and incidence ranges relevant for men who have sex with men
if(cond=="msm"){ 
  # conditions on prevlance: L: low activity class, H: high activity class, T: both activity classes
  prev_L <- c(0,0.0279) # at most 2.79 %
  prev_H <- c(0.0119,1) # at least 1.19 %
  prev_T <- c(0.0119,0.0279) # 1.19-2.79 % (urethral or oral or rectal infection)
  
  # condition on incidence
  inc_T <- c(0.0588,0.0719) # 5.88-7.19 % (urethral or oral or rectal), incidence of INTERVAL diagnoses
  
}else if(cond=="het"){ # if condition is 'het', use prevalences and incidence ranges relevant for heterosexual men and women
  # condition on prevalence: L: low activity class, H: high activity class, T: both activity classes
  prev_L <- c(0,0.0038) #  at most 0.38 %
  prev_H <- c(0.0016,1) # at least 0.16 % 
  prev_T <- c(0.0016,0.0038) # 0.16-0.38 %
  
  # condition on incidence
  inc_T <- c(0.00120,0.00360) # 120-360 per 100 000 persons per year (mean 240, based on CDC figure)
}

load(paste("output/output_", pop, "-", seed, ".data", sep=""))


# select only those parameter sets that lie within defined ranges
ind <- which(abs(output[,1])-10^(-10)>=prev_L[1]
             & abs(output[,1])-10^(-10)<=prev_L[2]
             & abs(output[,2])-10^(-10)>=prev_H[1]
             & abs(output[,2])-10^(-10)<=prev_H[2]
             & abs(output[,3])-10^(-10)>=prev_T[1]
             & abs(output[,3])-10^(-10)<=prev_T[2]
             
             & abs(output[,6]*output[,11])-10^(-10)>=inc_T[1] 
             & abs(output[,6]*output[,11])-10^(-10)<=inc_T[2]
)

outind <- output[ind,] # this line has no further implication.

ros <- ind
save(ros, file=paste("output/ros_",pop, "-", seed, "-", cond, ".data", sep=""))
outros <- output[ros,]
save(outros, file=paste("output/outros_", pop, "-", seed, "-", cond, ".data", sep=""))
