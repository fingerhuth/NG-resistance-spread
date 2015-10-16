args = commandArgs(trailingOnly=T)

pop <- as.character(args[1])
seed <- as.numeric(args[2])
cond <- as.character(args[3])
load(paste("../../data/output_", pop, "-", seed, ".data", sep=""))
load(paste("../../data/ros_", pop, "-", seed, "-", cond, ".data", sep=""))
output[,4]<-output[,4]*output[,11]
output[,5]<-output[,5]*output[,11]
output[,6]<-output[,6]*output[,11]

names <- names(output)
names[1:ncol(output)] <- c( "Prevalence Low", #1
                            "Prevalence High", #2
                            "Prevalence Total", #3
                            "Incidence Low", #4
                            "Incidence High", #5
                            "Incidence Total", #6
                            "Assortativity Coefficient", #7
                           "Transmission Probability Within Low Activity Group", #8
                           "Transmission Probability Within High Acitivty Group", #9
                           "Duration of Infection (months)", #10
                           "Frequency of Symptomatic Infection" #11
                           )
pdf(file=paste("../../figures/fig_", pop, "-", seed, "-", cond, ".pdf", sep=""), width=21, height=35)
split.screen(figs=c(8,3))
for(j in 1:6){
  screen(j, new=T)
  # histogram of posterior (columns 1-6)
  hist(output[ros,j], freq=F, col=rgb(100,180,0,100,maxColorValue=255), xlab=names[j], main="",
       add=F)
  close.screen(j)
}
for(j in 7:9){
  screen(j, new=T)
  # histogram of posterior
  hist(output[ros,(j-3)]*output[ros,10]/12, freq=F, col=rgb(100,180,0,100,maxColorValue=255), xlab=paste(names[j-3], "*D/12", sep=""), main="",
       add=F)
  close.screen(j)
}
for(j in 10:12){
  screen(j, new=T)
  # histogram of posterior (columns 1-6)
  hist(output[ros,(j-9)]*output[ros,11], freq=F, col=rgb(100,180,0,100,maxColorValue=255), xlab=paste(names[j-9],"*f", sep=""), main="",
       add=F)
  close.screen(j)
}
for(j in 1:11){
  screen(n=j+12, new=T)
  dens.prior <- density(output[,j], na.rm=T)
  dens.post <- hist(output[ros,j], plot=F)$density
  dens.post.breaks <- hist(output[ros,j], plot=F)$breaks
  
  # histogram of posterior
  hist(output[ros,j], freq=F, col=rgb(100,180,0,100,maxColorValue=255), xlab=names[j], main="",
    add=F, ylim=c(0,max(dens.post, dens.prior$y)), 
    xlim=c(min(dens.post.breaks, dens.prior$x), max(dens.post.breaks, dens.prior$x)))
  
  if (j == 7 | j == 8 | j == 11 ){
    # epsilon (7), betaL (8), f (11) have uniform prior
    polygon(c(0,1,1,0), c(0,0,1,1), col=rgb(0,110,255,100,maxColorValue=255), border=NA)
  }else{
    # betaH (9), D (10); cut off values below zero (artefact from density kernel estimate)
    xx <- dens.prior$x[which(dens.prior$x>=0)]
    yy <- dens.prior$y[which(dens.prior$x>=0)]
    polygon(c(xx, 1, 0), c(yy, 0, 0), col=rgb(0,110,255,100,maxColorValue=255), border=NA)
  }
  close.screen(n=j+12)
}
close.screen(all.screens=T)
dev.off()

