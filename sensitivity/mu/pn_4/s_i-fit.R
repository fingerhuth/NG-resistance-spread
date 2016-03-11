# fit proportion resitant to logistic growth model

# get first equilibrium timepoint after switch
times1 <- head(timesRes,1)
times2 <- tail(timesRes,1)

eq.temp <- matrix(nrow=nrow(res.out)-1, ncol=ncol(res.out)-1)
swtime <- tail(times1, 1) # switch time
edtime <- tail(times2, 1) # end time
for(i in 2:nrow(res.out)){
  eq.temp[i-1,] <- abs(res.out[i,2:ncol(res.out)]-res.out[i-1,2:ncol(res.out)])<10^-15
}
eq.row <- which(abs(rowSums(eq.temp)-ncol(eq.temp))<10^-15)-1 # gives all rows in equilibrium (rows give row number in res.out)
eq.sw <- eq.row[which((eq.row-which(abs(res.out[,1]-swtime)<10^-15))>0)] # gives all equilibria indices after switch

sw <- which(abs(res.out[,1]-swtime)<10^-6)
eq <- if(is.na(eq.sw[1])){length(timesRes)}else{eq.sw[1]}


# prepare timeline
for (n in 1:3){
  if(n==1){
    propsen <- abs(res.out[sw:eq-sw,4])/rowSums(abs(res.out[sw:eq-sw,c(4,6)])) # proportion non-resistant LOW
  }else if(n==2){
    propsen <- abs(res.out[sw:eq-sw,5])/rowSums(abs(res.out[sw:eq-sw,c(5,7)])) # proportion non-resistant HIGH
  }else if(n==3){
    propsen <- abs(rowSums(res.out[sw:eq-sw,c(4,5)]))/rowSums(abs(res.out[sw:eq-sw,4:7])) # proportion non-resistant TOTAL
  }
  
  jit<- jitter(propsen)
  jit[which(jit>1)]<-propsen[which(jit>1)]
  jit[which(jit<0)]<-propsen[which(jit<0)]
  y <- 1-jit
  x <- timesRes[sw:eq-sw]
  df <- data.frame(x=x[which(y!=0)], y=y[which(y!=0)])
  
  # try fit
  triedfit <- try ( log.ss <- nls(y ~ SSlogis(x,phi1,phi2,phi3), data=df) )
  
  if("try-error" %in% class(triedfit)){
    mutcomp[mci,l,n,1] <- NA
    mutcomp[mci,l,n,2] <- NA
    mutcomp[mci,l,n,3] <- NA
    nofit <- c(nofit, paste(k,l,n, sep=" "))
  }else if(summary(log.ss)$coef[1]<0.99){
    mutcomp[mci,l,n,1] <- NA
    mutcomp[mci,l,n,2] <- NA
    mutcomp[mci,l,n,3] <- NA
    corrfit <- c(nofit, paste(k,l,n, sep=" "))
  }else{
    mutcomp[mci,l,n,1] <- 1/summary(log.ss)$coef[3] # b
    mutcomp[mci,l,n,2] <- summary(log.ss)$coef[1] # c = asymptote
    mutcomp[mci,l,n,3] <- exp(summary(log.ss)$coef[2]/summary(log.ss)$coef[3]) # a
  }
}