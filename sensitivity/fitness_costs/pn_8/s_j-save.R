# save all relevant strucutres


save("costcomp", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_costcomp.data", sep=""))

save("nofit", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_nofit.data", sep=""))

save("corrfit", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_corrfit.data", sep=""))

