# save all relevant strucutres


save("mutcomp", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_mutcomp.data", sep=""))

save("nofit", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_nomutfit.data", sep=""))

save("corrfit", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_corrmutfit.data", sep=""))

save("mutno", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_mutno.data", sep=""))

save("propresT", file=paste("../data/", ins,"-", pn, "-", pop, "-", seed, "-", batch.no, "_propresT.data", sep=""))


