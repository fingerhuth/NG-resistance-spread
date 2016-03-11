mutno[mci,l,] <- res.out[,12]

propresT[mci,l,] <- rowSums(res.out[,6:7])/rowSums(res.out[,4:7]) # proportion resistant in total pop: (RES_L+RES_H)/(RES_L+RES_H+WT_L+WT_H)
