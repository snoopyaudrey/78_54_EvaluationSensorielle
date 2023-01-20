
tegument=read.csv2("ex_Friedman_tegument.csv",row.names = 1)

? friedman.test
friedman.test(y=as.matrix(tegument))
 
library(PMCMRplus)  # PMCMR: Pairwise Multiple Comparisons of Mean Rank
frdAllPairsNemenyiTest(y=as.matrix(tegument))
frdAllPairsConoverTest(y=as.matrix(tegument))

