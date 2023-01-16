library(ggplot2)

# ###############################################################
# distribution de la loi binomiale de parametre n et p0
n=36
p0=1/3
dat <- data.frame(valeurs = rbinom(10000,n,p0))
ggplot(dat, aes(x=valeurs)) + geom_histogram(binwidth=0.5)

# quantile pour une loi binomiale
? qbinom
# pour determiner q tel que Prob(X <= q /B(n,p0))=1-alpha
alpha=0.05
q=qbinom(1-alpha,n,p0) 
q
#   q     est la plus grande valeur dans la zone d'acceptation de H0
# (q+1)=x est la plus petite valeur dans la zone de rejet de H0
# pour un risque d'erreur alpha, 5% par exemple

# A titre complémentaire, on peut calculer Prob(X <= q /B(n,p0))
# pour q
pbinom(q,n,p0)
1-pbinom(q,n,p0)   # < alpha


# distribution de la loi binomiale de parametre n et p0
# et distribution de la loi binomiale de parametre n et p1,
# p1 etant associe a un certain niveau Pd de discrimination 
# au-dela du niveau de discrimination par chance (i.e. p0)
Pd=0.375
p1=p0+ Pd*(1-p0)
p1
dat <- data.frame(cond=factor(rep(c("p0","p1"),each=10000)),
                  val = c(rbinom(10000,n,p0),rbinom(10000,n,p1)))
ggplot(dat, aes(x=val, fill=cond)) +
  geom_histogram(binwidth=.5, position="dodge")+
  geom_vline(xintercept = q+0.5)

# calcul du risque beta
beta=pbinom(q,n,p1)
beta


# test binomial {stats}
? binom.test
nb.good=24   # nombre de reponses correctes
binom.test(x=nb.good,n, p0, alternative="greater")
# Cette fonction peut etre utilisee pour des tests discriminatifs
# pour lesquels la probabilite de reponse correctement au hasard est
# differente (par ex 1/2), ou des tests bilateraux
