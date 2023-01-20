library(ggplot2)
library(gridExtra)
library(car)
library(SensoMineR)
library(FactoMineR)
library(factoextra)

setwd("......")

senso<-read.csv2("data_senso.csv",dec=".",stringsAsFactors = TRUE)
summary(senso)
p=ncol(senso)-2 # nb de descripteurs sensoriels
nprod=nlevels(senso$Sample)
njug=nlevels(senso$Judge)

#########################################################
# analyses univariees
# description des distributions des notes par produit
# rq : ici dans le fichier de donnees, 
#      les descripteurs sensoriels sont places apres les deux facteurs 
listplt=list()
for (k in 1:p) {
  dta=data.frame(Sample=senso$Sample,attrib=senso[,k+2])
  listplt[[k]]=ggplot(dta,aes(x=Sample, y=attrib, fill=Sample))+
    ylab(colnames(senso)[k+2])+
    ylim(limits=c(0,100))+
    theme(legend.position="none")+
    geom_boxplot() 
}
X11()
grid.arrange(grobs=listplt, ncol=4, nrow = 4)


# identification des descripteurs discriminants
options(contrasts = c("contr.sum","contr.sum"))
resanova=matrix(NA,nrow=p,ncol=4)
rownames(resanova)= colnames(senso)[-(1:2)]
colnames(resanova)=c("Sample Fvalue","Sample p.value","Judge Fvalue","Judge p.value")
for (k in 1:p) {
  dta=data.frame(Sample=senso$Sample,Judge=senso$Judge, descript=senso[,k+2])
  mod=lm(descript~Sample+Judge,dta)
  tabaov=Anova(mod,type="III")
  resanova[k,c(1,3)]=tabaov$`F value`[2:3]
  resanova[k,c(2,4)]=tabaov$`Pr(>F)`[2:3]
}
round(resanova,4)


# ########################################################
# analyse multivariee
# des donnees individuelles
# ########################################################

# --------------------------------------------------------------
# Calculs des moyennes par juge 
# et des moyennes par produit
# pour chacun des descripteurs
# --------------------------------------------------------------
? aggregate
# en considérant le facteur produit
tabmoy.prod=aggregate(senso[,-(1:2)],by=list(senso$Sample),FUN="mean")
View(tabmoy.prod)
rownames(tabmoy.prod)=tabmoy.prod[,1]
tabmoy.prod=tabmoy.prod[,-1]
round(tabmoy.prod,2)
# en considérant le facteur juge
tabmoy.jug=aggregate(senso[,-(1:2)],by=list(senso$Judge),FUN="mean")
View(tabmoy.jug)
rownames(tabmoy.jug)=tabmoy.jug[,1]
tabmoy.jug=tabmoy.jug[,-1]
round(tabmoy.jug,2)


# ---------------------------------------------------------------------------
# donnees individuelles corrigees des differences de niveau moyen de notation
# et synthese (tableau consensus)
# ---------------------------------------------------------------------------
? scalebypanelist   
tab=scalebypanelist(senso, center = TRUE, scale = FALSE, 
                    col.p=2, col.j=1, firstvar=3)
View(tab)
rownames(tab)=paste(tab[,1],"-",tab[,2],sep="")
tab=tab[,-(1:2)]

senso_consensus=tab[1:nprod,]
round(senso_consensus,2)

# --------------------------------------------------------------------------
# ACP non normee des profils sensoriels au niveau du jury (tableau consensus)
# ---------------------------------------------------------------------------
respca=PCA(senso_consensus,scale.unit=FALSE,ncp = Inf, graph = FALSE)

fviz_screeplot(respca,addlabels=TRUE)
choix.axes=c(1,2)

pvar=fviz_pca_var(respca,axes = choix.axes,repel = TRUE,labelsize =5,
                  arrowsize =1,select.var = list(cos2 = 0))+
  theme_bw()+theme(axis.title = element_text(face="bold",size=10))+coord_fixed()
X11(); print(pvar)

pind=fviz_pca_ind(respca,axes = choix.axes,repel=TRUE,pointsize =1,labelsize = 4)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+theme(axis.title = element_text(face="bold",size=10))+coord_fixed()
X11();print(pind)
# --------------------------------------------------------------

# --------------------------------------------------------------
# ACP non normee des tableaux individuels concatenes,
# avec le facteur produit en variable supplementaire
# --------------------------------------------------------------
dta=cbind(senso$Sample,tab[-(1:9),])
respca2=PCA(dta,scale.unit=FALSE,ncp = Inf,graph = FALSE,quali.sup=1)

fviz_screeplot(respca2,addlabels=TRUE)
choix.axes=c(1,2)

pindpca2=fviz_pca_ind(respca2,axes = choix.axes, geom="text",repel=TRUE,
                      pointsize =1,labelsize = 3, habillage=1,
                      addEllipses = TRUE,ellipse.alpha = 0)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+theme(axis.title = element_text(face="bold",size=10))+coord_fixed()
X11();print(pindpca2)
----------------------------------------------------------------------

# --------------------------------------------------------------------------------
# sauvegarde du tableau des profils sensoriels au niveau du jury (tableau consensus)
# ----------------------------------------------------------------------------------
 write.csv(senso_consensus,"data_senso_moy.csv") # uses "." for the decimal point and a comma for the separator
