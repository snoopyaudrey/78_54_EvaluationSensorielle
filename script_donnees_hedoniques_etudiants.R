library(reshape)
library(multcomp)
library(ggplot2)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(ClustVarLV)


setwd(".....")

hedo<-read.csv2("data_hedo.csv",dec=".",row.names = 1)
p=ncol(hedo) # nb de consommateurs
n=nrow(hedo) # nb de produits
colnames(hedo)=gsub("X", "C", colnames(hedo))
View(hedo)


#########################################################
# apercu des donnees : relation moyenne - ecart-type
moy=apply(hedo,2,mean)
ectype=apply(hedo,2,sd)
plot(moy,ectype, pch="+", xlim=c(1,9),xlab="moyenne",ylab="ecart-type")
axis(1,at=1:9)

     
# moyenne des notes d'appreciation sur l'ensemble du panel
moy.prod=apply(hedo,1,mean)
barplot(moy.prod[order(moy.prod)],ylim=c(1,9),xpd=FALSE,col="chocolate")
axis(2,at=1:9)

# anova sur les scores d'appreciations, 2 facteurs : conso et produit
# depliage le tableau de donnees
hedo_long <- melt(hedo)
hedo_long=data.frame(prod=as.factor(rep(rownames(hedo),n*p)),
                     conso=as.factor(hedo_long$variable),
                     value=hedo_long$value)
View(hedo_long)

options(contrasts = c("contr.sum","contr.sum"))
mod=lm(value~prod+conso,hedo_long)
anova(mod)
tuk=glht(mod,linfct = mcp(prod="Tukey"))
tuk.cld=cld(tuk)
sort(tuk.cld$mcletters$Letters)


# ########################################################
# cartographie interne des pr?f?rences
# ########################################################
# ACP non normee
# consommateur 308 exclu
hedobis=hedo[,-202]
carto=PCA(hedobis,scale.unit=FALSE,ncp = Inf,graph = FALSE)
round(carto$eig,2)
fviz_screeplot(carto,addlabels=TRUE)
choix.axes=c(1,2)
pbiplot=fviz_pca_biplot(carto,axes = choix.axes,repel = TRUE,geom.var = c("arrow"),
            labelsize = 6,pointsize =3)+
            theme_bw()+
            theme(axis.title = element_text(face="bold",size=10))+
            coord_fixed()
X11();print(pbiplot)


# ########################################################
# liens entre l'appreciation hedonique des produits 
# et leurs caracteristiques sensorielles de flaveur.
# ########################################################
# cartographie interne des preferences, avec les descripteurs
# sensoriels en variables supplementaires
senso_moy=read.csv("data_senso_moy.csv",row.names =1)
colnames(senso_moy)
hedo_senso=cbind(hedobis,scale(senso_moy[,8:13]))
dim(hedo_senso)
carto_senso=PCA(hedo_senso,scale.unit=FALSE,quanti.sup=291:296,ncp = Inf,graph = FALSE)
pvar=fviz_pca_var(carto_senso,axes = choix.axes,repel=TRUE,labelsize = 6, arrowsize =1, 
                  label="quanti.sup",invisible="var")+
                  theme_bw()+
                  theme(axis.title = element_text(face="bold",size=10))+
                  coord_fixed()
X11();print(pvar)






# ########################################################
# segmentation des consommateurs
# ########################################################
resclv=CLV(hedobis,method="local",sX=FALSE)
plot(resclv);
plot(resclv,"delta")
summary(resclv,K=2)

resclvkm2=CLV_kmeans(hedobis,method="local",sX=FALSE,clust=2,strategy="kplusone")

table(get_partition(resclv,K=2),get_partition(resclvkm2))

plot_var(resclvkm2)

mG1=apply(hedobis[,which(get_partition(resclvkm2)==1)],1,mean)
mG2=apply(hedobis[,which(get_partition(resclvkm2)==2)],1,mean)
mG0=apply(hedobis[,which(get_partition(resclvkm2)==0)],1,mean)

dat <- data.frame(group=factor(rep(c("G1","G2","G0"),each=n),levels=c("G1","G2","G0")),
                  prod=factor(rep(rownames(hedobis),3)),
                  val = c(mG1,mG2,mG0))
bp=ggplot(dat, aes(x=group, y=val, fill=prod)) +  
  scale_y_continuous(limits=c(0,9), breaks=0:9)+
  geom_bar(stat="identity",color="black",position=position_dodge())+
  scale_fill_brewer(palette="Set1")
X11(); bp

