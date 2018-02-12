#PGLS using taxonomy

#libraries
library(nlme)
library(MuMIn)
library(arm)
library(ape)
library(phylolm)

##Method 1

PredAllo=read.csv("PredAlloPoll10218.csv",header=T)

##Individual data frames
PredAllo.split=split(PredAllo,PredAllo$Taxa)
BEE=as.data.frame(PredAllo.split[[1]])
HOV=PredAllo.split[[2]]

#One specimen without genus
BEE=BEE[-454,]
BEE[454,]



##########
## Bees ##
##########

##Own data frame
BEE.PGLS=BEE
rownames(BEE.PGLS)=BEE$Species
#build tree from taxonomy
  #if error message, remove row 454 if haven't already - no subfamily/tribe/genus

BEEtree=as.phylo(~Family/Subfamily/Tribe/Genus/Species, data=BEE)
plot(BEEtree)
#gives the tree random branch lengths (will try a bunch to see how it impacts the results)
BEEtree.rand <- compute.brlen(BEEtree,method="Grafen")

summary(BEEtree.rand)

#first compute correlation matrix from tree
BEEcormatrix<-corPagel (0.5,BEEtree.rand,fixed=FALSE)

#GLS without phylogeny
gls_BeeIT1<- gls(log(Spec.wgt)~log(IT), data=BEE, method="ML")
summary(gls_BeeIT1)
gls_BeeIT2<- gls(log(Spec.wgt)~log(IT)+factor(Region), data=BEE,method="ML")
summary(gls_BeeIT2)

#PGLS with tree and random branch lengths
pgls_BeeIT1 = gls(log(Spec.wgt)~log(IT), data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT1)
#check for heteroscedasticity
plot(pgls_BeeIT1, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",
     abline=c(0,0))
#check for departures from normal distribution of residuals
qqnorm(resid(pgls_BeeIT1, type="n"), col="blue")
qqline(resid(pgls_BeeIT1, type="n"), col="blue")

pgls_BeeIT2 = gls(log(Spec.wgt)~log(IT)+relevel(Region,ref="Europe"), data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT2)
pgls_BeeIT3 = gls(log(Spec.wgt)~log(IT)+Region+Family, data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT3)
pgls_BeeIT4 = gls(log(Spec.wgt)~log(IT)+Region+Subfamily, data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT4)
pgls_BeeIT5 = gls(log(Spec.wgt)~log(IT)+Region+Genus, data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT5)

##Much more to be done...

