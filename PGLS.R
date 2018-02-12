#PGLS using taxonomy

#libraries
library(nlme)
library(MuMIn)
library(arm)
library(ape)

##Method 1

##########
## Bees ##
##########

##Own data frame
BEE.PGLS=BEE
rownames(BEE.PGLS)=BEE$Species
#build tree from taxonomy - if error message, remove row 454 if haven't already - no subfamily/tribe/genus

BEEtree=as.phylo(~Family/Subfamily/Tribe/Genus/Species, data=BEE)
plot(BEEtree)
#gives the tree random branch lengths (will try a bunch to see how it impacts the results)
BEEtree.rand <- compute.brlen(BEEtree,method="Grafen")

summary(BEEtree.rand)
#first compute correlation matrix from tree
BEEcormatrix<-corPagel (1,BEEtree.rand,fixed=FALSE)

#PGLS with tree and random branch lengths
pgls_BeeIT1<- gls(log(Spec.wgt)~log(IT), data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT1)
pgls_BeeIT2<- gls(log(Spec.wgt)~log(IT)+log(BL), data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT2)
pgls_BeeIT3<- gls(log(Spec.wgt)~log(IT)+Region, data=BEE, correlation=BEEcormatrix, method="ML")
summary(pgls_BeeIT3)

##PHYLOLM

phylolm_BIT1=phylolm(log(Spec.wgt)~log(IT), BEE, BEEtree.rand)
summary(phylolm_BIT1)
