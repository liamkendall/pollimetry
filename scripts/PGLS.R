#PGLS using taxonomy

#libraries
library(nlme)
library(MuMIn)
library(arm)
library(ape)
require(phytools)

##Need to decide how far to go with this for IT/WGT and foraging distance


bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
str(bee.phy[[1]])
bee.phy_1=bee.phy[[1]]
bee.phy_1=as.phylo(bee.phy_1)

bee_pruned=drop.tip(bee.phy_1, setdiff(bee.phy_1$tip.label, bee_phylo$Genus))
table(bee_phylo$Species)
bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)

##In Sydenham et al 2018
#We manually placed the remaining 317 species at the nodes of their respective genera,
#so their positions in the phylogeny were resolved to the genus level, without branch 
#length information at the intra-genus level.

##Method 1



##########
## Bees ##
##########

##Own data frame

#build tree from taxonomy
  #if error message, remove row 454 if haven't already - no subfamily/tribe/genus

#first compute correlation matrix from tree
Bee_phy_vcov<-corPagel (0.5,bee_pruned,fixed=FALSE)

#GLS without phylogeny
bee_gls1<- gls(log(Spec.wgt)~log(IT), data=bee_phylo, method="ML")
summary(bee_gls1)

#PGLS with tree and random branch lengths
bee_pgls1 = gls(log(Spec.wgt)~log(IT), data=bee_phylo, correlation=Bee_phy_vcov, method="ML")
summary(bee_pgls1)

#check for heteroscedasticity
plot(pgls_BeeIT1, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",
     abline=c(0,0))
#check for departures from normal distribution of residuals
qqnorm(resid(pgls_BeeIT1, type="n"), col="blue")
qqline(resid(pgls_BeeIT1, type="n"), col="blue")

AIC(bee_gls1,bee_pgls1)


gls(log(Spec.wgt)~0+Climate + Family + Latitude + log(IT) + 
  Sex + Climate:log(IT) + Latitude:log(IT), data=bee_phylo, correlation=Bee_phy_vcov, method="ML")

plot(log(Spec.wgt)~log(IT),data=BEE)
abline(gls_BeeIT1)
abline(pgls_BeeIT1)



#PGLS with tree and random branch lengths
Wgt_pgls1 = gls(Spec.wgt~1, data=bee_phylo, correlation=Bee_phy_vcov, method="ML")
summary(Wgt_pgls1)

IT_pgls1 = gls(IT~1, data=bee_phylo, correlation=Bee_phy_vcov, method="ML")
summary(IT_pgls1)

BL_pgls1 = gls(BL~1, data=bee_phylo, correlation=Bee_phy_vcov, method="ML")
summary(BL_pgls1)

##Much more to be done...

