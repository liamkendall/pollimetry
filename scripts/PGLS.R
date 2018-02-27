#PGLS using taxonomy

#libraries
library(nlme)
library(MuMIn)
library(arm)
library(ape)
require(phytools)

##Need to decide how far to go with this for IT/WGT and foraging distance



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

bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
str(bee.phy[[1]])
bee.phy_1=bee.phy[[1]]
bee.phy_1=as.phylo(bee.phy_1)

bee.phy_1=bind.tip(bee.phy_1,where=which(bee.phy_1$tip.label=="Panurgus"),tip.label="Flavipanurgus")
bee_pruned=drop.tip(bee.phy_1, setdiff(bee.phy_1$tip.label, bee_phylo$Genus))
plot(bee_pruned)
bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)

bee_pruned$tip.label

str(bee_pruned)


plot(bee_pruned)
data.frame(c(sort(bee_pruned$tip.label),sort(bee_phylo$Species)))
#first compute correlation matrix from tree
Bee_phy_vcov<-corPagel(0.5,bee_pruned,fixed=FALSE)

#GLS without phylogeny
bee_gls1<- gls(log(Spec.wgt)~log(IT), data=bee_phylo[-47,], method="ML")
summary(bee_gls1)

#PGLS with tree and random branch lengths
bee_pgls1 = gls(log(Spec.wgt)~log(IT), data=bee_phylo[-47,], correlation=Bee_phy_vcov, method="ML")
summary(bee_pgls1)

bee_all$pgls_pred=exp(predict(bee_pgls1,newdata=bee_all))
#check for heteroscedasticity
plot(bee_pgls1, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",
     abline=c(0,0))
#check for departures from normal distribution of residuals
qqnorm(resid(bee_gls1, type="n"), col="blue")
qqline(resid(bee_gls1, type="n"), col="blue")

AIC(bee_gls1,bee_pgls1)


str(bee_phylo)
bee_phylo[,c("Family")]=as.factor(bee_phylo[,c("Family")])

climate_pgls=gls(log(Spec.wgt)~0+Cl_simp*log(IT), data=bee_phylo[-47,], 
                 correlation=Bee_phy_vcov, method="ML")

summary(climate_pgls)

plot(log(Spec.wgt)~log(IT),data=BEE)
abline(gls_BeeIT1)
abline(pgls_BeeIT1)


bee_pgls2 = gls(log(Spec.wgt)~as.factor(Genus), data=bee_phylo[-47,], correlation=Bee_phy_vcov, method="ML")


