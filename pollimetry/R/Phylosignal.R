<<<<<<< HEAD
##PHYLOSIGNAL - NOT WORKING CURRENTLY
=======
##PHYLOSIGNAL
>>>>>>> 66509a52f80b9d6b2e66bc2fdac516d5c7318e36

#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

library(phytools)
library(nlme)
library(phylosignal)
library(phylobase)

bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
str(bee.phy[[1]])
bee.phy=bee.phy[[1]]
bee.phy=as.phylo(bee.phy)

str(bee_pruned)
str(bee_phylo)
bee_phylo[82,]

bee_pruned=drop.tip(bee.phy, setdiff(bee.phy$tip.label, bee_phylo$Genus))
plot(bee_pruned)
bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Subgenus)
bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)
plot(bee_pruned)
write.csv(cbind(bee_pruned$tip.label,as.character(bee_phylo$Species[1:237])),"tips.csv")
bee_pruned=as.phylo(bee_pruned)

#####   

bee_pruned
bee_phylo
str(bee_phylo)

#####

bee_phy4d=phylo4d(x = bee_pruned,tip.data=bee_phy_traits,tip.label=bee_pruned$tip.label,order="postorder")

barplot.phylo4d(bee_phy4d)
phyloSignal(bee_phy4d)

IT.cg = phyloCorrelogram(bee_phy4d, trait = "IT")
WGT.cg =phyloCorrelogram(bee_phy4d, trait = "Spec.wgt")
par(mfrow=c(1,2))
plot(IT.cg)
plot(WGT.cg)


local.i = lipaMoran(bee_phy4d, trait = "Spec.wgt",
                     prox.phylo = "nNodes", as.p4d = TRUE)
points.col= lipaMoran(bee_phy4d, trait = "IT", prox.phylo = "nNodes")$p.value
points.col = ifelse(points.col < 0.05, "red", "black")
dotplot.phylo4d(local.i, dot.col = points.col)

<<<<<<< HEAD
##PHYLOSIGNAL

#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

library(phytools)
library(nlme)
library(phylosignal)
library(phylobase)

@#########
#NOT WORKING ATM
rownames(bee_phylo)=bee_phylo$Species
bee_phy_traits=bee_phylo[,c("IT","Spec.wgt")]
bee_phy_traits$ITrel=bee_phylo[,c("IT")]/bee_phylo[,c("Spec.wgt")]

str(bee_pruned)

bee_phy4d=phylo4d(x = bee_pruned,tip.data=bee_phy_traits,tip.label=bee_pruned$tip.label,order="postorder")

barplot.phylo4d(bee_phy4d)
phyloSignal(bee_phy4d)

IT.cg = phyloCorrelogram(bee_phy4d, trait = "IT")
WGT.cg =phyloCorrelogram(bee_phy4d, trait = "Spec.wgt")
par(mfrow=c(1,2))
plot(IT.cg)
plot(WGT.cg)


local.i = lipaMoran(bee_phy4d, trait = "Spec.wgt",
                    prox.phylo = "nNodes", as.p4d = TRUE)
points.col= lipaMoran(bee_phy4d, trait = "IT", prox.phylo = "nNodes")$p.value
points.col = ifelse(points.col < 0.05, "red", "black")
dotplot.phylo4d(local.i, dot.col = points.col)

=======
>>>>>>> 66509a52f80b9d6b2e66bc2fdac516d5c7318e36
