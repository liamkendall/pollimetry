bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
str(bee.phy[[1]])
bee.phy=bee.phy[[1]]

bee.phy=as.phylo(bee.phy)
bee.phy=force.ultrametric(bee.phy)
bee.phy$node.label
str(bee_pruned)
str(bee_phylo)

bee_pruned=drop.tip(bee.phy, setdiff(bee.phy$tip.label, bee_phylo$Genus))
bee_pruned$node.label=c(1:225)


bee_pruned$edge.length[bee_pruned$edge.length == 0] <- 0.0000000001
bee_pruned <- ifelse(bee_pruned<0.1,NA,mat)
plotTree(bee_pruned)
edgelabels(bee_pruned$edge.length)

bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)


plotTree(bee_pruned)
edgelabels(bee_pruned$edge.length)

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
??newick

ex
bee_pruned=as.phylo(bee_pruned)

bee_pruned$node.label
bee_phylo$Genus
###MCMCGLMM
unique
MCMCglmm

unique(bee_pruned$tip.label)
plot(bee_all$IT~bee_all$Spec.wgt)
any(duplicated(bee_pruned$tip.label))

is.ultrametric(bee_pruned)



library(phangorn)
bee_pruned$node.label=c(1:40)
bee_pruned=force.ultrametric(bee_pruned)
bee_pruned$Nnode
plot(bee_pruned)
locator()

inverseA(pedigree=bee_pruned,nodes="TIPS")

pedigree=bee_pruned, nodes="TIPS"
head(bee_all)
MCMC1=MCMCglmm(log(Spec.wgt)~log(IT),random=~Measurement+Species,family="gaussian", mev=NULL,
         data=bee_all,start=NULL, prior=NULL, tune=NULL,
         scale=TRUE, nitt=50000, thin=10, burnin=10000, pr=FALSE,
         pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
         saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)

MCMC2=MCMCglmm(log(Spec.wgt)~0+log(IT)+Region,random=~Measurement+Species,family="gaussian", mev=NULL,
               data=bee_all,start=NULL, prior=NULL, tune=NULL,
               scale=TRUE, nitt=50000, thin=10, burnin=10000, pr=FALSE,
               pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
               saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)


MCMC3=MCMCglmm(log(Spec.wgt)~log(IT)+Cl_simp,random=~Measurement+Species,family="gaussian", mev=NULL,
               data=bee_all,start=NULL, prior=NULL, tune=NULL,
               scale=TRUE, nitt=50000, thin=10, burnin=10000, pr=FALSE,
               pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
               saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)

DIC(MCMC1,MCMC2,MCMC3)

#df      DIC
#MCMC1  5 1666.686
#MCMC2  9 1670.570
#MCMC3 21 1663.671
#

###Interesting how


summary(MCMC3)
