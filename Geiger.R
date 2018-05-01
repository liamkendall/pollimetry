##Phylo-signal using geiger 

#fitContinuous - individiual traits PIC
##tree
bee_pruned
##

bee_phylo_WGT=as.data.frame(bee_phylo[,c("Spec.wgt")])
rownames(bee_phylo_WGT)=rownames(bee_phylo)

fitContinuous(bee_pruned, (bee_phylo_WGT),SE=0,
              model = c("lambda"))

bee_phylo_IT=as.data.frame(bee_phylo[,c("IT")])
rownames(bee_phylo_IT)=rownames(bee_phylo)

fitContinuous(bee_pruned, bee_phylo_IT,SE=0,
              model = c("lambda"))

bee_pgls=gls(log(Spec.wgt) ~ log(IT), correlation = corPagel(phy = bee_pruned,value=0),
    data = bee_phylo, method = "ML")

rmse(log(bee_test$Spec.wgt),predict(bee_pgls,bee_test))
name.check(bee_pruned,bee_phylo)


##WITH INTRASPECIFIC VARIATION
bee_intra_phy

##TREE


bee_prun_int=drop.tip(bee.phy, setdiff(bee.phy$tip.label,bee_intra_phy$Genus))
bee_prun_int=genus.to.species.tree(bee_prun_int, species=bee_intra_phy$Species)
options(na.action=na.omit)

Intra_pgls=pgls.SEy(log(Spec.wgt)~log(IT),data=bee_phylo,corClass=corPagel,tree=bee_pruned)
AIC(Intra_pgls)


pgls.Ives(bee_prun_int, x=setNames(XX$x,XX$species), y=setNames(XX$y,XX$species))

