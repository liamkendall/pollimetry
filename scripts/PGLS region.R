PGLS with region

bee_phylo_2=aggregate(Latitude~Region+Family+Subfamily+
                      Tribe+Genus+Species,bee_all,median)
bee_phylo_2$Longitude=as.numeric(unlist(aggregate(Longitude~Region+Family+Subfamily+
                                                  Tribe+Genus+Species,bee_all,median)[7]))
bee_phylo_2$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Region+Family+Subfamily+
                                                 Tribe+Genus+Species,bee_all,mean)[7]))
bee_phylo_2$Wgt.SD=as.numeric(unlist(aggregate(log(Spec.wgt)~Region+Family+Subfamily+
                                               Tribe+Genus+Species,bee_all,sd)[7]))
bee_phylo_2$IT=as.numeric(unlist(aggregate(IT~Region+Family+Subfamily+
                                           Tribe+Genus+Species,bee_all,mean)[7]))
bee_phylo_2$IT.SD=as.numeric(unlist(aggregate(log(IT)~Region+Family+Subfamily+
                                              Tribe+Genus+Species,bee_all,sd)[7]))
duplicated(bee_phylo_2$Species)

##REMOVE DUPLICATES FROM REGIONS - APis from Australia, Halictus rubicundus from NA
bee_phylo_2=bee_phylo_2[-55,]
bee_phylo_2=bee_phylo_2[-120,]
bee_phylo_2=bee_phylo_2[-113,]

rownames(bee_phylo_2)=bee_phylo_2$Species
##PGLS WITH REGION

##TREE
bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
bee.phy=bee.phy[[1]]
bee.phy=as.phylo(bee.phy)
#bee.phy=force.ultrametric(bee.phy) Not sure if this is required
bee_pruned_2=drop.tip(bee.phy, setdiff(bee.phy$tip.label,bee_phylo_2$Genus))
bee_pruned_2=genus.to.species.tree(bee_pruned, species=bee_phylo_2$Species)

#With region
Bee2vcv=corPagel(value=0.5,phy=bee_pruned_2,fixed=FALSE)

Bee_GLS1<- gls(log(Spec.wgt)~log(IT), data=bee_phylo_2,
               method="ML")
summary(Bee_GLS1)
Bee_GLS2<- gls(log(Spec.wgt)~log(IT)*Region, data=bee_phylo_2,
               method="ML")
summary(Bee_GLS2)
Bee_GLS3<- gls(log(Spec.wgt)~log(IT)+Region, data=bee_phylo_2,
               method="ML")
summary(Bee_GLS3)

Bee_PGLS1 = gls(log(Spec.wgt)~log(IT), data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")
summary(Bee_PGLS1)
Bee_PGLS2 = gls(log(Spec.wgt)~log(IT)*Region, data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")
summary(Bee_PGLS2)
Bee_PGLS3 = gls(log(Spec.wgt)~log(IT)+Region, data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")
summary(Bee_PGLS3)

AIC(Bee_GLS1,Bee_GLS2,Bee_GLS3,Bee_PGLS1,Bee_PGLS2,Bee_PGLS3)
