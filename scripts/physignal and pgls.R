library(phytools)

##DATA FRAME
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


##REMOVE DUPLICATES FROM REGIONS - Apis from Australia, Halictus rubicundus from NA and Flavipanurgus
#duplicated(bee_phylo_2$Species)

bee_phylo_2[110,]
bee_phylo_2[103,]
bee_phylo_2[52,]

bee_phylo_2=bee_phylo_2[-110,]
bee_phylo_2=bee_phylo_2[-103,]
bee_phylo_2=bee_phylo_2[-52,]

rownames(bee_phylo_2)=bee_phylo_2$Species

##TREE
bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
bee.phy=bee.phy[[1]]
bee.phy=as.phylo(bee.phy)
#bee.phy=force.ultrametric(bee.phy) Not sure if this is required
bee_pruned_2=drop.tip(bee.phy, setdiff(bee.phy$tip.label,bee_phylo_2$Genus))
bee_pruned_2=genus.to.species.tree(bee_pruned_2, species=bee_phylo_2$Species)

##########

WGT=as.data.frame(bee_phylo[,c("Spec.wgt")])
WGT=log(WGT)
rownames(WGT)=rownames(bee_phylo)
WGT<-as.matrix((WGT))[,1]

IT=as.data.frame(bee_phylo_2[,c("IT")])
IT=log(IT)
rownames(IT)=rownames(bee_phylo_2)
IT<-as.matrix((IT))[,1]


##TEST OF SIGNIFICANCE OF PHYLOSIGNAL
#WGT
phylosig(tree=bee_pruned_2,x=WGT,method="lambda",test=TRUE)
phylosig(tree=bee_pruned_2,x=IT,method="lambda",test=TRUE)

##PGLS

#standard gls

Bee_GLS1<- gls(log(Spec.wgt)~log(IT)*Region, data=bee_phylo_2,
               method="ML")

Bee_GLS2<- gls(log(Spec.wgt)~log(IT)+Region, data=bee_phylo_2,
               method="ML")

Bee_GLS3<- gls(log(Spec.wgt)~log(IT), data=bee_phylo_2,
               method="ML")

#PGLS
#compute correlation matrix from tree
Bee2vcv=corPagel(value=0.5,phy=bee_pruned_2,fixed=FALSE)


Bee_PGLS1 = gls(log(Spec.wgt)~log(IT)*Region, data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")

Bee_PGLS2 = gls(log(Spec.wgt)~log(IT)+Region, data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")

Bee_PGLS3 = gls(log(Spec.wgt)~log(IT), data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")

AIC(Bee_GLS1,Bee_GLS2,Bee_GLS3,Bee_PGLS1,Bee_PGLS2,Bee_PGLS3)

dredge(Bee_PGLS1,rank="AIC")

###PLOT BODY SIZE phylogeny

bee_wgt_phy=contMap(bee_pruned,WGT,plot=FALSE)
plot(bee_wgt_phy,fsize=c(0.4,1),lwd=0.75,leg.txt="ln(BS)")
