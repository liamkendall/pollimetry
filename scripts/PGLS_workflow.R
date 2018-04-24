#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

library(phytools)
library(nlme)
library(phangorn)
########PHYLO DATASET
#split to bees and hoverflies
poll_all_split=split(poll_all,poll_all$Superfamily)
bee_all=poll_all_split[[1]]
hov_all=poll_all_split[[2]]

bee_phylo=aggregate(Latitude~Family+Subfamily+Tribe+Genus+Species,bee_all,median)
bee_phylo$Longitude=as.numeric(unlist(aggregate(Longitude~Family+Subfamily+Tribe+Genus+Species,bee_all,median)[6]))
bee_phylo$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Subfamily+Tribe+Genus+Species,bee_all,mean)[6]))
bee_phylo$Wgt.SD=as.numeric(unlist(aggregate(log(Spec.wgt)~Family+Subfamily+Tribe+Genus+Species,bee_all,sd)[6]))
bee_phylo$IT=as.numeric(unlist(aggregate(IT~Family+Subfamily+Tribe+Genus+Species,bee_all,mean)[6]))
bee_phylo$IT.SD=as.numeric(unlist(aggregate(log(IT)~Family+Subfamily+Tribe+Genus+Species,bee_all,sd)[6]))
rownames(bee_phylo)=bee_phylo$Species

###MINUS FLAVIPANURGUS FOR NOW
##NO FLAVIPANURGUS on phylogeny by Hedtke - we can add individual tip though
bee_phylo[82,]
bee_phylo=bee_phylo[-82,]

##TREE
bee.phy=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
str(bee.phy[[1]])
bee.phy=bee.phy[[1]]
bee.phy=as.phylo(bee.phy)
#bee.phy=force.ultrametric(bee.phy) Not sure if this is required
plot(bee.phy)

bee_pruned=drop.tip(bee.phy, setdiff(bee.phy$tip.label,bee_phylo$Genus))
plot(bee_pruned)

#bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Subgenus)
bee_pruned=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)

#first compute correlation matrix from tree
Bee_vcv=corPagel(0.5,bee_pruned,fixed=FALSE)

Bee_GLS1<- gls(log(Spec.wgt)~log(IT), data=bee_phylo, method="ML")
summary(Bee_GLS1)

Bee_PGLS1 = gls(log(Spec.wgt)~log(IT), data=bee_phylo, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS1)
#Lambda
#0.6892926

AIC(Bee_GLS1,Bee_PGLS1)
#df      AIC
#Bee_GLS1   3 275.4794
#Bee_PGLS1  4 251.7689

###NEED TO CHECK IF ULTRAMETRICITY IS IMPORTANT AS CHANGES LAMBDA

##GRAPH IT
pgls_pred=predict(Bee_PGLS1,bee_all)
par(pty="s")

plot(Spec.wgt~IT,bee_all)
points(exp(pgls_pred)~bee_all$IT,col="darkred")

#check also lambda of each trait itself.

##Specimen weight
Bee_GLS_WGT1<- gls(log(Spec.wgt)~1, data=bee_phylo, method="ML")
summary(Bee_GLS_WGT1)

Bee_PGLS_WGT1 = gls(log(Spec.wgt)~1, data=bee_phylo, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_WGT1)
#lambda 0.764347   

AIC(Bee_GLS_WGT1,Bee_PGLS_WGT1) #Phylo signal strong
#df      AIC
#Bee_GLS_WGT1   2 706.8179
#Bee_PGLS_WGT1  3 557.8304


##IT span
Bee_GLS_IT1<- gls(log(IT)~1, data=bee_phylo, method="ML")
summary(Bee_GLS_IT1)

Bee_PGLS_IT1 = gls(log(IT)~1, data=bee_phylo, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_IT1)

#lambda 0.8002247  

AIC(Bee_GLS_IT1,Bee_PGLS_IT1) #Really high phylo signal... interesting

#df      AIC
#Bee_GLS_IT1   2 272.5669
#Bee_PGLS_IT1  3  95.26964




