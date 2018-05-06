library(phytools)

obj<-contMap(bee_pruned,bee_phylo_WGT)
str(bee_phylo_WGT)
plot(obj)

obj<-contMap(bee_intra_pruned,WGT,lwd=2,fsize=0.4)

obj2<-contMap(bee_pruned,WGT2,lwd=2,fsize=0.4)
obj
plot(obj)

##########

IT<-as.matrix(log(bee_phylo_IT))[,1]
ITsd

phylo.heatmap(bee_pruned,svl)

WGT=as.data.frame(bee_intra_phy[,c("Spec.wgt")])
rownames(WGT)=rownames(bee_intra_phy)
WGT<-as.matrix((WGT))[,1]

WGT2=as.data.frame(bee_phylo[,c("Spec.wgt")])
WGT2=log(WGT2)
rownames(WGT2)=rownames(bee_phylo)
WGT2<-as.matrix((WGT2))[,1]


WGTsd=as.data.frame(bee_intra_phy[,c("Wgt.SD")])
rownames(WGTsd)=rownames(bee_intra_phy)
WGTsd<-as.matrix((WGTsd))[,1]
WGTse=WGTsd/sqrt(bee_species_counts)

IT=as.data.frame(bee_intra_phy[,c("IT")])
rownames(IT)=rownames(bee_intra_phy)
IT<-as.matrix((IT))[,1]

ITsd=as.data.frame(bee_intra_phy[,c("IT.SD")])
rownames(ITsd)=rownames(bee_intra_phy)
ITsd<-as.matrix((ITsd))[,1]
options(na.action = "na.omit")

WGT=as.data.frame(bee_intra_phy[,c("Spec.wgt")])
rownames(WGT)=rownames(bee_intra_phy)
WGT<-as.matrix((WGT))[,1]
WGT=log(WGT)
phylosig(tree=bee_intra_pruned,x=WGT,method="lambda",test=TRUE,nsim=1000)


####CREATE DATAFRAME OF SAMPLE SIZES FOR BEES WITH MORE THAN ONE ie. intra 
#Remove two flavipanurgus

bee_count=bee_all[!bee_all$Species==c("Flavipanurgus_venustus"),]

bee_species_counts=as.matrix(table(bee_count$Species))

##Go through each row and determine if a value is zero
row_sub = apply(bee_species_counts, 1, function(row) all(row >1 ))
##Subset as usual 
bee_species_counts=bee_species_counts[row_sub,]

WGTse=WGTsd/sqrt(bee_species_counts)
ITse=ITsd/sqrt(bee_species_counts)

##TEST OF SIGNIFICANCE OF PHYLOSIGNAL
#WGT
phylosig(tree=bee_intra_pruned,x=WGT,method="lambda",se=WGTse,test=TRUE)

#IT
phylosig(tree=bee_intra_pruned,x=IT,method="lambda",se=ITse,test=TRUE)

###Phylo regression
pgls.Ives(tree=bee_intra_pruned, X=IT, y=WGT, Vx=ITse, Vy=WGTse)

bee_pgls2=pgls.SEy(log(Spec.wgt)~log(IT), data=bee_intra_phy, corClass=corPagel, tree=bee_intra_pruned,
         se=WGTse, method=c("REML","ML"), interval=c(0,1000))


bee_pgls2$coefficients[1]+bee_pgls2$coefficients[2]*bee_test$IT
predict(bee_pgls2,bee_test)
Bee_PGLS1
