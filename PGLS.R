##Phylo tree from taxonomy

require(ape)
require(caper)
str(HOV)

##Unique tips - one per species
HOVT=HOV[!duplicated(HOV$Species), ]
#Tree based on taxonomy - equal branch lengths
Hov.tree <- as.phylo(~Family/Subfamily/Tribe/Genus/Species, data = HOVT)

#Comparative data frame for PGLS
HOV.CD=comparative.data(data=HOVT,names.col=Species, Hov.tree)

pgls(log(Spec.wgt) ~ log(IT), data = HOV.CD)
