#Data preparation
#read data (1 file)----
poll_all <- read.csv(file="data/PA090518.csv")
#split to bees and hoverflies
poll_all$Spec.wgt=poll_all$Spec.wgt*1000
poll_all_split=split(poll_all,poll_all$Superfamily)
bee_all=poll_all_split[[1]]
hov_all=poll_all_split[[2]]

#########################
#Species mean dataframes#
#########################

#Bees
options(na.action = "na.omit")

bee_mean=aggregate(Latitude~Family+Subfamily+Tribe+Region+Country+Measurement+Subfamily+Genus+Species+Sex,bee_all,mean)
bee_mean$Longitude=as.numeric(unlist(aggregate(Longitude~Family+Country+Subfamily+Tribe+Region+Measurement+
                                                Subfamily+Genus+Species+Sex,bee_all,mean)[10]))
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Country+Subfamily+Tribe+Region+Measurement+
                                                Subfamily+Genus+Species+Sex,bee_all,mean)[10]))
bee_mean$IT=(as.numeric(unlist(aggregate(IT~Family+Subfamily+Tribe+Region+Country+
                                         Measurement+Subfamily+Genus+Species+Sex,bee_all,mean)[10])))

##Hoverflies
options(na.action = "na.omit")
hov_mean=aggregate(Latitude~Family+Tribe+Country+
                     Region+Measurement+Subfamily+Genus+Species+Sex,hov_all,mean)
hov_mean$Longitude=aggregate(Longitude~Family+Tribe+Country+
                     Region+Measurement+Subfamily+Genus+Species+Sex,hov_all,mean)[10]
#hov_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Climate+Species+Sex,hov_all,unique)[4]))
hov_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Tribe+Country+
                                                Region+Measurement+Subfamily+
                                                Genus+Species+Sex,hov_all,mean)[10]))
#hov_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Climate+Species+Sex,hov_all,sd)[4]))
hov_mean$IT=as.numeric(unlist(aggregate(IT~Family+Tribe+Country+
                                          Region+Measurement+Subfamily+
                                          Genus+Species+Sex,hov_all,mean)[10]))
#hov_mean$IT.SD=as.numeric(unlist(aggregate(IT~Climate+Species+Sex,hov_all,sd)[4]))
hov_mean$BL=as.numeric(unlist(aggregate(BL~Family+Tribe+Country+
                                          Region+Measurement+Subfamily+
                                          Genus+Species+Sex,hov_all,mean)[10]))
#hov_mean$BL.SD=as.numeric(unlist(aggregate(BL~Climate+Species+Sex,hov_all,sd)[4]))
str(hov_mean)

## set the seed to make your partition reproductible
set.seed(123)
bee_subset=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-bee_subset, ]
bee_train <- bee_mean[bee_subset, ]


hov_subset=sample(seq_len(nrow(hov_mean)), size = floor(0.8 * nrow(hov_mean)))
hov_test <- hov_mean[-hov_subset, ]
hov_train <- hov_mean[hov_subset, ]


#########
#EXPORTS#
#########

save(poll_all,file =  "pollimetry/data/poll_all.rda")
#bee
save(bee_train,file =  "pollimetry/data/bee_train.rda")
save(bee_test,file =  "pollimetry/data/bee_test.rda")
save(bee_all,file =  "pollimetry/data/bee_all.rda")
#hoverflies
save(hov_train,file =  "pollimetry/data/bee_train.rda")
save(hov_test,file =  "pollimetry/data/bee_test.rda")
save(hov_all,file =  "pollimetry/data/bee_all.rda")

###models
save(bee_model,file =  "pollimetry/data/bee_model.rda")

########PHYLO DATASET
#split to bees and hoverflies

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

Bee2vcv=corPagel(value=0.5,phy=bee_pruned_2,fixed=FALSE)


Bee_PGLS1 = gls(log(Spec.wgt)~log(IT)*Region, data=bee_phylo_2, 
                correlation=Bee2vcv, method="ML")
