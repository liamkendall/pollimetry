#Data preparation
#read data (1 file)----
poll_all <- read.csv(file="data/PA010518.csv")

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
poll_all_split=split(poll_all,poll_all$Superfamily)
bee_all.2=poll_all_split[[1]]
hov_all.2=poll_all_split[[2]]

bee_phylo=aggregate(Latitude~Family+Subfamily+
                      Tribe+Genus+Species,bee_all,median)
bee_phylo$Longitude=as.numeric(unlist(aggregate(Longitude~Family+Subfamily+
                                                  Tribe+Genus+Species,bee_all,median)[6]))
bee_phylo$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Subfamily+
                                                 Tribe+Genus+Species,bee_all,mean)[6]))
bee_phylo$Wgt.SD=as.numeric(unlist(aggregate(log(Spec.wgt)~Family+Subfamily+
                                               Tribe+Genus+Species,bee_all,sd)[6]))
bee_phylo$IT=as.numeric(unlist(aggregate(IT~Family+Subfamily+
                                           Tribe+Genus+Species,bee_all,mean)[6]))
bee_phylo$IT.SD=as.numeric(unlist(aggregate(log(IT)~Family+Subfamily+
                                              Tribe+Genus+Species,bee_all,sd)[6]))

bee_phylo=bee_phylo[-93,]
rownames(bee_phylo)=bee_phylo$Species

