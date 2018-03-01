#Data preparation

#libraries
require(caret)
require(kgc)
#read data (1 file)----
poll_all <- read.csv(file="data/PredAlloPoll23218.csv")
str(poll_all)
options(stringsAsFactors = TRUE)

##REMOVED MONSTROSA AND COHORT  (BSk climate zone - 3,
# 1 only preserved hoverfly)

#without Germany and Cane 1987
poll_country <- split(poll_all,poll_all$Country)
poll_all <- rbind(poll_country$Australia,poll_country$Britain,poll_country$Spain,poll_country$Ireland)

##Add climate zones
poll_climate <- data.frame(poll_all, rndCoord.lon = RoundCoordinates(poll_all$Longitude),
                           rndCoord.lat = RoundCoordinates(poll_all$Latitude))

poll_all <- data.frame(poll_climate,Climate=LookupCZ(poll_climate))
poll_all <- data.frame(poll_all,Cl_simp=strtrim(poll_all$Climate, width=1))

##Could standardise after extracting climate


########PHYLO DATASET
#split to bees and hoverflies
poll_all_split=split(poll_all,poll_all$Superfamily)
bee_all.2=poll_all_split[[1]]
hov_all.2=poll_all_split[[2]]


bee_phylo=aggregate(Latitude~Family+Subfamily+Genus+Species,bee_all.2,median)
bee_phylo$Longitude=as.numeric(unlist(aggregate(Longitude~Species,bee_all.2,median)[2]))
bee_phylo$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Species,bee_all.2,mean)[2]))
bee_phylo$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Species,bee_all.2,sd)[2]))
bee_phylo$IT=as.numeric(unlist(aggregate(IT~Species,bee_all.2,mean)[2]))
bee_phylo$IT.SD=as.numeric(unlist(aggregate(IT~Species,bee_all.2,sd)[2]))
bee_phylo$BL=as.numeric(unlist(aggregate(BL~Species,bee_all.2,mean)[2]))
bee_phylo$BL.SD=as.numeric(unlist(aggregate(BL~Species,bee_all.2,sd)[2]))
bee_phylo$BL.SD=as.numeric(unlist(aggregate(BL~Species,bee_all.2,sd)[2]))

bee_phylo[1,c("Latitude","Longitude")]=c(-14.54,	132.13)
bee_phylo_climate <- data.frame(bee_phylo, rndCoord.lon = RoundCoordinates(bee_phylo$Longitude),
                                rndCoord.lat = RoundCoordinates(bee_phylo$Latitude))

bee_phylo <- data.frame(bee_phylo_climate,Climate=LookupCZ(bee_phylo_climate))
bee_phylo <- data.frame(bee_phylo,Cl_simp=strtrim(bee_phylo$Climate, width=1))
bee_phylo

##RESPLIT WITH STANDARDISED LATITUDE

##Standardise after extracting climate
poll_all2=split(poll_all,poll_all$Region)
poll_all2$Australasia$Latitude=poll_all2$Australasia$Latitude*-1
poll_all=rbind.data.frame(poll_all2$Australasia,poll_all2$Europe)

#split to bees and hoverflies
poll_all_split=split(poll_all,poll_all$Superfamily)
bee_all=poll_all_split[[1]]
hov_all=poll_all_split[[2]]

#########################
#Species mean dataframes#
#########################

#Bees
options(na.action = "na.omit")

bee_mean=aggregate(Latitude~Family+Tribe+Country+Measurer+Subfamily+Genus+Species+Sex,bee_all,mean)
bee_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee_all,sd)[4]))
bee_mean$IT=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$IT.SD=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee_all,sd)[4]))
bee_mean$Cane=Cane(bee_mean$IT)/1000
##Hoverflies
options(na.action = "na.omit")
hov_mean=aggregate(Latitude~Family+Region+Climate+Subfamily+Genus+Species+Sex,hov_all,mean)
hov_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Climate+Species+Sex,hov_all,unique)[4]))
hov_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Climate+Species+Sex,hov_all,mean)[4]))
hov_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Climate+Species+Sex,hov_all,sd)[4]))
hov_mean$IT=as.numeric(unlist(aggregate(IT~Climate+Species+Sex,hov_all,mean)[4]))
hov_mean$IT.SD=as.numeric(unlist(aggregate(IT~Climate+Species+Sex,hov_all,sd)[4]))
hov_mean$BL=as.numeric(unlist(aggregate(BL~Climate+Species+Sex,hov_all,mean)[4]))
hov_mean$BL.SD=as.numeric(unlist(aggregate(BL~Climate+Species+Sex,hov_all,sd)[4]))
str(hov_mean)

## set the seed to make your partition reproductible
set.seed(123)
bee_subset=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-bee_subset, ]
bee_train <- bee_mean[bee_subset, ]


hov_subset=sample(seq_len(nrow(hov_mean)), size = floor(0.8 * nrow(hov_mean)))
hov_test <- hov_mean[-hov_subset, ]
hov_train <- hov_mean[hov_subset, ]

##THE CURRENT BEE MODEL
bee_model=tidy(bee_dr_mods$`2302`)
bee_model





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

