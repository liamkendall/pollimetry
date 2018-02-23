#Data preparation
#read data (1 file)----
poll_all <- read.csv(file="data/PredAlloPoll22218.csv")
str(poll_all)

#dummy variable for spain latitude
#Hoverflies
poll_all[163:170,c("Latitude")] <- 37.396355
#Bees
poll_all[269:344,c("Latitude")] <- 37.396355

#without Germany and Cane 1987
poll_all <- poll_all[1:1246,]
#split to bees and hoverflies
poll_all_split=split(poll_all,poll_all$Taxa)
bee_all=poll_all_split[[1]]
hov_all=poll_all_split[[2]]

#Species mean dataframes

#Bees
options(na.action = "na.omit")

bee_mean=aggregate(Latitude~Family+Region+Country+Subfamily+Genus+Species+Sex,bee_all,mean)
bee_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee_all,sd)[4]))
bee_mean$IT=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$IT.SD=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee_all,sd)[4]))
bee_mean$BL=as.numeric(unlist(aggregate(BL~Country+Species+Sex,bee_all,mean)[4]))
bee_mean$BL.SD=as.numeric(unlist(aggregate(BL~Country+Species+Sex,bee_all,sd)[4]))

plot(Spec.wgt~IT,bee_mean)

##Fix weight of spanish Flavipanurgus venustus
#bee_mean[207,]$Spec.wgt=bee_mean[207,]$Spec.wgt*0.1

##Hoverflies
options(na.action = "na.omit")
hov_mean=aggregate(Latitude~Family+Region+Country+Subfamily+Genus+Species+Sex,hov_all,mean)
hov_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Country+Species+Sex,hov_all,unique)[4]))
hov_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,hov_all,mean)[4]))
hov_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,hov_all,sd)[4]))
hov_mean$IT=as.numeric(unlist(aggregate(IT~Country+Species+Sex,hov_all,mean)[4]))
hov_mean$IT.SD=as.numeric(unlist(aggregate(IT~Country+Species+Sex,hov_all,sd)[4]))
hov_mean$BL=as.numeric(unlist(aggregate(BL~Country+Species+Sex,hov_all,mean)[4]))
hov_mean$BL.SD=as.numeric(unlist(aggregate(BL~Country+Species+Sex,hov_all,sd)[4]))
str(hov_mean)

## set the seed to make your partition reproductible
set.seed(123)
require(caret)
bee_subset=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-bee_subset, ]
bee_train <- bee_mean[bee_subset, ]

hov_subset=sample(seq_len(nrow(hov_mean)), size = floor(0.8 * nrow(hov_mean)))
hov_test <- hov_mean[-hov_subset, ]
hov_train <- hov_mean[hov_subset, ]



#I would save test train and all.
save(all,file =  "pollimetry/data/poll_all.rda")
save(bee_train,file =  "pollimetry/data/bee_train.rda")
save(bee_test,file =  "pollimetry/data/bee_test.rda")
save(bee_all,file =  "pollimetry/data/bee_all.rda")

save(hov_train,file =  "pollimetry/data/bee_train.rda")
save(hov_test,file =  "pollimetry/data/bee_test.rda")
save(hov_all,file =  "pollimetry/data/bee_all.rda")
