#Intraspecific variation ~ Sample size

#X axis = standard deviation | error of each trait: WGT, IT, BL
#Y axis = sample size


##variation within a species rather than all speceis
###More than ten species

##Top five
bee_species=split(bee_all,bee_all$Species)

#Homalictus_urbanus
#Lasioglossum_pauxillum
#Bombus_lucorum
#Andrena_flavipes
#Lasioglossum_lanarium

set.seed(123)
##ONE##
#Homalictus_urbanus
#IT
HU_IT = c()
for(i in 1:length(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT, i)
  HU_IT[i] = mean(subset1)
}
plot(HU_IT)

#Specimen weight
HU_WT = c()
for(i in 1:length(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt, i)
  HU_WT[i] = mean(subset1)
}
gplot(HU_WT)

##TWO##

#Lasioglossum_pauxillum
#IT
LP_IT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT, i)
  LP_IT[i] = mean(subset1)
}
plot(LP_IT)

#Specimen weight
LP_WT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt, i)
  LP_WT[i] = mean(subset1)
}
plot(LP_WT)

##THREE
#Bombus_lucorum
BL_IT = c()
for(i in 1:length(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$IT, i)
  BL_IT[i] = mean(subset1)
}
plot(BL_IT)

#Specimen weight
BL_WT = c()
for(i in 1:length(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$Spec.wgt, i)
  BL_WT[i] = mean(subset1)
}
plot(BL_WT)


##FOUR##
#Andrena_flavipes
#IT
AF_IT = c()
for(i in 1:length(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT, i)
  AF_IT[i] = mean(subset1)
}
plot(AF_IT)

#Specimen weight
AF_WT = c()
for(i in 1:length(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt, i)
  AF_WT[i] = mean(subset1)
}
plot(AF_WT)


##FIVE##
#Lasioglossum_lanarium
LL_IT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT, i)
  LL_IT[i] = mean(subset1)
}
plot(LL_IT)

#Specimen weight
LL_WT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt, i)
  LL_WT[i] = mean(subset1)
}
plot(LL_WT)
par(pty="s")
par(mfrow=c(2,5))
#1
plot(HU_IT,main="Homalictus urbanus",ylab = "ITD (mm)",xlab="")
abline(a=HU_IT[211],b=0,col=2)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT~1))[2],b=0,col=3)
#2
plot(LP_IT,main="Lasioglossum pauxillum",ylab = "ITD (mm)",xlab="")
abline(a=LP_IT[112],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT~1))[2],b=0,col=3)
#3
plot(BL_IT,main="Bombus lucorum",ylab = "ITD (mm)",xlab="")
abline(a=BL_IT[103],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$IT~1))[2],b=0,col=3)
#4
plot(AF_IT,main="Andrena flavipes",ylab = "ITD (mm)",xlab="")
abline(a=AF_IT[59],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT~1))[2],b=0,col=3)
#5
plot(LL_IT,main="Lasioglossum lanarium",ylab = "ITD (mm)",xlab="")
abline(a=LL_IT[63],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT~1))[2],b=0,col=3)

#1
plot(HU_WT,xlab="Sample size",ylab = "Specimen weight (mg)")
abline(a=HU_WT[211],b=0,col=2)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#2
plot(LP_WT,xlab="Sample size",ylab="")
abline(a=LP_WT[112],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#3
plot(BL_WT,xlab="Sample size",ylab="")
abline(a=BL_WT[103],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#4
plot(AF_WT,xlab="Sample size",ylab="")
abline(a=AF_WT[59],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#5
plot(LL_WT,xlab="Sample size",ylab="")
abline(a=LL_WT[63],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

