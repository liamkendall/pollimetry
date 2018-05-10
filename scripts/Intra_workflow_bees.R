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


##ONE##
#Homalictus_urbanus
#IT
HU_IT = c()
for(i in 1:length(bee_species$Homalictus_urbanus$IT)){
  subset1 = sample(bee_species$Homalictus_urbanus$IT, i)
  HU_IT[i] = mean(subset1)
}
plot(HU_IT)

#Specimen weight
HU_WT = c()
for(i in 1:length(bee_species$Homalictus_urbanus$Spec.wgt)){
  subset1 = sample(bee_species$Homalictus_urbanus$Spec.wgt, i)
  HU_WT[i] = mean(subset1)
}
plot(HU_WT)

##TWO##

#Lasioglossum_pauxillum
#IT
LP_IT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum$IT)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum$IT, i)
  LP_IT[i] = mean(subset1)
}
plot(LP_IT)

#Specimen weight
LP_WT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum$Spec.wgt, i)
  LP_WT[i] = mean(subset1)
}
plot(LP_WT)

##THREE
#Bombus_lucorum
BL_IT = c()
for(i in 1:length(bee_species$Bombus_lucorum$IT)){
  subset1 = sample(bee_species$Bombus_lucorum$IT, i)
  BL_IT[i] = mean(subset1)
}
plot(BL_IT)

#Specimen weight
BL_WT = c()
for(i in 1:length(bee_species$Bombus_lucorum$Spec.wgt)){
  subset1 = sample(bee_species$Bombus_lucorum$Spec.wgt, i)
  BL_WT[i] = mean(subset1)
}
plot(BL_WT)


##FOUR##
#Andrena_flavipes
#IT
AF_IT = c()
for(i in 1:length(bee_species$Andrena_flavipes$IT)){
  subset1 = sample(bee_species$Andrena_flavipes$IT, i)
  AF_IT[i] = mean(subset1)
}
plot(AF_IT)

#Specimen weight
AF_WT = c()
for(i in 1:length(bee_species$Andrena_flavipes$Spec.wgt)){
  subset1 = sample(bee_species$Andrena_flavipes$Spec.wgt, i)
  AF_WT[i] = mean(subset1)
}
plot(AF_WT)


##FIVE##
#Lasioglossum_lanarium
LL_IT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium$IT)){
  subset1 = sample(bee_species$Lasioglossum_lanarium$IT, i)
  LL_IT[i] = mean(subset1)
}
plot(LL_IT)

#Specimen weight
LL_WT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_lanarium$Spec.wgt, i)
  LL_WT[i] = mean(subset1)
}
plot(LL_WT)

par(mfrow=c(2,5))
plot(HU_IT)
plot(LP_IT)
plot(BL_IT)
plot(AF_IT)
plot(LL_IT)

plot(HU_WT)
plot(LP_WT)
plot(BL_WT)
plot(AF_WT)
plot(LL_WT)