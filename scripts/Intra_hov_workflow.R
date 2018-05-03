table(hov_all$Species)

Helophilus_parallelus 19
Sphaerophoria_macrogaster 17
Episyrphus_balteatus 15
Melanostoma_mellinum 12
Syritta_pipiens 12

hov_species=split(hov_all,hov_all$Species)

##ONE##
#Helophilus_parallelus
#IT
HHP_IT = c()
for(i in 1:length(hov_species$Helophilus_parallelus$IT)){
  subset1 = sample(hov_species$Helophilus_parallelus$IT, i)
  HHP_IT[i] = mean(subset1)
}

#Specimen weight
HHP_WT = c()
for(i in 1:length(hov_species$Helophilus_parallelus$Spec.wgt)){
  subset1 = sample(hov_species$Helophilus_parallelus$Spec.wgt, i)
  HHP_WT[i] = mean(subset1)
}


##TWO##

#Sphaerophoria_macrogaster
#IT
HSM_IT = c()
for(i in 1:length(hov_species$Sphaerophoria_macrogaster$IT)){
  subset1 = sample(hov_species$Sphaerophoria_macrogaster$IT, i)
  HSM_IT[i] = mean(subset1)
}

#Specimen weight
HSM_WT = c()
for(i in 1:length(hov_species$Sphaerophoria_macrogaster$Spec.wgt)){
  subset1 = sample(hov_species$Sphaerophoria_macrogaster$Spec.wgt, i)
  HSM_WT[i] = mean(subset1)
}

##THREE
#Episyrphus_balteatus
HEB_IT = c()
for(i in 1:length(hov_species$Episyrphus_balteatus$IT)){
  subset1 = sample(hov_species$Episyrphus_balteatus$IT, i)
  HEB_IT[i] = mean(subset1)
}

#Specimen weight
HEB_WT = c()
for(i in 1:length(hov_species$Episyrphus_balteatus$Spec.wgt)){
  subset1 = sample(hov_species$Episyrphus_balteatus$Spec.wgt, i)
  HEB_WT[i] = mean(subset1)
}

##FOUR##
#Melanostoma_mellinum
#IT
HMM_IT = c()
for(i in 1:length(hov_species$Melanostoma_mellinum$IT)){
  subset1 = sample(hov_species$Melanostoma_mellinum$IT, i)
  HMM_IT[i] = mean(subset1)
}

#Specimen weight
HMM_WT = c()
for(i in 1:length(hov_species$Melanostoma_mellinum$Spec.wgt)){
  subset1 = sample(hov_species$Melanostoma_mellinum$Spec.wgt, i)
  HMM_WT[i] = mean(subset1)
}

##FIVE##
#Syritta_pipiens
HSP_IT = c()
for(i in 1:length(hov_species$Syritta_pipiens$IT)){
  subset1 = sample(hov_species$Syritta_pipiens$IT, i)
  HSP_IT[i] = mean(subset1)
}


#Specimen weight
HSP_WT = c()
for(i in 1:length(hov_species$Syritta_pipiens$Spec.wgt)){
  subset1 = sample(hov_species$Syritta_pipiens$Spec.wgt, i)
  HSP_WT[i] = mean(subset1)
}

##graphed three for aesthetics based on size range
par(mfrow=c(2,3))
par(pty="s")
plot(HHP_IT,main="Helophilus parallelus",ylab = "ITD (mm)",xlab="")
plot(HSM_IT,main="Sphaerophoria macrogaster",ylab = "",xlab="")
plot(HEB_IT,main="Episyrphus balteatus",ylab = "",xlab="Sample size")
#plot(HMM_IT,main="Melanostoma_mellinum",ylab = "",xlab="")
#plot(HSP_IT,main="Syritta_pipiens",ylab = "",xlab="Sample size")

plot(HHP_WT,ylab = "Specimen weight (mg)",xlab="Sample size")
plot(HSM_WT,ylab = "",xlab="Sample size")
plot(HEB_WT,ylab = "",xlab="Sample size")
#plot(HMM_WT,ylab = "",xlab="Sample size")
#plot(HSP_WT,ylab = "",xlab="Sample size")
