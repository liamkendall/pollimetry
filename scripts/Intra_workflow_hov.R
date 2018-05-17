table(hov_all$Species)
#Females
Austrosyrphus 32
Helophilus_parallelus 19
Sphaerophoria_macrogaster 10
Episyrphus_balteatus 11
Melanostoma_scalare 9


hov_species=split(hov_all,hov_all$Species)

##ONE##
#Austrosyrphus 
HAA_IT = c()
for(i in 1:length(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$IT)){
  subset1 = sample(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$IT, i)
  HAA_IT[i] = mean(subset1)
}


#Specimen weight
HAA_WT = c()
for(i in 1:length(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$Spec.wgt, i)
  HAA_WT[i] = mean(subset1)
}
##TWO##
#Helophilus_parallelus
#IT
HHP_IT = c()
for(i in 1:length(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$IT)){
  subset1 = sample(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$IT, i)
  HHP_IT[i] = mean(subset1)
}

#Specimen weight
HHP_WT = c()
for(i in 1:length(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$Spec.wgt, i)
  HHP_WT[i] = mean(subset1)
}


##THREE

#Sphaerophoria_macrogaster
#IT
HSM_IT = c()
for(i in 1:length(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$IT)){
  subset1 = sample(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$IT, i)
  HSM_IT[i] = mean(subset1)
}

#Specimen weight
HSM_WT = c()
for(i in 1:length(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$Spec.wgt, i)
  HSM_WT[i] = mean(subset1)
}

##FOUR##
#Episyrphus_balteatus
HEB_IT = c()
for(i in 1:length(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$IT)){
  subset1 = sample(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$IT, i)
  HEB_IT[i] = mean(subset1)
}

#Specimen weight
HEB_WT = c()
for(i in 1:length(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$Spec.wgt, i)
  HEB_WT[i] = mean(subset1)
}

##FIVE##
#Melanostoma_scalare
#IT
HMS_IT = c()
for(i in 1:length(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$IT)){
  subset1 = sample(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$IT, i)
  HMS_IT[i] = mean(subset1)
}

#Specimen weight
HMS_WT = c()
for(i in 1:length(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$Spec.wgt, i)
  HMS_WT[i] = mean(subset1)
}




##graphed three for aesthetics based on size range
par(mfrow=c(2,5))
par(pty="s")
plot(HAA_IT,main="Austrosyrphus spp.",ylab = "ITD (mm)",xlab="")
abline(a=HAA_IT[32],b=0,col=2)
abline(a=confint(lm(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$IT~1))[2],b=0,col=3)

plot(HHP_IT,main="Helophilus parallelus",ylab = "",xlab="",ylim=c(3.6,4))
abline(a=HHP_IT[19],b=0,col=2)
abline(a=confint(lm(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$IT~1))[2],b=0,col=3)

plot(HEB_IT,main="Episyrphus balteatus",ylab = "",xlab="",ylim=c(2.1,2.55))
abline(a=HEB_IT[11],b=0,col=2)
abline(a=confint(lm(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$IT~1))[2],b=0,col=3)

plot(HSM_IT,main="Sphaerophoria macrogaster",ylab = "",xlab="",ylim=c(1.1,1.5))
abline(a=HSM_IT[10],b=0,col=2)
abline(a=confint(lm(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$IT~1))[2],b=0,col=3)

plot(HMS_IT,main="Melanostoma scalare",ylab = "",xlab="",ylim=c(1.45,1.7))
abline(a=HMS_IT[9],b=0,col=2)
abline(a=confint(lm(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$IT~1))[2],b=0,col=3)

plot(HAA_WT,ylab = "Specimen weight (mg)",xlab="Sample size")
abline(a=HAA_WT[32],b=0,col=2)
abline(a=confint(lm(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Austrosyphus_aussp1[hov_species$Austrosyphus_aussp1$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

plot(HHP_WT,ylab = "",xlab="Sample size",ylim=c(32,42))
abline(a=HHP_WT[19],b=0,col=2)
abline(a=confint(lm(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

plot(HEB_WT,ylab = "",xlab="Sample size",ylim=c(6,14))
abline(a=HEB_WT[11],b=0,col=2)
abline(a=confint(lm(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

plot(HSM_WT,ylab = "",xlab="Sample size",ylim=c(1.2,2))
abline(a=HSM_WT[10],b=0,col=2)
abline(a=confint(lm(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

plot(HMS_WT,ylab = "",xlab="Sample size",ylim=c(1.5,6.5))
abline(a=HMS_WT[9],b=0,col=2,ylim=c(1.5,6.5))
abline(a=confint(lm(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(hov_species$Melanostoma_scalare[hov_species$Melanostoma_scalare$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
