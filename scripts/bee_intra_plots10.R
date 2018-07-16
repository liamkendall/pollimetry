#Andrena flavipes
#Andrena nigroaenea 
#Bombus_impatiens
#Bombus_lapidarius
#Bombus terrestris
#Homalictus urbanus
#Lasioglossum_glabriusculum
#Lasioglossum lanarium
#Lasioglossum pauxillum
#Trigona_spinipes

set.seed(123)
##ONE##
#Homalictus_urbanus
#IT
HU_IT = c()
for(i in 1:length(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT, i)
  HU_IT[i] = mean(subset1)
}


#Specimen weight
HU_WT = c()
for(i in 1:length(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt, i)
  HU_WT[i] = mean(subset1)
}


##TWO##

#Lasioglossum_pauxillum
#IT
LP_IT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT, i)
  LP_IT[i] = mean(subset1)
}


#Specimen weight
LP_WT = c()
for(i in 1:length(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt, i)
  LP_WT[i] = mean(subset1)
}


##THREE
#Bombus_lucorum
BT_IT = c()
for(i in 1:length(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$IT, i)
  BT_IT[i] = mean(subset1)
}


#Specimen weight
BT_WT = c()
for(i in 1:length(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$Spec.wgt, i)
  BT_WT[i] = mean(subset1)
}

##FOUR##
#Andrena_flavipes
#IT
AF_IT = c()
for(i in 1:length(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT, i)
  AF_IT[i] = mean(subset1)
}


#Specimen weight
AF_WT = c()
for(i in 1:length(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt, i)
  AF_WT[i] = mean(subset1)
}

##FIVE##
#Lasioglossum_lanarium
LL_IT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT, i)
  LL_IT[i] = mean(subset1)
}

#Specimen weight
LL_WT = c()
for(i in 1:length(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt, i)
  LL_WT[i] = mean(subset1)
}

##Six##
#Andrena_nigroaenea 
AN_IT = c()
for(i in 1:length(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$IT, i)
  AN_IT[i] = mean(subset1)
}

#Specimen weight
AN_WT = c()
for(i in 1:length(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$Spec.wgt, i)
  AN_WT[i] = mean(subset1)
}

##Seven##
#Bombus_impatiens 
BI_IT = c()
for(i in 1:length(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$IT)){
  subset1 = sample(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$IT, i)
  BI_IT[i] = mean(subset1)
}

#Specimen weight
BI_WT = c()
for(i in 1:length(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$Spec.wgt)){
  subset1 = sample(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$Spec.wgt, i)
  BI_WT[i] = mean(subset1)
}

##Eight##
#Bombus_lapidarius 
BL_IT = c()
for(i in 1:length(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$IT, i)
  BL_IT[i] = mean(subset1)
}

#Specimen weight
BL_WT = c()
for(i in 1:length(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$Spec.wgt, i)
  BL_WT[i] = mean(subset1)
}

##Nine##
#Lasioglossum_glabriusculum 
LG_IT = c()
for(i in 1:length(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$IT, i)
  LG_IT[i] = mean(subset1)
}

#Specimen weight
LG_WT = c()
for(i in 1:length(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$Spec.wgt, i)
  LG_WT[i] = mean(subset1)
}

##Ten##
#Trigona_spinipes 
TS_IT = c()
for(i in 1:length(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$IT)){
  subset1 = sample(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$IT, i)
  TS_IT[i] = mean(subset1)
}

#Specimen weight
TS_WT = c()
for(i in 1:length(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$Spec.wgt)){
  subset1 = sample(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$Spec.wgt, i)
  TS_WT[i] = mean(subset1)
}

par(pty="s")
par(mfrow=c(4,5))

#Andrena flavipes
#Andrena nigroaenea 
#Bombus_impatiens
#Bombus_lapidarius
#Bombus terrestris
#Homalictus urbanus
#Lasioglossum_glabriusculum
#Lasioglossum lanarium
#Lasioglossum pauxillum
#Trigona_spinipes
par(mfrow=c(4,5),pty="s")
#1
plot(AF_IT,main="Andrena flavipes",ylab = "ITD (mm)",xlab="")
abline(a=AF_IT[72],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$IT~1))[2],b=0,col=3)

#2
plot(AN_IT,main="Andrena nigroaenea",ylab = "",xlab="")
abline(a=AN_IT[52],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$IT~1))[2],b=0,col=3)
#3
plot(BI_IT,main="Bombus impatiens",ylab = "",xlab="")
abline(a=BI_IT[68],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$IT~1))[2],b=0,col=3)
#4
plot(BL_IT,main="Bombus_lapidarius",ylab = "",xlab="")
abline(a=BL_IT[56],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$IT~1))[2],b=0,col=3)
#5
plot(BT_IT,main="Bombus terrestris",ylab = "",xlab="")
abline(a=BT_IT[83],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$IT~1))[2],b=0,col=3)

#WEIGHT
#1
plot(AF_WT,main="",ylab = "Dry weight (mg)",xlab="")
abline(a=AF_WT[72],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

#2
plot(AN_WT,main="",ylab = "",xlab="")
abline(a=AN_WT[52],b=0,col=2)
abline(a=confint(lm(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Andrena_nigroaenea[bee_species$Andrena_nigroaenea$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#3
plot(BI_WT,main="",ylab = "",xlab="")
abline(a=BI_WT[68],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_impatiens[bee_species$Bombus_impatiens$Sex == "Male",]$Spec.wgt~1))[2],b=0,col=3)
#4
plot(BL_WT,main="",ylab = "",xlab="")
abline(a=BL_WT[56],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_lapidarius[bee_species$Bombus_lapidarius$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#5
plot(BT_WT,main="",ylab = "",xlab="")
abline(a=BT_WT[83],b=0,col=2)
abline(a=confint(lm(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Bombus_terrestris[bee_species$Bombus_terrestris$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

#Homalictus urbanus
#Lasioglossum_glabriusculum
#Lasioglossum lanarium
#Lasioglossum pauxillum
#Trigona_spinipes
#####IT
#6
plot(HU_IT,main="Homalictus urbanus",ylab = "ITD (mm)",xlab="")
abline(a=HU_IT[211],b=0,col=2)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$IT~1))[2],b=0,col=3)

#7
plot(LG_IT,main="Lasioglossum glabriusculum",ylab = "",xlab="")
abline(a=LG_IT[49],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$IT~1))[2],b=0,col=3)
#8
plot(LL_IT,main="Lasioglossum lanarium",ylab = "",xlab="")
abline(a=LL_IT[63],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$IT~1))[2],b=0,col=3)
#9
plot(LP_IT,main="Lasioglossum pauxillum",ylab = "",xlab="")
abline(a=LP_IT[131],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$IT~1))[2],b=0,col=3)
#10
plot(TS_IT,main="Trigona spinipes",ylab = "",xlab="")
abline(a=TS_IT[50],b=0,col=2)
abline(a=confint(lm(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$IT~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$IT~1))[2],b=0,col=3)

#WEIGHT
#6
plot(HU_WT,main="",ylab = "Dry weight (mg)",xlab="Sample size (n)")
abline(a=HU_WT[211],b=0,col=2)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

#7
plot(LG_WT,main="",ylab = "",xlab="Sample size (n)")
abline(a=LG_WT[49],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_glabriusculum[bee_species$Lasioglossum_glabriusculum$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#8
plot(LL_WT,main="",ylab = "",xlab="Sample size (n)")
abline(a=LL_WT[63],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#9
plot(LP_WT,main="",ylab = "",xlab="Sample size (n)")
abline(a=LP_WT[131],b=0,col=2)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)
#10
plot(TS_WT,main="",ylab = "",xlab="Sample size (n)")
abline(a=TS_WT[50],b=0,col=2)
abline(a=confint(lm(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$Spec.wgt~1))[1],b=0,col=3)
abline(a=confint(lm(bee_species$Trigona_spinipes[bee_species$Trigona_spinipes$Sex == "Female",]$Spec.wgt~1))[2],b=0,col=3)

