###Bee analyses

PredAllo=read.csv("PredAlloPoll10218.csv",header=T)

##Individual data frames
PredAllo.split=split(PredAllo,PredAllo$Taxa)
BEE=PredAllo.split[[1]]
HOV=PredAllo.split[[2]]

#One specimen without genus
BEE=BEE[-454,]
BEE[454,]


#Graphing parameters
par(mfrow=c(2,2))
par(pty="s")

#Libraries
#1
Bee.ITm1=lm(log(Spec.wgt)~log(IT),BEE)
plot(Bee.ITm1)
#2
Bee.ITm2=lm(log(Spec.wgt)~log(IT)+Country,BEE)
plot(Bee.ITm2)
#3
Bee.ITm3=lm(log(Spec.wgt)~log(IT)+Country+Subfamily,BEE)
plot(Bee.ITm3)
#4
Bee.ITm4=lm(log(Spec.wgt)~log(IT)+Country+Tribe,BEE)
plot(Bee.ITm4)
#5
Bee.ITm5=lm(log(Spec.wgt)~log(IT)+Country+Genus,BEE)
plot(Bee.ITm5)
is.na(BEE$Genus)

summary(Bee.ITm1)
#Adjusted R-squared:  0.8488 
summary(Bee.ITm2)
#Adjusted R-squared:  0.9268 
summary(Bee.ITm3)
#Adjusted R-squared:  0.9314 
summary(Bee.ITm4)
#Adjusted R-squared:  0.9351 
summary(Bee.ITm5)
#Adjusted R-squared:  0.9421 

anova(Bee.ITm1,Bee.ITm2,Bee.ITm3,Bee.ITm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(IT)
#Model 2: log(Spec.wgt) ~ log(IT) + Country
#Model 3: log(Spec.wgt) ~ log(IT) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(IT) + Country + Genus
  #Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
#1   1074 392.17                                    
#2   1071 189.30  3   202.872 485.1239 < 2.2e-16 ***
#3   1059 175.48 12    13.824   8.2643 4.911e-15 ***
#4   1036 144.41 23    31.064   9.6891 < 2.2e-16 ***

AIC(Bee.ITm1,Bee.ITm2,Bee.ITm3,Bee.ITm4,Bee.ITm5)

#df       AIC
#Bee.ITm1  3 1973.5501
#Bee.ITm2  6 1195.8314
#Bee.ITm3 18 1138.2381
#Bee.ITm5 41  974.6007


#Different sample size due to missing tribes
#Bee.ITm4 28 1002.8947

summary(Bee.ITm1)
#Adjusted R-squared:  0.8485 
summary(Bee.ITm2)
#Adjusted R-squared:  0.9267 
summary(Bee.ITm3)
#Adjusted R-squared:  0.9312 
summary(Bee.ITm4)
#Adjusted R-squared:  0.9351
summary(Bee.ITm5)
#Adjusted R-squared:  0.9422

###Body length
Bee.BLm1=lm(log(Spec.wgt)~log(BL),BEE)
plot(Bee.BLm1)
Bee.BLm2=lm(log(Spec.wgt)~log(BL)+Country,BEE)
plot(Bee.BLm2)
Bee.BLm3=lm(log(Spec.wgt)~log(BL)+Country+Subfamily,BEE)
plot(Bee.BLm3)
Bee.BLm4=lm(log(Spec.wgt)~log(BL)+Country+Tribe,BEE)
plot(Bee.BLm4)
Bee.BLm5=lm(log(Spec.wgt)~log(BL)+Country+Genus,BEE)
plot(Bee.BLm5)

anova(Bee.BLm1,Bee.BLm2,Bee.BLm3,Bee.BLm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(BL)
#Model 2: log(Spec.wgt) ~ log(BL) + Country
#Model 3: log(Spec.wgt) ~ log(BL) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(BL) + Country + Genus
  #Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
#1   1074 522.45                                    
#2   1071 240.22  3   282.228 618.6484 < 2.2e-16 ***
#3   1059 184.16 12    56.069  30.7260 < 2.2e-16 ***
#4   1036 157.54 23    26.614   7.6095 < 2.2e-16 ***

summary(Bee.BLm1)
#Adjusted R-squared:  0.7981 
summary(Bee.BLm2)
#Adjusted R-squared:  0.9069 
summary(Bee.BLm3)
#Adjusted R-squared:  0.9278 
summary(Bee.BLm4)
#Adjusted R-squared:  0.9355 
summary(Bee.BLm5)
#Adjusted R-squared:  0.9369 

##IT*BL

Bee.ITBLm1=lm(log(Spec.wgt)~log(ITxBL),BEE)
plot(Bee.ITBLm1)
Bee.ITBLm2=lm(log(Spec.wgt)~log(ITxBL)+Country,BEE)
plot(Bee.ITBLm2)
Bee.ITBLm3=lm(log(Spec.wgt)~log(ITxBL)+Country+Subfamily,BEE)
plot(Bee.ITBLm3)
Bee.ITBLm4=lm(log(Spec.wgt)~log(ITxBL)+Country+Tribe,BEE)
plot(Bee.ITBLm4)
Bee.ITBLm5=lm(log(Spec.wgt)~log(ITxBL)+Country+Genus,BEE)
plot(Bee.ITBLm5)

anova(Bee.ITBLm1,Bee.ITBLm2,Bee.ITBLm3,Bee.ITBLm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(ITxBL)
#Model 2: log(Spec.wgt) ~ log(ITxBL) + Country
#Model 3: log(Spec.wgt) ~ log(ITxBL) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(ITxBL) + Country + Genus
  #Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
#1   1074 397.81                                    
#2   1071 170.40  3   227.408 588.5981 < 2.2e-16 ***
#3   1059 154.85 12    15.556  10.0660 < 2.2e-16 ***
#4   1036 133.42 23    21.427   7.2338 < 2.2e-16 ***

summary(Bee.ITBLm1)
#Adjusted R-squared:  0.8463 
summary(Bee.ITBLm2)
#Adjusted R-squared:  0.934 
summary(Bee.ITBLm3)
#Adjusted R-squared:  0.9393
summary(Bee.ITBLm4)
#Adjusted R-squared:  0.9442
summary(Bee.ITBLm5)
#Adjusted R-squared:  0.9466 


#########
# Is gender important??)

#One specimen without genus
table(BEE$Sex)
#1
Bee.ITSm1=lm(log(Spec.wgt)~log(IT)+Sex,BEE)
plot(Bee.ITSm1)
#2
Bee.ITSm2=lm(log(Spec.wgt)~log(IT)+Country+Sex,BEE)
plot(Bee.ITSm2)
#3
Bee.ITSm3=lm(log(Spec.wgt)~log(IT)+Country+Subfamily+Sex,BEE)
plot(Bee.ITSm3)
#4
Bee.ITSm4=lm(log(Spec.wgt)~log(IT)+Country+Tribe+Sex,BEE)
plot(Bee.ITSm4)
#5
Bee.ITSm5=lm(log(Spec.wgt)~log(IT)+Country+Genus+Sex,BEE)
plot(Bee.ITSm5)

summary(Bee.ITSm1)
#Adjusted R-squared:  0.8507 
summary(Bee.ITSm2)
#Adjusted R-squared:  0.927 
summary(Bee.ITSm3)
#Adjusted R-squared:  0.9317 
summary(Bee.ITSm4)
#Adjusted R-squared:  0.9353 
summary(Bee.ITSm5)
#Adjusted R-squared:  0.9425 

