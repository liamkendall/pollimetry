#Hoverfly analyses

PredAllo=read.csv("PredAlloPoll10218.csv",header=T)\

##Individual data frames
PredAllo.split=split(PredAllo,PredAllo$Taxa)
BEE=as.data.frame(PredAllo.split[[1]])
HOV=PredAllo.split[[2]]

## Excluding Spain for now
HOV=HOV[1:162,]

levels(BEE$Genus)

aggregate(BEE$Spec.wgt~BEE$Species,FUN="mean")


##
par(mfrow=c(2,2))
par(pty="s")

##Hoverflies
plot(log(Spec.wgt)~log(IT)+log(BL),HOV,col=Country,ylab=c("Specimen weight (mg)"))

#1
Hov.ITm1=lm(log(Spec.wgt)~log(IT),HOV)
plot(Hov.ITm1)
#2
Hov.ITm2=lm(log(Spec.wgt)~log(IT)+Country,HOV)
plot(Hov.ITm2)
#3
Hov.ITm3=lm(log(Spec.wgt)~log(IT)+Country+Subfamily,HOV)
plot(Hov.ITm3)
#4
Hov.ITm4=lm(log(Spec.wgt)~log(IT)+Country+Tribe,HOV)
plot(Hov.ITm4)
#5
Hov.ITm5=lm(log(Spec.wgt)~log(IT)+Country+Genus,HOV)
plot(Hov.ITm5)

anova(Hov.ITm1,Hov.ITm2,Hov.ITm3,Hov.ITm4,Hov.ITm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(IT)
#Model 2: log(Spec.wgt) ~ log(IT) + Country
#Model 3: log(Spec.wgt) ~ log(IT) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(IT) + Country + Tribe
#Model 5: log(Spec.wgt) ~ log(IT) + Country + Genus

  #Res.Df  RSS   Df Sum of Sq  F      Pr(>F)    
#1    160 50.658                                  
#2    159 20.215  1   30.4430 259.9654 < 2e-16 ***
#3    158 19.568  1    0.6464   5.5196 0.02029 *  
#4    151 19.088  7    0.4807   0.5864 0.76604    
#5    132 15.458 19    3.6298   1.6314 0.05737 .  

AIC(Hov.ITm1,Hov.ITm2,Hov.ITm3,Hov.ITm4,Hov.ITm5)

#df      AIC
#Hov.ITm1  3 277.4098
#Hov.ITm2  4 130.5831
#Hov.ITm3  5 127.3185
#Hov.ITm4 12 137.2891
#Hov.ITm5 31 141.1190

summary(Hov.ITm1)
summary(Hov.ITm2)
summary(Hov.ITm3)
summary(Hov.ITm4)
summary(Hov.ITm5)

###Body length
Hov.BLm1=lm(log(Spec.wgt)~log(BL),HOV)
plot(Hov.BLm1)
Hov.BLm2=lm(log(Spec.wgt)~log(BL)+Country,HOV)
plot(Hov.BLm2)
Hov.BLm3=lm(log(Spec.wgt)~log(BL)+Country+Subfamily,HOV)
plot(Hov.BLm3)
Hov.BLm4=lm(log(Spec.wgt)~log(BL)+Country+Tribe,HOV)
plot(Hov.BLm4)
Hov.BLm5=lm(log(Spec.wgt)~log(BL)+Country+Genus,HOV)
plot(Hov.BLm5)

anova(Hov.BLm1,Hov.BLm2,Hov.BLm3,Hov.BLm4,Hov.BLm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(BL)
#Model 2: log(Spec.wgt) ~ log(BL) + Country
#Model 3: log(Spec.wgt) ~ log(BL) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(BL) + Country + Tribe
#Model 5: log(Spec.wgt) ~ log(BL) + Country + Genus
  #Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
#1    168 90.268                                    
#2    166 38.097  2    52.171 181.2604 < 2.2e-16 ***
#3    165 35.380  1     2.717  18.8822 2.689e-05 ***
#4    156 29.338  9     6.042   4.6647 2.117e-05 ***
#5    137 19.716 19     9.622   3.5191 8.958e-06 ***

summary(Hov.BLm1)
summary(Hov.BLm2)
summary(Hov.BLm3)
summary(Hov.BLm4)
summary(Hov.BLm5)

##IT*BL

Hov.ITBLm1=lm(log(Spec.wgt)~log(ITxBL),HOV)
plot(Hov.ITBLm1)
Hov.ITBLm2=lm(log(Spec.wgt)~log(ITxBL)+Country,HOV)
plot(Hov.ITBLm2)
Hov.ITBLm3=lm(log(Spec.wgt)~log(ITxBL)+Country+Subfamily,HOV)
plot(Hov.ITBLm3)
Hov.ITBLm4=lm(log(Spec.wgt)~log(ITxBL)+Country+Tribe,HOV)
plot(Hov.ITBLm4)
Hov.ITBLm5=lm(log(Spec.wgt)~log(ITxBL)+Country+Genus,HOV)
plot(Hov.ITBLm5)

anova(Hov.ITBLm1,Hov.ITBLm2,Hov.ITBLm3,Hov.ITBLm4,Hov.ITBLm5)
#Analysis of Variance Table

#Model 1: log(Spec.wgt) ~ log(ITxBL)
#Model 2: log(Spec.wgt) ~ log(ITxBL) + Country
#Model 3: log(Spec.wgt) ~ log(ITxBL) + Country + Subfamily
#Model 4: log(Spec.wgt) ~ log(ITxBL) + Country + Tribe
#Model 5: log(Spec.wgt) ~ log(ITxBL) + Country + Genus
  #Res.Df    RSS Df Sum of Sq        F  Pr(>F)    
#1    160 59.981                                  
#2    159 18.779  1    41.202 366.2701 < 2e-16 ***
#3    158 18.189  1     0.590   5.2425 0.02363 *  
#4    151 17.670  7     0.519   0.6585 0.70671    
#5    132 14.849 19     2.822   1.3201 0.18114    

summary(Hov.ITBLm1)
summary(Hov.ITBLm2)
summary(Hov.ITBLm3)
summary(Hov.ITBLm4)
summary(Hov.ITBLm5)

??gls
