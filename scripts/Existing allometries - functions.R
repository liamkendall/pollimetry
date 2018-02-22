##Functions
Wrap-all #function, 
#1. select Taxa (Check)
#2. select data type (BL, BLxW, HW, un-transformed) (Check)
#3. Long/Lat provided (Check), calculate distance between eq and samples
#4. Optional: check if values within range of equation sample

##Check all mm

##IT equations
Cane = function(x){exp(0.6453 + 2.4691*log(x))}
curve(Cane)

##########
DIPTERA
##########
##Head width eqiuations
H97DB
H97DN

##Body length equations
#Diptera,Nematocera,Brachycera,Cyclorrapha,Non-Nematocera,Asilidae,Bombyliidae
##Diptera
#ALL
R77D
S80DMF    
S80DCF
S80DCR
GR84D
S93DA
G97D
JS00DA
BN06D
W13D

#Nematocera
S93DN
JS00DN
Sabo02DN
#Brachycera
S93DB
Sabo02DB

#Cyclorrapha
S93DC

#Non-Nematocera
JS00DO

#Asilidae
Sabo02DA

#Bombyliidae
Sabo02DBB


##Body length x width equations
#ALL
S93DALW
W13DLW

#Nematocera
S93DNLW
#Brachycera
S93DBLW
#Cycclorapha
S93DCLW

------------------------------------------------------
##Rogers et al 1977
  
#dip D

R77D
R77D = function(x){exp(-3.293 + 2.366*log(x))}
curve(R77D)
  
#hym

R77H = function(x){exp(-3.871 + 2.407*log(x))}
curve(R77H,add=T)

#lep
  
R77L = function(x){exp(-4.037 + 2.903*log(x))}
curve(R77L,add=T)
  
#extra - Ants
  
R77A = function(x){exp(-4.029 + 2.572*log(x))}
curve(R77A,add=T)
  
-----------------------------------------------------
##Schoener 1980 - mg & mm


#MF = Massachesutts
#CF = Costa Rica dry forest
#CR = Costa Rica rainforest

#dip
S80DMF    
S80DCF
S80DCR

S80DMF = function(x){exp(log(0.022) + 2.42*log(x))}
curve(S80DMF)
  
S80DCF = function(x){exp(log(0.074) + 1.64*log(x))}
curve(S80DCF,add=T)
  
S80DCR = function(x){exp(log(0.068) + 1.59*log(x))}
curve(S80DCR)
  
#hym - winged
  
S80HMF = function(x){exp(log(0.016) + 2.55*log(x))}
curve(S80HMF)
  
S80HCF = function(x){exp(log(0.043) + 2.07*log(x))}
curve(S80HCF)

S80HCR = function(x){exp(log(0.022) + 2.29*log(x))}
curve(S80HCR)
  
#hym - for (ants)
  
S80FMF = function(x){exp(log(0.034) + 2.19*log(x))}
curve(S80FMF)
  
S80FCF = function(x){exp(log(0.012) + 2.72*log(x))}
curve(S80FCF)
  
S80FCR = function(x){exp(log(0.021) + 2.31*log(x))}
curve(S80FCR)
  
#lEP
  
S80LMF = function(x){exp(log(0.014) + 2.55*log(x))}
curve(S80LMF)
  
S80LCF = function(x){exp(log(0.026) + 2.5*log(x))}
curve(S80LCF)
  
S80LCR = function(x){exp(log(0.078) + 1.32*log(x))}
curve(S80LCR)
  
-------------
  
#Gowing & Recher 1984
  
#lnWt = ln a + b*ln(x)
  
##dip
GR84D
GR84D = function(x){exp(-3.653 + log(x)*2.546)}
curve(GR84D,add=T)  

##hym - exc for - exp model

GR84H = function(x){exp(-2.86 + (x)*0.478)}
curve(GR84H,add=F) 

##for (ants)

GR84F = function(x){exp(-3.997 + log(x)*2.489)}
curve(GR84F)

------------
#Sample et al 1993
  
#Length & Length*Width models

#Dip - all
S93DA
S93DN
S93DB
S93DC

S93DALW
S93DNLW
S93DBLW
S93DCLW

  #Length
S93DA = function(x){exp(-3.184 + 2.23*log(x))}
curve(S93DA)

#Length * Width

S93DALW = function(x){exp(-2.197 + 1.309*log(x))}
curve(S93DALW,add=T)

#Dip - Nematocera

#Length
S93DN = function(x){exp(-3.675 + 2.212*log(x))}
curve(S93DN,add=T)

#Length * Width

S93DNLW = function(x){exp(-2.217 + 1.288*log(x))}
curve(S93DNLW,add=T)

#Dip - Brachycera

#Length
S93DB = function(x){exp(-3.374 + 2.158*log(x))}
curve(S93DB,add=T)

#Length * Width

S93DBLW = function(x){exp(-2.2 + 1.259*log(x))}
curve(S93DBLW,add=T)

#Dip - Cyclorrapha

#Length
S93DC = function(x){exp(-3.619 + 2.632*log(x))}
curve(S93DC,add=T)

#Length * Width

S93DCLW = function(x){exp(-2.02 + 1.298*log(x))}
curve(S93DCLW,add=T)

---
  
##Hymenoptera

#Hym - All
  
#Length
S93HA = function(x){exp(-4.284 + 2.696*log(x))}
curve(S93HA,add=T)

#Length * Width

S93HALW = function(x){exp(-2.375 + 1.456*log(x))}
curve(S93HALW,add=T)

#Hym - Ich

#Length
S93HI = function(x){exp(-4.149 + 2.464*log(x))}
curve(S93HI,add=T)

#Length * Width

S93HILW = function(x){exp(-2.497 + 1.445*log(x))}
curve(S93HILW,add=T)

#Hym - Bra

#Length
S93HB = function(x){exp(-3.854 + 2.441*log(x))}
curve(S93HB,add=T)

#Length * Width

S93HBLW = function(x){exp(-2.19 + 1.445*log(x))}
curve(S93HBLW,add=T)

#Hym - Ves

#Length
S93HV = function(x){exp(-3.54 + 2.782*log(x))}
curve(S93HV,add=T)

#Length * Width

S93HVLW = function(x){exp(-1.537 + 1.319*log(x))}
curve(S93HVLW,add=T)

#Hym - FOR

#Length
S93HF = function(x){exp(-4.727 + 2.919*log(x))}
curve(S93HF,add=T)

#Length * Width

S93HFLW = function(x){exp(-2.378 + 1.473*log(x))}
curve(S93HFLW,add=T)

#Hym - HAL

#Length
S93HH = function(x){exp(-2.891 + 2.302*log(x))}
curve(S93HH,add=T)

#Length * Width

S93HHLW = function(x){exp(-1.946 + 1.444*log(x))}
curve(S93HHLW,add=T)

#Hym - POM

#Length
S93HP = function(x){exp(-2.341 + 2.006*log(x))}
curve(S93HP,add=T)

#Length * Width

S93HPLW = function(x){exp(-1.946 + 1.444*log(x))}
curve(S93HPLW,add=T)

----
  
#LEPIDOPTERA
  
#LEP - ALL
  
#Length
S93LA = function(x){exp(-5.036 + 3.122*log(x))}
curve(S93LA,add=T)

#Length * Width

S93LALW = function(x){exp(-2.607 + 1.457*log(x))}
curve(S93LALW,add=T)
  
#LEP - mic

#Length
S93LM = function(x){exp(-4.913 + 2.918*log(x))}
curve(S93LM,add=T)

#Length * Width

S93LMLW = function(x){exp(-2.715 + 1.395*log(x))}
curve(S93LMLW,add=T)

#LEP - NOC

#Length
S93LN = function(x){exp(-3.337 + 2.499*log(x))}
curve(S93LN,add=T)

#Length * Width

S93LNLW = function(x){exp(-1.607 + 1.214*log(x))}
curve(S93LNLW,add=T)

#LEP - GEO

#Length
S93LG = function(x){exp(-4.172 + 2.628*log(x))}
curve(S93LG,add=T)

#Length * Width

S93LGLW = function(x){exp(-2.343 + 1.387*log(x))}
curve(S93LGLW,add=T)

#LEP - ARC

#Length
S93LC = function(x){exp(-3.755 + 2.658*log(x))}
curve(S93LC,add=T)

#Length * Width

S93LCLW = function(x){exp(-1.658 + 1.222*log(x))}
curve(S93LCLW,add=T)

-----
  
#Ganihar 1997
  
#Diptera
  
G97D = function(x){exp(-3.4294 + 2.5943*log(x))}
curve(G97D)

#Hymenoptera - EXC FOR

G97H = function(x){exp(-3.5917 + 2.6429*log(x))}
curve(G97H,add=T)

#Hymenoptera - FOR 

G97F = function(x){exp(-3.1415 + 2.3447*log(x))}
curve(G97F,add=T)

#Lepidoptera

G97L = function(x){exp(-4.7915 + 2.8585*log(x))}
curve(G97L,add=T)

------

#JOHNSON & STRONG 2000
  
#DIPTERA
JS00DA
JS00DN
JS00DO
#DIP - ALL
  
JS00DA = function(x){exp(-2.462 + 1.881*log(x))}
curve(JS00DA)
    
#DIP = NEM

JS00DN = function(x){exp(-2.562 + 1.373*log(x))}
curve(JS00DN,add =T)

#DIP = ALL - NEM  

JS00DO = function(x){exp(-2.105 + 1.805*log(x))}
curve(JS00DO,add=T)

#HYMENOPTERA

#HYM - ALL

JS00HA = function(x){exp(-3.556 + 2.193*log(x))}
curve(JS00HA)

#HYM - FOR

JS00HF = function(x){exp(-3.730 + 2.103*log(x))}
curve(JS00HF,add =T)

#HYM - HYM WITHOUT FOR

JS00HNF = function(x){exp(-3.295 + 2.102*log(x))}
curve(JS00HNF,add =T)

#LEPIDOPTERA

#LEP

JS00L = function(x){exp(-3.268 + 2.243*log(x))}
curve(JS00L, add=T)

----
  
##Hodar 1997

##Head Width
H97DB
H97DN

#DIPTERA
  
#Brachycera
H97DB = function(x){0.655*(x)^2.526}
curve(H97DB,add=T)

#Nematocera
H97DN = function(x){3.942*(x)^3.106}
curve(H97DN,add=T)

#HYMENOPTERA

#all
H97HA = function(x){1.999*(x)^2.09}
curve(H97HA,add=T)

#For - workers
H97FO = function(x){0.522*(x)^2.55}
curve(H97FO,add=T)

#For - winged
H97FW = function(x){1.607*(x)^2.752}
curve(H97FW,add=T)

#LEPIDOPTERA

#HETEROCERA

H97LH = function(x){2.053*(x)^2.804}
curve(H97LH,add=T)

H97LR = function(x){1.634*(x)^2.793}
curve(H97LR)

----
  
##Sage et al. 1982

lnY = a + bX + b1*x^2

"quadratic polynomial equation"

#Lepidoptera

Sage82=function(x){exp(-8.108+(0.38002*(x))+((-0.0055428)*((x)^2)))}
Sage82(10)
curve(Sage82,add=F)

-----
  
##Sabo et al. (2002)

#DIPTERA
Sabo02DB
Sabo02DN
Sabo02DA
Sabo02DB
#Brachycera
Sabo02DB=function(x){0.006*(x)^3.05}
curve(Sabo02DB,xlim=c(0,100))

#Nematocera
Sabo02DN=function(x){0.1*(x)^1.57}
curve(Sabo02DN,add=T)

#Asilidae

Sabo02DA=function(x){0.38*(x)^1.5}

#Bombyliidae

Sabo02DBB=function(x){0.007*(x)^3.337}

#Hymenoptera

Sabo02H=function(x){0.56*(x)^1.56}
curve(Sabo02H,add=T)

#Apidae

Sabo02HA=function(x){0.006*(x)^3.407}

#Vespidae

Sabo02HV=function(x){0.001*(x)^3.723}
curve(Sabo02HV,add=T)
----
  
##Brady & Noske 2006

### SOMETHING WRONG WITH all/WINGED HYM ONE - may be error in publishing
  

#Diptera
  BN06D
#linear model
BN06D=function(x){-0.041+0.010*(x)}
curve(BN06D)

#Hym  -winged

#a co-efficient too high

BN06H1 = function(x){exp(log(6.783)+2.544*log(x))}
curve(BN06H1,add=T)

#Hym - FOR

BN06HF = function(x){exp(log(0.001)+2.33*log(x))}
curve(BN06HF,add=F)

##Lepidoptera

BN06L = function(x){exp(log(0.001)+2.313*log(x))}
curve(BN06L,add=T)

-------------
##Wardhaugh 2013

##Diptera
  W13D
W13DLW
W13D= function(x){exp(-3.29+2.65*log(x))}
curve(W13D,xlim=c(0,100),add=F)

#length * width

W13DLW=function(x){exp(-1.91+1.22*log(x))}
curve(W13DLW,add=T)

##Hym

W13H = function(x){exp(-4.3+3*log(x))}
curve(W13H,add=T)

#length * width

W13HLW=function(x){exp(-2.1+1.34*log(x))}
curve(W13HLW,add=T)

##LEP

W13L = function(x){exp(-3.83+2.77*log(x))}
curve(W13L,add=T)

#length * width

W13LLW=function(x){exp(-2.1+1.37*log(x))}
curve(W13LLW,add=T)

