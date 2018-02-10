##Bee Foraging range equations

##Greenleaf et al. 2007

GreenleafMax=function(x){exp(-1.363 + 3.366*log(x))}
  
GreenleafTyp=function(x){exp(-1.643 + 3.242*log(x))}

GreenleafFeeder=function(x){exp(-0.760 + 2.313*log(x))}

GreenleafComm=function(x){exp(-0.993 + 2.788*log(x))}

GreenleafMax(1)
GreenleafTyp(1)
GreenleafFeeder(1)
GreenleafComm(1)

##van Nieuwstadt & Iraheta 1996

#Head width and foraging size in stingless bees

VNI1996=function(x){550.9*(x)-579.1}

VNI1996_2=function(x){560.8*(x)-908.2}

VNI1996(2)
