

#################
##MAIN ANALYSES##
#################

#To DO:

###I THINK we need a way to determine if climate or sampling bias important
## - assessing if climate is gradient fits hypothesis
## whats the hypothesis?

#DONE - Need to change for HOV -data_prep file: loads original from github data and creates 3 rda's train test and all in the package.
#- analysis script for selecting best model...
#DONE dredge with and without preservative time
#DONE Select top five models
#NEED TO TABULATE

#STARTED- help files for data (.rda's and .R) - DONE 6 files (3 bee, 3 hoverfly)) - RE-DO Data prep to match

#DONE- make a wrapper to other existing equations (1 R file)

#DONE- Check species in more than one region/country

#main analysis: 
################
##WORKFLOW ONE##
################

#confounding factors 
#explain best model at mean level?

################
##WORKFLOW TWO##
################

#justify sample size to acccurate mean?

##################
##WORKFLOW THREE##
##################

#discuss PGLS and phylo signal?

##NEXT
#- replicate all for hoverflies and foraging distances

#- Extra weigth flies Monday.

##DONE## Need latitude for Spanish samples (Assume no pres.time) - Set to Seville latitude

##DONE## Assess the inluence confounding variables (removed collection method) 
        #Lets keep preservative time in a model BOOM

##DONE #Use species mean for simplicity
        #Mean data frame returned species means for each species
        #in each climate zone and sex and region

##REMOVED 4 specimens from australia sample
        #unique climate zone for bees, only preserved hoverfly) 

#libraries
require(MuMIn)
require(broom)
require(caret)

####QUICK DIAGNOSTICS FOR BEES AND HOVERFLIES

##BEES
plot(log(Spec.wgt)~log(IT),bee_mean,col=Climate)
par(pty="s")
par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,bee_mean))
plot(lm(log(Spec.wgt)~log(IT),bee_mean))

##HOVERFLIES
par(mfrow=c(1,1))
plot(hov_mean$Spec.wgt,hov_mean$IT,col=hov_mean$Region)
plot(log(Spec.wgt)~log(IT),data=hov_mean[-40,],col=Region)

#Check outlier
hov_mean[ which(log(hov_mean$Spec.wgt)  < -6.25), ]

par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,hov_mean[-40,]))
plot(lm(log(Spec.wgt)~log(IT),hov_mean[-40,]))

##########
###BEES###
##########

#####Models
#dredge with and without preservative time and compare coefficients / AIC / BIC
#############

##Added interaction between climate and latitude

###
#1# Full model - with preservative time
###

options(na.action = "na.omit") 

bee.full=lm(log(Spec.wgt) ~ 0+Climate+log(IT) + Latitude + Sex +
              Family + Pres.time #fix
              + log(IT):Latitude + log(IT):Sex + log(IT):Climate + #interactions
              log(IT):Pres.time +
              log(IT):Family+
              Climate:Latitude
              ,data=bee_mean)

###
#2# Full model  without preservative time
###

bee.reduced=lm(log(Spec.wgt) ~ 0 + Climate + 
                 +log(IT) +Latitude +Sex + Family +  #fix
                 log(IT):Latitude + log(IT):Sex + log(IT):Climate + #interactions
                 log(IT):Family+
                 Climate:Latitude,data=bee_mean)

summary(bee.reduced)

##########
##dredge##
##########

options(na.action = "na.fail")

###
#1# Full model - w pres.time
###

bee_dr=dredge(bee.full,beta="none",rank="AIC",
              trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr)

##AIC 58.1

#Preservative time is in best model at 0.0004 (+) per day

#Reduced model without preservative time

bee_dr_reduced=dredge(bee.reduced,beta="none",rank="AIC",trace=100)
head(bee_dr_reduced)

##AIC 76.3

76.3-58.1

##AIC distance of 18.2 (58.1 with pres.time, 76.3 without)##

##May this would be useful as a function
#    = with or without preservation time - as it is very commom

###Extract coefficients

##WITH PRESERVATIVE TIME##

bee_dr_mods=get.models(bee_dr[1:5],subset=TRUE)
bee_coef=lapply(bee_dr_mods[1:5],function (x) tidy(x))

bee_pres=bee_mean$Pres.time*-0.005564608
##WITHOUT PRESERVATIVE TIME##

bee_reduced_mods=get.models(bee_dr_reduced[1:5],subset=TRUE)
bee_reduced_coef=lapply(bee_reduced_mods[1:5],function (x) tidy(x))

bee_models=c(bee_coef,bee_reduced_coef)
names(bee_models)=c("P1","P2","P3","P4","P5", "R1","R2","R3","R4","R5")

##File that will be updated


#WITH PRESERVATIVE TIME
bee_models$P1

#WITHOUT PRESERVATIVE TIME
bee_models$R1

################
###HOVERFLIES###
################

##Added interaction between climate and latitude

###
#1# Full model - with preservative time
###

options(na.action = "na.omit") 

hov.full=lm(log(Spec.wgt) ~ log(IT) + Climate + Latitude + Sex + Subfamily+ #fix
              log(IT):Latitude + log(IT):Sex + log(IT):Climate + #interactions
              log(IT):Subfamily+
              Climate:Latitude
            ,data=hov_mean)

table(bee_mean$Climate, bee_mean$Region)

##########
##dredge##
##########

options(na.action = "na.fail")

hov_dr=dredge(hov.full,beta="none",rank="AIC",
              trace=100) #think about "sd" and "AICc". AIC show same pattern.

##AIC 85.5


##May this would be useful as a function
#    = with or without preservation time - as it is very commom

###Extract coefficients

hov_dr_mods=get.models(hov_dr[1:5],subset=TRUE)
hov_coefs=lapply(hov_dr_mods[1:5],function (x) tidy(x))


names(hov_coefs)=c(rep(1:5,1))

##File that will be updated
hov_coefs$`1`

ggplot(data=bee_mean,aes(IT,Spec.wgt))+
  geom_point(aes(col=bee_mean$Climate),pch=2)+theme_bw()

       