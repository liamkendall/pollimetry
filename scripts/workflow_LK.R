#libraries
library(MuMIn)
library(lme4)
library(broom)
library(caret)
library(ggplot2)
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

###

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

####QUICK DIAGNOSTICS FOR BEES AND HOVERFLIES

##BEES
head(bee_mean)
unique(bee_mean$Species) #>150

#black = aus
#red = aus
#green = aus
#blue = eur triangles /aus
#ligth blue = spain

par(pty="s")
par(mfrow=c(2,2))

plot(log(Spec.wgt)~log(IT),bee_mean,col=Measurement)
plot(log(Spec.wgt)~log(IT),bee_mean,col=Region)
plot(log(Spec.wgt)~log(IT),bee_mean,col=Measurement)
plot(log(Spec.wgt)~log(IT),bee_mean,col=Country)

##HOVERFLIES
par(mfrow=c(2,2))
plot(log(Spec.wgt)~log(IT),hov_mean,col=Measurement)
plot(log(Spec.wgt)~log(IT),hov_mean,col=Region)
plot(log(Spec.wgt)~log(IT),hov_mean,col=Measurement)
plot(log(Spec.wgt)~log(IT),hov_mean,col=Country)

#Check outlier
hov_mean[ which(log(hov_mean$Spec.wgt)  < -6.25), ]

par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,hov_mean[-40,]))
plot(lm(log(Spec.wgt)~log(IT),hov_mean[-40,]))

##########
###BEES###
##########

#####Models
#############

##Added interaction between climate and latitude

###
#1# Full model
###

options(na.action = "na.omit") 

bee_f_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                              log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                              (1|Measurement),REML=FALSE,bee_mean)
plot(bee_f_lme)

AIC(bee_f_lme)

##########
##dredge##
##########

options(na.action = "na.fail")

###
#1# Full model
###

bee_dr_lme=dredge(bee_f_lme,beta="none",rank="AICc",
              trace=10) #think about "sd" and "AICc". AIC show same pattern.

head(bee_dr_lme)

bee_dr_lme_mod=get.models(bee_dr_lme[1],subset=TRUE)
bee_dr_lme_mod

#best model
bee_lme_model=lmer(log(Spec.wgt) ~ Family + log(IT) + Region + Sex + (1 | Measurement) +  
                     Family:log(IT) + log(IT):Region + log(IT):Sex, data = bee_mean)

r.squaredGLMM(bee_lme_model)
#R2m       R2c 
#0.8708545 0.8715625

r.squaredLR(bee_lme_model)
#[1] 0.8757662
#attr(,"adj.r.squared")
#[1] 0.9149603


################ ################ ################ ################ ################ ################ ################
###HOVERFLIES### ###HOVERFLIES### ###HOVERFLIES### ###HOVERFLIES### ###HOVERFLIES### ###HOVERFLIES### ###HOVERFLIES### 
################ ################ ################ ################ ################ ################ ################ 

##Added interaction between climate and latitude

###
#1# Full model - with preservative time
###

options(na.action = "na.omit") 

hov.full=lmer(log(Spec.wgt) ~ log(IT) + Sex + Subfamily+ #fix
            log(IT):Sex + #interactions
            log(IT):Subfamily + (1|Measurement)
            ,REML=FALSE,data=hov_mean)

##########
##dredge##
##########

options(na.action = "na.fail")

hov_dr=dredge(hov.full,beta="none",rank="AIC",
              trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(hov_dr)
###Extract coefficients

hov_dr_mods=get.models(hov_dr[1],subset=TRUE)

hov_model=lmer(log(Spec.wgt) ~ log(IT) + Subfamily + (1 | Measurement) + log(IT):Subfamily,hov_mean)

r.squaredGLMM(hov_model)
#R2m       R2c 
#0.7276233 0.7338602 
r.squaredLR(hov_model)
#[1] 0.7346383
#attr(,"adj.r.squared")
#[1] 0.7686215

hov_coefs=lapply(hov_dr_mods[1],function (x) tidy(x))


       