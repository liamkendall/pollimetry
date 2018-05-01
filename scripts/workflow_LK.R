#libraries
library(MuMIn)
library(lme4)
library(broom)
library(caret)
library(ggplot2)
source("https://bioconductor.org/biocLite.R")
biocLite("ggtree")
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
plot(log(Spec.wgt)~log(IT),hov_mean,col=Subfamily)
plot(log(Spec.wgt)~log(IT),hov_mean,col=Country)

#Check outlier
hov_mean[ which(log(hov_mean$Spec.wgt)  < -6.25), ]

par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,hov_mean[-40,]))
plot(lm(log(Spec.wgt)~log(IT),hov_mean[-40,]))

##########
###BEES###
################
#####Models#####
################

###
#1# Full model
###

bee_mean$Spec.wgt=bee_mean$Spec.wgt*1000
bee_mean$Spec.wgt=log(bee_mean$Spec.wgt)
bee_mean$IT=log(bee_mean$IT)

options(na.action = "na.omit") 


##WITH Region
bee_f_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                              log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                              (1|Measurement),REML=FALSE,bee_mean)

#Without Region
bee_f_lme_red=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex  + #fix factors
                 log(IT):Family + log(IT):Sex + #interactions
                 (1|Measurement),REML=FALSE,bee_mean)

##########
##dredge##
##########

options(na.action = "na.fail")

###
#1# Full model
###

bee_dr_lme=dredge(bee_f_lme,beta="none",rank="AIC",
              trace=10) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr_lme)

bee_dr_lme_red=dredge(bee_f_lme_red,beta="none",rank="AICc",
                  trace=10) 
head(bee_dr_lme_red)
bee_f_lme_red

#GET Top MODEL
bee_dr_lme_mod=get.models(bee_dr_lme[1],subset=TRUE)

bee_dr_lme_red_mod=get.models(bee_dr_lme_red[1],subset=TRUE)

r.squaredGLMM(bee_dr_lme_mod[[1]])
#R2m       R2c 
#0.8833778 0.8952139 
r.squaredGLMM(bee_dr_lme_red_mod[[1]])
#R2m       R2c 
#0.8647225 0.8712988
summary(bee_dr_lme_red_mod[[1]])
# What I would do:
#1) test preservation time in AUS and decide to keep it or not. weight ~ pres time (species) or species by species with the NON averaged dataset.
#1.1) If pres time is imp... you correct it based in Weight ~ a + b*prestime, _ STARTED
#1.2) Then you do the means. - YES
#2) I would test latitude within species for species which you have good coverage. - NEW DATAFRAME
#3) Test why Spain / aus bees are wrong :( DONE
#4) Make means and test  #sup mat when the mean stabilizes or sensitivity analysis)
#bee.reduced=lm(log(Spec.wgt) ~ 0 + log(IT) + Climate/Region + 
 # +Sex + Family +  #fix factors
 # log(IT):Sex + log(IT):Climate + #interactions
 # log(IT):Family
 # ,data=bee_mean)
#5) Select best model and use that one for the function.
# you can show PGLS as sup mat here (without family then).
#6) IN THE PACKAGE: Parametrize the function with train data and test with test data. #as extra with bootstrap.
#7) test all other functions with test data and compare... 
# and for Hoverflies, and for foraging distances.

##May this would be useful as a function
#    = with or without preservation time - as it is very commom


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

hov_model=lmer(log(Spec.wgt) ~ log(IT) + Sex + Subfamily + (1 | Measurement) +      log(IT):Sex,hov_mean)

r.squaredGLMM(hov_model)
#R2m       R2c 
#0.7635235 0.7759880 
r.squaredLR(hov_model)
#[1] 0.7761203
#attr(,"adj.r.squared")
#[1] 0.825845

hov_coefs=lapply(hov_dr_mods[1],function (x) tidy(x))

ggplot(data=hov_mean,aes(log(IT),log(Spec.wgt)))+
  geom_smooth(aes(col=Subfamily),method="lm",se=FALSE)+theme_bw()+geom_point()




 # geom_smooth(data=bee_mean,aes(x=log(bee_mean$IT),y=log(bee_mean$Cane),method="lm"))

ggplot(data=bee_all,aes(log(IT),log(Spec.wgt)))+
  geom_smooth(aes(),method="glm",se=FALSE)+theme_bw()+
  geom_smooth(data=bee_mean,aes(col=2),method="glm",se=FALSE)+
  geom_point(aes(pch=1))

  
geom_smooth(data=bee_mean,aes(x=log(bee_mean$IT),y=log(bee_mean$Cane),method="lm"))

ggplot(data=bee_mean,aes(IT,Spec.wgt))+
  geom_smooth(aes(col=Family),method="lm",se=FALSE)+theme_bw()
