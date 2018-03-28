

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
head(bee_mean)
unique(bee_mean$Species) #>150
plot(log(Spec.wgt)~log(IT),bee_mean,col=Climate, pch = as.numeric(Region))
#black = aus
#red = aus
#green = aus
#blue = eur triangles /aus
#ligth blue = spain

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
#1# Full model
###

options(na.action = "na.omit") 

str(bee_mean)
plot(bee_mean$Climate, bee_mean$Latitude)

bee.full_lme=lmer(log(Spec.wgt) ~  log(IT) + Latitude + Sex + Family + #fix factors
              log(IT):Latitude + log(IT):Sex + #interactions
              log(IT):Family +
                (1|Measurer),
            data=bee_mean)

##########
##dredge##
##########

options(na.action = "na.fail")

###
#1# Full model
###

bee_dr_lme=dredge(bee.full_lme,beta="none",rank="AIC",
              trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr_lme)
bee_dr_lme[1]

plot(lmer(log(Spec.wgt) ~ log(IT) + Latitude + Sex + Family + 
             log(IT):Latitude + log(IT):Sex + log(IT):Family + (1 | Measurer), 
           data = bee_mean))

###Extract coefficients

bee_dr_mods=get.models(bee_dr_lme[1:5],subset=TRUE)
bee_coef=lapply(bee_dr_mods[1:5],function (x) tidy(x))

bee_dr_mods[1]


# What I would do:
#1) test preservation time in AUS and decide to keep it or not. weight ~ pres time (species) or species by species with the NON averaged dataset.
#1.1) If pres time is imp... you correct it based in Weight ~ a + b*prestime, _ STARTED
#1.2) Then you do the means. - YES
#2) I would test latitude within species for species which you have good coverage. - NEW DATAFRAME
#3) Test why Spain / aus bees are wrong :( 
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


bee_model=lmer(log(Spec.wgt) ~ Family + Latitude + log(IT) + Sex + (1 | Measurer) +      
                 Family:log(IT) + Latitude:log(IT),data=bee_mean)



##File that will be updated

summary(bee_model)

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
str(bee_mean)
ggplot(data=bee_mean,aes(log(IT),log(Spec.wgt)))+
  geom_smooth(data=bee_mean,col=bee_mean$Family,method="lm",se=FALSE)+theme_bw()
  geom_smooth(data=bee_mean,aes(x=log(bee_mean$IT),y=log(bee_mean$Cane),method="lm"))

ggplot(data=bee_mean,aes(log(IT),log(Spec.wgt)))+
  geom_smooth(aes(col=bee_mean$Family),method="lm",se=FALSE)+theme_bw()

ggplot(data=bee_mean,aes(log(Latitude),log(Spec.wgt)))+
  geom_smooth(aes(col=Subfamily),method="lm",se=FALSE)+theme_bw()
       