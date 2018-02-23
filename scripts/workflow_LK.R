#workflow

###Issues / Things to do
#Need latitude for Spanish samples (Assume no pres.time) - Set to Seville latitude

#Assess the inluence confounding variables (remove col.method) in part == Pres.time

##Work flow

##Use species mean for simplicity

#####Models

#Remove collection method

#dredge with and without and compare coefficients

#Training set - Check species

#fit model (with and without pres.time)

#With country / region

#extract co-efficients (with and without pres.time)

####working function

#-one column w/ pres, one column wout pres.

#Run test set through functions old and new
  
#add country - region

#libraries
require(MuMIn)
require(broom)
require(caret)

#########Bee workflow

##Diagnostic plots
plot(Spec.wgt~IT,bee)
par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,bee))

plot(lm(log(Spec.wgt)~log(IT),bee))

#Kinda makes sense to use species means
#because australasia has a massive signal by sample size/species


#SKIPPED
##Latitude (*-1 for australasia) 
#bee_2=split(bee,bee$Region)
#bee_2$Australasia$Latitude=bee_2$Australasia$Latitude*-1
#bee=rbind.data.frame(bee_2$Australasia,bee_2$Europe)
#Maybe not? Check.

#boxplot(bee$Latitude~bee$Region)

#One check how many specimens needed to stabilize mean and variance----

#############

#Make a full model IT-length ----

#WITH COUNTRY

#1. #Full model - w/ pres.time
options(na.action = "na.omit") 

bee.full=lm(log(Spec.wgt) ~ log(IT) + Latitude + Sex + Family + Country + Pres.time #fix
              + log(IT):Latitude + log(IT):Sex + log(IT):Country + #interactions
              log(IT):Pres.time +
              log(IT):Family,data=bee_mean)

#2. #Full model  w/out pres.time
bee.full.np=lm(log(Spec.wgt) ~ log(IT) + Latitude + Sex + Family + Country #fix
            + log(IT):Latitude + log(IT):Sex + log(IT):Country + #interactions
              log(IT):Family,data=bee_mean)

summary(bee.full.np)

#Sex... ignore and pool or have a mean for female and a mean for male - DONE.

#Same for method - DONE - REMOVED 22-2. I would ignore pres time.

#Check all species have only one region.- DONE
table(bee_mean$Species,bee_mean$Country)

#Duplicates - species names in dataframe - FIXED
#mellifera, cingulata, bicolor

##lucorum and lucorum agg == lucorum ##data file 22-2

#Maybe drop covariates not relevant in the first analysis?
#took out pres. time

#Option 1: dredge ()----
##I think AICc is good, because simple/parsimonous = better

options(na.action = "na.fail")

#Full model - w pres.time

bee_dr=dredge(bee.full,beta="none",rank="AIC",trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr)
bee_dr[1:10]

#Preservative time is in second best model at 0.0004 (+) per day

#Full model - w/out pres.time
bee_dr.np=dredge(bee.full.np,beta="none",rank="AIC",trace=100)
head(bee_dr.np)

##WIth AICc  - Same again
bee_dr_Cc=dredge(bee.full,beta="none",rank="AICc",trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr)

##Top models are the same... 

#Tabulate dredge Top 5 w. pres.time (as same without)

###Extract coefficients
bee_dr_mods=get.models(bee_dr[1:5],subset=TRUE)


#Retrieve the estimates (package broom)----

bee_coef=lapply(bee_dr_mods[1:5],function (x) tidy(x))

#For top five models
summary(bee_dr_mods[1]$`112`) #Rsq 0.9293 not bad
summary(bee_dr_mods[2]$`1136`) #Rsq 0.9291
summary(bee_dr_mods[3]$`128`) #Rsq 0.9291
summary(bee_dr_mods[4]$`368`) #0.9291 
summary(bee_dr_mods[5]$`240`) #0.9302 

##File that will be updated - not sure about naming convention out of dredge
bee_model=tidy(bee_dr_mods[1]$`112`)
rownames(bee_model)=bee_model[,1]
bee_model
bee_dr_mods[1]$`112`

bee_mod_int=lm(formula = log(Spec.wgt) ~ 0+Country + Family + log(IT) + Latitude + Sex + 
     Country:log(IT), data = bee_mean)
bee_mod=tidy(summary(bee_mod_int))

#To DO:

#- DONE - Need to change for HOV -data_prep file: loads original from github data and creates 3 rda's train test and all in the package.
#- analysis script for selecting best model...
    #DONE dredge with and without preservative time
    #DONE Select top five models
    #NEED TO TABULATE
    #Would model averaging be good here?

#- help files for data (.rda's and .R) - DONE 6 files (3 bee, 3 hoverfly)) - RE-DO Data prep to match

#- make a wrapper to other existing equations (1 R file)

#- Check species in more than one region/country

#main analysis: 
  #confounding factors 
  #explain best model at mean level?
  #justify sample size to acccurate mean?
  #discuss PGLS and phylo signal?

#- replicate all for hoverflies and foraging distances

#- Extra weigth flies Monday.
