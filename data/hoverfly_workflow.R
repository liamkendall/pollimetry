##hoverfly_workflow

#libraries
require(MuMIn)
require(broom)
require(caret)

data(hov_mean)
data(hov_test)
data(hov_train)

table(hov_mean$Climate)
##Diagnostic plots
plot(Spec.wgt~IT,hov_mean)
par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,hov_mean))

plot(lm(log(Spec.wgt)~log(IT),hov_mean))

#1. #Full model - wo/ pres.time -Only one specimen - we can remove or keep
options(na.action = "na.omit") 

hov.full=lm(log(Spec.wgt) ~ log(IT) + Latitude + Sex + Subfamily + Climate + #fix
            log(IT):Latitude + log(IT):Sex + log(IT):Climate + #interactions
              log(IT):Subfamily,data=hov_mean)

summary(hov.full)


#DREDGE

options(na.action = "na.fail")

#Full model

hov_dr=dredge(hov.full,beta="none",rank="AIC",trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(hov_dr)
bee_dr[1:10]

plot(hov_mean$Climate,hov_mean$Spec.wgt)
##WIth AICc  - Same again
hov_dr_Cc=dredge(hov.full,beta="none",rank="AICc",trace=100) #think about "sd" and "AICc". AIC show same pattern.
head(hov_dr_Cc)

###Extract coefficients
hov_dr_mods=get.models(hov_dr[1:5],subset=TRUE)


#Retrieve the estimates (package broom)----

hov_coef=lapply(hov_dr_mods[1:5],function (x) tidy(x))

#Top five models
summary(hov_dr_mods[1]$`143`) #Rsq 0.739
summary(hov_dr_mods[2]$`15`) #Rsq 0.7343
summary(hov_dr_mods[3]$`207`) #Rsq 0.7407
summary(hov_dr_mods[4]$`144`) #0.7474
summary(hov_dr_mods[5]$`15`) #0.7344
poll_all[1245,]

##File that will be updated - not sure about naming convention out of dredge
hov_model=tidy(hov_dr_mods[1]$`142`)


hov_mod_int=lm(formula = log(Spec.wgt) ~ Country + log(IT) + Sex + log(IT):Sex, data = hov_mean)
summary(hov_mod_int)
hov_mod=tidy(summary(hov_mod_int))
