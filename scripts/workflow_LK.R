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


table(bee$Species,bee$Pres.time)

#libraries
require(MuMIn)
require(lme4)
library(nlme)
require(broom)
require(glmnet)
library(ape)

#read data (1 file)----

all <- read.csv(file="data/PredAlloPoll22218.csv")
str(all)

#dummy variable for spain latitude
all[269:344,c("Latitude")] <- 37.396355

#without Germany and Cane 1987
all <- all[1:1246,]
#split to bees and hoverflies
all_split=split(all,all$Taxa)
bee=all_split[[1]]
fly=all_split[[2]]


#########Bee workflow

##Diagnostic plots
plot(Spec.wgt~IT,bee)
par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,bee))

par(mfrow=c(1,3))
par(pty="s")
plot(Spec.wgt~IT,bee)
plot(log(Spec.wgt)~log(IT),bee)



plot(Spec.wgt~IT,bee)
par(mfrow=c(2,2))
plot(lm(Spec.wgt~IT,bee))

##Create species mean dataset
str(bee_mean$Species)

#bee_mean dataframe
bee_mean=aggregate(Latitude~Family+Country+Subfamily+Genus+Species+Sex,bee,mean)
bee_mean$Region=as.numeric(unlist(aggregate(Region~Species+Country+Sex,bee,unique)[4]))
bee_mean$Pres.time=as.numeric(unlist(aggregate(Pres.time~Country+Species+Sex,bee,mean)[4]))
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee,mean)[4]))
bee_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Country+Species+Sex,bee,sd)[4]))
bee_mean$IT=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee,mean)[4]))
bee_mean$IT.SD=as.numeric(unlist(aggregate(IT~Country+Species+Sex,bee,sd)[4]))
bee_mean$BL=as.numeric(unlist(aggregate(BL~Country+Species+Sex,bee,mean)[4]))
bee_mean$BL.SD=as.numeric(unlist(aggregate(BL~Country+Species+Sex,bee,sd)[4]))

str(bee_mean)
par(mfrow=c(1,1))
table(bee_mean$Country)

##log weight and IT
bee_mean[,c("Spec.wgt","IT")] <- log(bee_mean[,c("Spec.wgt","IT")])

plot(bee_mean$IT~bee_mean$Spec.wgt,col=bee_mean$Country)

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

bee.full=lm(Spec.wgt ~ IT + Latitude + Sex + Family + Country + Pres.time #fix
              + IT:Latitude + IT:Sex + IT:Country + #interactions
                 IT:Pres.time +
                IT:Family,data=bee_mean)

summary(bee.full)

#2. #Full model  w/out pres.time
bee.full.np=lm(Spec.wgt ~ IT + Latitude + Sex + Family + Country #fix
            + IT:Latitude + IT:Sex + IT:Country + #interactions
              IT:Family,data=bee_mean)

summary(bee.full.np)

#Sex... ignore and pool or have a mean for female and a mean for male - DONE.

#Same for method - DONE - REMOVED 22-2. I would ignore pres time.

#Check all species have only one region.- DONE
table(bee_mean$Species,bee_mean$Region)

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
0.0002*200

#Preservative time is in third best model at 0.0002 per day

#Full model - w/out pres.time
bee_dr.np=dredge(bee.full.np,beta="none",rank="AIC",trace=100)
head(bee_dr.np)

##Top models are the same... 

###Extract coefficients
bee_dr_mods=get.models(bee_dr[1:5],subset=TRUE)
summary(bee_dr_mods)
#Retrieve the estimates (package broom)----

bee_coef=lapply(bee_dr_mods[1:3],function (x) tidy(x))

#For top three models
summary(bee_dr_mods[1]$`112`) #Rsq 0.9293 not bad
summary(bee_dr_mods[2]$`1136`)
summary(bee_dr_mods[3]$`128`)

table(bee$Family)
##File that will be updated - not sure about naming convention out of dredge
bee_model=tidy(bee_dr_mods[1]$`112`)
rownames(bee_model)=bee_model[,1]
bee_model
bee_dr_mods[1]$`112`

bee_mod_int=lm(formula = Spec.wgt ~ 0+Country + Family + IT + Latitude + Sex + 
     Country:IT, data = bee_mean)

bee_mod=tidy(summary(bee_mod_int))
rownames(bee_mod)=bee_mod[,c("term")]

##Dummy data set for function
Country + Family + IT + Latitude + Sex

dummy=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-dummy, ]
bee_train <- bee_mean[dummy, ]
bee_mean$Dataset=

bee_test_mod=lm(formula = Spec.wgt ~ Country + Family + IT + Latitude + Sex + 
    Country:IT, data = bee_train)

predict(bee_test_mod,newdata = bee_test[,c("Country","Family","IT","Latitude","Sex")])

plot(Spec.wgt~IT,data=bee_test)

##166 / 42 species




#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

Bee_tree=as.phylo(~Family/Subfamily/Genus/Species, data=bee_mean)
plot(Bee_tree)

#gives the tree random branch lengths (will try a bunch to see how it impacts the results)
Bee_tree.rand <- compute.brlen(Bee_tree,method="Grafen")

summary(Bee_tree.rand)

#first compute correlation matrix from tree
Bee_vcv=corPagel (0.5,Bee_tree.rand,fixed=FALSE)

Bee_GLS1<- gls(Spec.wgt~IT, data=bee_mean, method="ML")
summary(Bee_GLS1)

Bee_PGLS1 = gls(Spec.wgt~IT, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS1)

#lambda 0.3206538  

AIC(Bee_GLS1,Bee_PGLS1)
#df      AIC
#Bee_GLS1   3 501.8803
#Bee_PGLS1  4 468.6825

#check also lambda of the trait itself.

##Specimen weight
Bee_GLS_WGT1<- gls(Spec.wgt~1, data=bee_mean, method="ML")
summary(Bee_GLS_WGT1)

Bee_PGLS_WGT1 = gls(Spec.wgt~1, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_WGT1)
#lambda 0.00281186  

AIC(Bee_GLS_WGT1,Bee_PGLS_WGT1) #Phylo signal weak
#df      AIC
#Bee_GLS_WGT1   2 671.6540
#Bee_PGLS_WGT1  3 673.6317

##IT span
Bee_GLS_IT1<- gls(IT~1, data=bee_mean, method="ML")
summary(Bee_GLS_IT1)

Bee_PGLS_IT1 = gls(IT~1, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_IT1)

#lambda -0.0.003330929 

AIC(Bee_GLS_IT1,Bee_PGLS_IT1) #Really low phylo signal... good.
#df      AIC
#Bee_GLS_IT1   2 289.4437
#Bee_PGLS_IT1  3 291.4097

#body weight and IT not

#Option 2: LASSO----
#this implies also dividing the datset in build and test datasets.
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/

##Training and test datasets

require(caret)

## set the seed to make your partition reproductible
set.seed(123)
dummy=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-dummy, ]
bee_train <- bee_mean[dummy, ]

library(glmnet)
str(bee_train)
train_sparse <- sparse.model.matrix(~.,bee_train[c("Country","Latitude","Family","IT","Sex")])
test_sparse <- sparse.model.matrix(~.,bee_test[c("Region","Latitude","Family","IT","Sex")])
fit <- glmnet(train_sparse,bee_train$Spec.wgt)
pred <- predict(fit, test_sparse, type="class")
par(mfrow=c(1,1))
plot(fit, xvar = "lambda", label = TRUE)


# use cv.glmnet to find best lambda/penalty 
#- choosing small nfolds for cv due to… 
# s is the penalty parameter
cv <- cv.glmnet(train_sparse,bee_train$Spec.wgt,nfolds=3)
pred <- predict(fit, test_sparse,type="response", s=cv$lambda.min)
plot(cv, xvar = "lambda",label = TRUE)
plot(fit, xvar = "dev", label = TRUE)

coef.apprx = coef(fit, s = 0.5, exact = FALSE) 
coef.apprx
coef.exact = coef(fit, s = 0.5, exact = TRUE, x=x, y=y) 
coef.exact
cbind2(coef.exact, coef.apprx)

cvfit = cv.glmnet( train_sparse,bee_train$Spec.wgt, type.measure = "mse", nfolds = 20)
cvfit

