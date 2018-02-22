#workflow

###Issues
#Need latitude for Spanish samples (Assume no pres.time)

#libraries
require(MuMIn)
require(lme4)
library(nlme)
require(broom)
require(glmnet)
library(ape)

options(stringsAsFactors = TRUE)

#read data (1 file)----

all <- read.csv(file="data/PredAlloPoll20218.csv")
allo=allo[,1:26]
str(allo)

##
all - untouched - no transformations


##log Weight and IT
allo[,23:24] <- log(allo[,23:24])

#without Germany and Cane 1987
allo <- allo[1:1247,]

allo=allo[-624,]

#dummy variable for spain latitude
allo[269:344,c("Latitude")] <- 37.396355



#split to bees and hoverflies
allo_split=split(allo,allo$Taxa)
bee=allo_split[[1]]
fly=allo_split[[2]]

##Diagnostic plots
plot(Spec.wgt~IT,bee)
qqplot(Spec.wgt~IT,bee)
?qqnorm
#Remove one 



##Latitude (*-1 for australasia)
bee_2=split(bee,bee$Region)
bee_2$Australasia$Latitude=bee_2$Australasia$Latitude*-1
bee=rbind.data.frame(bee_2$Australasia,bee_2$Europe)
#Maybe not? Check.

boxplot(bee$Latitude~bee$Region)

#One check how many specimens needed to stabilize mean and variance----

#############

#Make a full model IT-length ----

#1) Weight ~ IT * covariates (species). where cov are : Laitude, sex, collection method + family + region
# IT + cov 1 + cov2 +  IT:cov1 + IT:cov2 + ...

#Full model
options(na.action = "na.omit") 

bee.full=lmer(Spec.wgt ~ IT + Latitude + Sex + Col.method + Family + Region + Pres.time #fix
              + IT:Latitude + IT:Sex + IT:Region + #interactions
                IT:Col.method + IT:Pres.time +
                IT:Family +
              (1|Species),REML=FALSE,data=bee)

summary(bee.full)

#2) Weight ~ IT * covariates [at species level with mean per sp]. Where cov are : Laitude, sex, body length + collection method + family + region
#bee_mean=bee[!duplicated(bee[,c('Species','Sex','Col.method')]),]
#str(bee_mean)

#rownames(bee_mean)=bee_mean$Species

bee_mean=aggregate(Latitude~Family+Subfamily+Genus+Species+Sex+Col.method,bee,mean)

bee_mean$Region=as.numeric(unlist(aggregate(Region~Species+Sex+Col.method,bee,unique)[4]))
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Species+Sex+Col.method,bee,mean)[4]))
bee_mean$Wgt.SD=as.numeric(unlist(aggregate(Spec.wgt~Species+Sex+Col.method,bee,sd)[4]))
bee_mean$IT=as.numeric(unlist(aggregate(IT~Species+Sex+Col.method,bee,mean)[4]))
bee_mean$IT.SD=as.numeric(unlist(aggregate(IT~Species+Sex+Col.method,bee,sd)[4]))
bee_mean$BL=as.numeric(unlist(aggregate(BL~Species+Sex+Col.method,bee,mean)[4]))
bee_mean$BL.SD=as.numeric(unlist(aggregate(BL~Species+Sex+Col.method,bee,sd)[4]))
str(bee_mean)
plot(bee_mean$IT~bee_mean$Spec.wgt,col=bee_mean$Sex)

#Sex... ignore and pool or have a mean for female and a mean for male - DONE.

#Same for method - DONE. I would ignore pres time.

#Check all species have only one region.- DONE
table(bee$Species,bee$Region)

#Duplicates - species names in dataframe - FIXED
#mellifera, cingulata, bicolor

#Maybe drop covariates not relevant in the first analysis?
#took out pres. time

bee.full.res=lm(Spec.wgt~IT+Latitude+Sex+Col.method+Family+Region
                +IT:Latitude+IT:Sex+IT:Region+
                  IT:Col.method+
                  IT:Family,data=bee_mean)


#Option 1: dredge ()----
##I think AICc is good, because simple/parsimonous = better

##Columns with NA's
unlist(lapply(bee, function(x) any(is.na(x))))

options(na.action = "na.fail")

bee_dr=dredge(bee.full,beta="none",rank="AIC",trace=100,fixed=c("IT")) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr)
vcov(lmer(Spec.wgt ~ Col.method + Family + Latitude + Pres.time + Region +  
          IT + (1 | Species) + Pres.time:IT + Region:IT,bee))

bee_dr_res=dredge(bee.full.res,beta="none",rank="AIC",fixed=c("IT"))

bee_dr_mods=get.models(bee_dr[1:10],REML=TRUE,subset=TRUE)

bee_dr_res_mods=get.models(bee_dr_res[1:10],subset=TRUE)
bee_dr_res_mods[1]

##Two-step
#Hypothesised variables - predictive: IT, Region and Family?
bee_dr_pred=dredge(bee.full,beta="none",rank="AIC",
                   trace=100,fixed=c("IT","Region","Family")) #think about "sd" and "AICc". AIC show same pattern.
head(bee_dr_pred)
bee_dr_pred[6]

#Retrieve the estimates (package broom)----

bee_coef=lapply(bee_dr_mods[1],function (x) tidy(x))

bee_mean_coef=lapply(bee_dr_res_mods[1],function (x) tidy(x))

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
#lambda 0.06135144 

AIC(Bee_GLS_WGT1,Bee_PGLS_WGT1) #Phylo signal moderate.
#df      AIC
#Bee_GLS_WGT1   2 738.2531
#Bee_PGLS_WGT1  3 732.5889

##IT span
Bee_GLS_IT1<- gls(IT~1, data=bee_mean, method="ML")
summary(Bee_GLS_IT1)

Bee_PGLS_IT1 = gls(IT~1, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_IT1)

#lambda -0.02137243

AIC(Bee_GLS_IT1,Bee_PGLS_IT1) #Really low phylo signal... good.
#df      AIC
#Bee_GLS_IT1   2 199.0897
#Bee_PGLS_IT1  3 200.2350


#Option 2: LASSO----
#this implies also dividing the datset in build and test datasets.
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/

##Training and test datasets

require(caret)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(bee)), size = floor(0.8 * nrow(bee)))
bee_train <- bee[train_ind, ]
bee_test <- bee[-train_ind, ]

install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
str(bee_train)
train_sparse <- sparse.model.matrix(~.,bee_train[c("Region","Latitude","Pres.time","Col.method","Family","IT")])
test_sparse <- sparse.model.matrix(~.,bee_test[c("Region","Latitude","Pres.time","Col.method","Family","IT")])
fit <- glmnet(train_sparse,bee_train$Spec.wgt)
pred <- predict(fit, test_sparse, type="class")
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

?coef
cvfit = cv.glmnet( train_sparse,bee_train$Spec.wgt, type.measure = "mse", nfolds = 20)
cvfit

####Dummy model for package function

allo_IT_Family=lmer(Spec.wgt ~IT + Family +
                (1|Species),REML=FALSE,data=bee)
summary()

allo_coefs=tidy(allo_IT_Family)
allo_coefs


#Two step

#check co-variates - call predict

Using dummy dataframe = known IT and weight
#Use test data frame

bee_test
predict(bee_dr_mods[[1]])
plot(bee_test$Spec.wgt~bee_test$IT)

fitted(bee_dr_mods[[1]])/predict(bee_dr_mods[[1]], newdata=bee_test)

predict(bee_dr_mods[[2]], newdata=bee_test)
predict(bee_dr_mods[[3]], newdata=bee_test)
predict(bee_dr_mods[[4]], newdata=bee_test)
predict(bee_dr_mods[[5]], newdata=bee_test)



