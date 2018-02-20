#workflow

###Issues
Need latitude for Spanish samples (Assume no pres.time)

#libraries
require(MuMIn)
require(lme4)
require(broom)
require(glmnet)
options(stringsAsFactors = FALSE)
#read data (1 file)----

allo=read.csv(file="~/Dropbox/PhD/R/PollinateR/data/PredAlloPoll20218.csv")
str(allo)

allo[,24:25]=log(allo[,24:25])

#without Germany and Cane 1987
allo=allo[1:1247,]

#dummy variable for spain latitude
allo[269:344,c("Latitude")]=rnorm(mean=37.396355,sd=0.2,n=76)
?rnorm
#split to bees and hoverflies
allo_split=split(allo,allo$Taxa)
bee=allo_split[[1]]
fly=allo_split[[2]]


#Remove one 
bee=bee[-454,]


##Latitude (*-1 for australasia)
bee_2=split(bee,bee$Region)
bee_2$Australasia$Latitude=bee_2$Australasia$Latitude*-1
bee=rbind.data.frame(bee_2$Australasia,bee_2$Europe)




boxplot(bee$Latitude~bee$Region)

#One check how many specimens needed to stabilize mean and variance----

#subset to species with >5 specimens

subset(bee,)

#Make a full model IT-length ----

#1) Weight ~ IT * covariates (species). where cov are : Laitude, sex, body length + collection method + family + region
# IT + cov 1 + cov2 +  IT:cov1 + IT:cov2 + ...

#model formula
Spec.wgt~IT+Latitude+Sex+Col.method+Family+Region+Pres.time
+IT:Latitude+IT:Sex+IT:BL+IT:Col.method+IT:Family+IT:Region+IT:Pres.time
+(1|Species)

#
options(na.action = "na.omit") 

bee.full=lmer(Spec.wgt~IT+Latitude+Sex+Col.method+Family+Region+Pres.time
              +IT:Latitude+IT:Sex+IT:Region+
                IT:Col.method+IT:Pres.time+
                IT:Family+
              (1|Species),REML=FALSE,data=bee)

summary(bee.full)

bee.full.res=lm(Spec.wgt~IT+Latitude+Sex+Col.method+Family+Region+Pres.time
                  +IT:Latitude+IT:Sex+IT:Region+
                    IT:Col.method+IT:Pres.time+
                    IT:Family,data=bee_mean2)

#2) Weight ~ IT * covariates [at species level with mean per sp]. Where cov are : Laitude, sex, body length + collection method + family + region
bee_mean=bee[!duplicated(bee[,c('Species')]),]
rownames(bee_mean)=bee_mean$Species
bee_mean$Spec.wgt=as.numeric(unlist(aggregate(bee$Spec.wgt~bee$Species,FUN="mean")[2]))
bee_mean$WG.sd=as.numeric(unlist(aggregate(bee$Spec.wgt~bee$Species,FUN="sd")[2]))
bee_mean$IT=as.numeric(unlist(aggregate(bee$IT~bee$Species,FUN="mean")[2]))
bee_mean$IT.sd=as.numeric(unlist(aggregate(bee$IT~bee$Species,FUN="sd")[2]))
bee_mean$BL=as.numeric(unlist(aggregate(bee$BL~bee$Species,FUN="mean")[2]))
bee_mean$BL.sd=as.numeric(unlist(aggregate(bee$BL~bee$Species,FUN="mean")[2]))

bee_mean$Latitude=as.numeric(unlist(aggregate(bee$Latitude~bee$Species,FUN="mean")[2]))

#Option 1: dredge ()----
##I think AICc is good, because simple/parsimonous = better

##Columns with NA's
unlist(lapply(bee_mean, function(x) any(is.na(x))))

options(na.action = "na.fail") 


bee_dr=dredge(bee.full,beta="sd",rank="AICc")
bee_dr

bee_dr_res=dredge(bee.full.res,beta="sd",rank="AICc")
bee_dr_res

bee_dr_mods=get.models(bee_dr[1:10],method="REML",subset=TRUE)

bee_dr_res_mods=get.models(bee_dr_res[1:10],subset=TRUE)


#Retrieve the estimates (package broom)----

bee_coef=lapply(bee_dr_mods,function (x) tidy(x))

bee_mean_coef=lapply(bee_dr_res_mods,function (x) tidy(x))

#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

Bee_tree=as.phylo(~Family/Subfamily/Genus/Species, data=bee_mean)

#gives the tree random branch lengths (will try a bunch to see how it impacts the results)
Bee_tree.rand <- compute.brlen(Bee_tree,method="Grafen")

summary(Bee_tree.rand)

#first compute correlation matrix from tree
Bee_vcv=corPagel (0.5,Bee_tree.rand,fixed=FALSE)

Bee_GLS_IT1<- gls(Spec.wgt~IT, data=bee_mean, method="ML")
summary(gls_BeeIT1)

Bee_PGLS_IT1 = gls(Spec.wgt~IT, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_IT1)

AIC(Bee_GLS_IT1,Bee_PGLS_IT1)

plot(Spec.wgt~IT,bee_mean)
abline(Bee_GLS_IT1)
abline(Bee_PGLS_IT1)

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

#1st model
bee_dr_1=bee_dr_mods$'2343'

#without sex
bee_mean_coef$`271`

#with sex
bee_mean_coef$`4431`

bee_dr_test <- predict(bee_dr_1, bee_test)
bee_dr_test2 <- predict(bee_dr_res_mods$`271`, bee_test)
bee_dr_test3 <- predict(bee_dr_res_mods$`4431`, bee_test)




actuals_pred <- data.frame(cbind(actuals=bee_test$Spec.wgt, predicteds=bee_dr_test))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 48.38%, mean absolute percentage deviation

as.data.frame(table(unique(bee)$Species))
subset(bee, Species > 5)
tt <- table(bee$Species)
bee_10 <- subset(bee, Species %in% names(tt[tt < 3]))


bee_10$Spec.wgt=as.numeric(unlist(aggregate(bee_10$Spec.wgt~bee_10$Species,FUN="mean")[2]))
bee_10$WG.sd=as.numeric(unlist(aggregate(bee_10$Spec.wgt~bee_10$Species,FUN="sd")[2]))
bee_10$IT=as.numeric(unlist(aggregate(bee_10$IT~bee_10$Species,FUN="mean")[2]))
bee_10$IT.sd=as.numeric(unlist(aggregate(bee_10$IT~bee_10$Species,FUN="sd")[2]))
bee_10$BL=as.numeric(unlist(aggregate(bee_10$BL~bee_10$Species,FUN="mean")[2]))
bee_10$BL.sd=as.numeric(unlist(aggregate(bee_10$BL~bee_10$Species,FUN="mean")[2]))



