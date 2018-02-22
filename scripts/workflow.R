#workflow

###Issues
Need latitude for Spanish samples (Assume no pres.time)

#libraries
require(MuMIn)
require(lme4)
require(broom)

#read data (1 file)----

allo=read.csv(file="~/Dropbox/PhD/R/PollinateR/data/PredAlloPoll19218.csv")

str(allo)

#without Germany and Cane 1987
allo=allo[1:1247,]

#dummy variable for spain latitude
allo[269:344,c("Latitude")]=37.396355

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
Spec.wgt~IT+Latitude+Sex+BL+Col.method+Family+Region+Pres.time
+IT:Latitude+IT:Sex+IT:BL+IT:Col.method+IT:Family+IT:Region+IT:Pres.time
+(1|Species)

bee.full=lmer(Spec.wgt~IT+Latitude+Sex+BL+Col.method+Family+Region+Pres.time
              +IT:Latitude+IT:Sex+IT:BL+IT:Col.method+IT:Family+IT:Region+IT:Pres.time
              +(1|Species),REML=FALSE,data=bee)
summary(bee.full)

bee.full.res=lmer(bee$Spec.wgt~bee$IT+bee$Latitude+Sex+bee$BL+Col.method+Family+Region+Pres.time
              +bee$IT:Latitude+bee$IT:Sex+bee$IT:bee$BL+bee$IT:Col.method+bee$IT:Family+bee$IT:Region
              +bee$IT:Pres.time
              +(1|Species),REML=FALSE,data=bee_mean)

#2) Weight ~ IT * covariates [at species level with mean per sp]. Where cov are : Laitude, sex, body length + collection method + family + region
bee_mean=bee[!duplicated(bee[,c('Species')]),]
bee_mean$Spec.wgt=aggregate(bee$Spec.wgt~bee$Species,FUN="mean")[2]
bee_mean$IT=aggregate(bee$IT~bee$Species,FUN="mean")[2]
bee_mean$BL=aggregate(bee$BL~bee$Species,FUN="mean")[2]
bee_mean$Latitude=aggregate(bee$Latitude~bee$Species,FUN="mean")[2]
str(bee_mean)

is.na(bee$Latitude)

#Aggregate is ugly - merging



str(bee_mean)

#Option 1: dredge ()----

##Columns with NA's
unlist(lapply(bee, function(x) any(is.na(x))))

options(na.action = "na.fail") 

bee_dr=dredge(bee.full,beta="none",rank="AIC")


##perhaps subset here by delta/AIC etc
subset(bee_dr)

bee_dr_mods=get.models(bee_dr,method="REML",subset=TRUE)

#Retrieve the estimates (package broom)----

lapply(bee_dr_mods,function (x) tidy(x))

#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.


#Option 2: LASSO----
#this implies also dividing the datset in build and test datasets.
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/


