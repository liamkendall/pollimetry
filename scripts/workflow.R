#workflow


#read data (1 file)----



#One check how many specimens needed to stabilize mean and variance----


#Make a full model IT-length ----

#1) Weigth ~ IT * covariates (species). where cov are : Laitude, sex, body length + collection method + family + region
# IT + cov 1 + cov2 +  IT:cov1 + IT:cov2 + ...

#2) Weigth ~ IT * covariates [at species level with mean per sp]. Where cov are : Laitude, sex, body length + collection method + family + region

#Option 1: dredge ()----


#Retrieve the estimates (package broom)----


#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.


#Option 2: LASSO----
#this implies also dividing the datset in build and test datasets.
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/


