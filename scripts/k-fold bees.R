
##BEES##

#libraries
require(foreign)
require(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
require(plyr)
require(cvTools)
require(caret)

##NEW CV folds
fold <- createFolds(bee_mean$Region, k=10)
names(fold)=c('1','2','3','4','5','6','7','8','9','10')
fold=ldply(fold, data.frame)
colnames(fold)=c("which","subsets")

# set some constant values
prior1 <- list(R = list(V=1, nu=0.002),  G = list(G1 = list(V=1,nu=0.002)))
Nnitt <- 10000 # default is 13000
Nburnin <- 1000 # default is 3000
Nthin <- 100 # thinning of 1000 needed for autocorrelation in VCV

##Model1
model1<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),
                          MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),
                          R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
  for(i in 1:10){
    train=model1[bfold$subsets[bfold$which != i], ]
    validation=model1[bfold$subsets[bfold$which == i], ]
    newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT)  + Family + 
                     Sex + Region + #fix factors
                     log(IT):Family + log(IT):Region + log(IT):Sex, 
                   random = ~Species,prior=prior1,
                   family = "gaussian", 
                   singular.ok=TRUE,data=train, 
                   verbose=FALSE, thin=Nthin, 
                   nitt=Nnitt, burnin=Nburnin)
    newpred=exp(predict(newlm,newdata=validation))
    true=(validation$Spec.wgt)
    error=(true-newpred)
    rmse=sqrt(mean(error^2))
    mse=mean((newpred-true)^2)
    R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
    mae=mean(abs(error))
    model1[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
    model1[bfold$subsets[bfold$which == i], ]$RMSE=rmse
    model1[bfold$subsets[bfold$which == i], ]$MSE=mse
    model1[bfold$subsets[bfold$which == i], ]$MAE=mae
    model1[bfold$subsets[bfold$which == i], ]$R2=R2
    model1[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
    model1[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
    model1[bfold$subsets[bfold$which == i], ]$Fold=i
  }
model1%>%select(.,14:19)%>%map_dbl(mean,na.rm=T)

##Model 2
model2<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model2[bfold$subsets[bfold$which != i], ]
  validation=model2[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + 
                             Sex + Region + #fix factors
                             + log(IT):Sex, 
                           random = ~Species,prior=prior1,
                           family = "gaussian", 
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model2[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model2[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model2[bfold$subsets[bfold$which == i], ]$MSE=mse
  model2[bfold$subsets[bfold$which == i], ]$MAE=mae
  model2[bfold$subsets[bfold$which == i], ]$R2=R2
  model2[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model2[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model2[bfold$subsets[bfold$which == i], ]$Fold=i
}
model2%>%select(.,14:19)%>%map_dbl(median,na.rm=T)


##MOdel 3
model3<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model3[bfold$subsets[bfold$which != i], ]
  validation=model3[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Family+
                              Region + #fix factors
                             + log(IT):Family + log(IT):Region , 
                           random = ~Species,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model3[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model3[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model3[bfold$subsets[bfold$which == i], ]$MSE=mse
  model3[bfold$subsets[bfold$which == i], ]$MAE=mae
  model3[bfold$subsets[bfold$which == i], ]$R2=R2
  model3[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model3[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model3[bfold$subsets[bfold$which == i], ]$Fold=i
}
model3%>%select(.,14:19)%>%map_dbl(median,na.rm=T)


##Model 4
model4<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model4[bfold$subsets[bfold$which != i], ]
  validation=model4[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Family+
                             Sex + #fix factors
                             + log(IT):Family + log(IT):Sex , 
                           random = ~Species,prior=prior1,
                           family = "gaussian", 
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model4[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model4[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model4[bfold$subsets[bfold$which == i], ]$MSE=mse
  model4[bfold$subsets[bfold$which == i], ]$MAE=mae
  model4[bfold$subsets[bfold$which == i], ]$R2=R2
  model4[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model4[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model4[bfold$subsets[bfold$which == i], ]$Fold=i
}
model4%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

##Model 5
model5<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model5[bfold$subsets[bfold$which != i], ]
  validation=model5[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Sex+
                             log(IT):Sex , 
                           random = ~Species,prior=prior1,
                           family = "gaussian", 
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model5[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model5[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model5[bfold$subsets[bfold$which == i], ]$MSE=mse
  model5[bfold$subsets[bfold$which == i], ]$MAE=mae
  model5[bfold$subsets[bfold$which == i], ]$R2=R2
  model5[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model5[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model5[bfold$subsets[bfold$which == i], ]$Fold=i
}
model5%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

##Model 6
model6<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model6[bfold$subsets[bfold$which != i], ]
  validation=model6[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + 
                             Region , 
                           random = ~Species,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model6[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model6[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model6[bfold$subsets[bfold$which == i], ]$MSE=mse
  model6[bfold$subsets[bfold$which == i], ]$MAE=mae
  model6[bfold$subsets[bfold$which == i], ]$R2=R2
  model6[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model6[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model6[bfold$subsets[bfold$which == i], ]$Fold=i
}
model6%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

##Model 7
model7<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model7[bfold$subsets[bfold$which != i], ]
  validation=model7[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + 
                             Family, 
                           random = ~Species,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model7[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model7[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model7[bfold$subsets[bfold$which == i], ]$MSE=mse
  model7[bfold$subsets[bfold$which == i], ]$MAE=mae
  model7[bfold$subsets[bfold$which == i], ]$R2=R2
  model7[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model7[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model7[bfold$subsets[bfold$which == i], ]$Fold=i
}
model7%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

##Model 8
model8<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=model8[bfold$subsets[bfold$which != i], ]
  validation=model8[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + 
                             Family, 
                           random = ~Species,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train, 
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model8[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  model8[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  model8[bfold$subsets[bfold$which == i], ]$MSE=mse
  model8[bfold$subsets[bfold$which == i], ]$MAE=mae
  model8[bfold$subsets[bfold$which == i], ]$R2=R2
  model8[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  model8[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  model8[bfold$subsets[bfold$which == i], ]$Fold=i
}
model8%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

#MODEL9 PGLMM1
pglmm1<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),
                            holdoutpred=rep(0,nrow(bee_mean)),
                            MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),
                            MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),
                            DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=pglmm1[bfold$subsets[bfold$which != i], ]
  validation=pglmm1[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Region + Sex+
                             log(IT):Region+log(IT):Sex, 
                           random = ~animal,pedigree = bee.tree,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train,
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pglmm1[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  pglmm1[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  pglmm1[bfold$subsets[bfold$which == i], ]$MSE=mse
  pglmm1[bfold$subsets[bfold$which == i], ]$MAE=mae
  pglmm1[bfold$subsets[bfold$which == i], ]$R2=R2
  pglmm1[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  pglmm1[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  pglmm1[bfold$subsets[bfold$which == i], ]$Fold=i
}
pglmm1%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
pglmm1%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

##JUST IT + Region
pglmm2<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=pglmm2[bfold$subsets[bfold$which != i], ]
  validation=pglmm2[bfold$subsets[bfold$which == i], ]
  
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Sex+
                             log(IT):Sex, 
                           random = ~animal,pedigree = bee.tree,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train,
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pglmm2[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  pglmm2[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  pglmm2[bfold$subsets[bfold$which == i], ]$MSE=mse
  pglmm2[bfold$subsets[bfold$which == i], ]$MAE=mae
  pglmm2[bfold$subsets[bfold$which == i], ]$R2=R2
  pglmm2[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  pglmm2[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  pglmm2[bfold$subsets[bfold$which == i], ]$Fold=i
}
pglmm2%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
pglmm2%>%select(.,14:19)%>%map_dbl(median,na.rm=T)
table(pglmm2$Fold)

##JUST IT
pglmm3<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=pglmm3[bfold$subsets[bfold$which != i], ]
  validation=pglmm3[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT) + Region+
                             log(IT):Region, 
                           random = ~animal,pedigree = bee.tree,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train,
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pglmm3[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  pglmm3[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  pglmm3[bfold$subsets[bfold$which == i], ]$MSE=mse
  pglmm3[bfold$subsets[bfold$which == i], ]$MAE=mae
  pglmm3[bfold$subsets[bfold$which == i], ]$R2=R2
  pglmm3[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  pglmm3[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  pglmm3[bfold$subsets[bfold$which == i], ]$Fold=i
}
pglmm3%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()


##pglmm4
pglmm4<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=pglmm4[bfold$subsets[bfold$which != i], ]
  validation=pglmm4[bfold$subsets[bfold$which == i], ]
  newlm=MCMCglmm::MCMCglmm(log(Spec.wgt) ~ log(IT), 
                           random = ~animal,pedigree = bee.tree,
                           family = "gaussian", prior=prior1,
                           singular.ok=TRUE,data=train,
                           verbose=FALSE, thin=Nthin, 
                           nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pglmm4[bfold$subsets[bfold$which == i], ]$holdoutpred <- newpred
  pglmm4[bfold$subsets[bfold$which == i], ]$RMSE=rmse
  pglmm4[bfold$subsets[bfold$which == i], ]$MSE=mse
  pglmm4[bfold$subsets[bfold$which == i], ]$MAE=mae
  pglmm4[bfold$subsets[bfold$which == i], ]$R2=R2
  pglmm4[bfold$subsets[bfold$which == i], ]$DIC=DIC(newlm)
  pglmm4[bfold$subsets[bfold$which == i], ]$BIC=BIC(newlm)
  pglmm4[bfold$subsets[bfold$which == i], ]$Fold=i
}
pglmm4%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
pglmm4%>%select(.,14:19)%>%map_dbl(median,na.rm=T)

K_sets=rbind(
model1%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model2%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model3%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model4%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model5%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model6%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model7%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
model8%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
pglmm1%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
pglmm2%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
pglmm3%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
pglmm4%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T))
str(K_sets)
rownames(K_sets)=c("lme1","lme2","lme3","lme4","lme5","lme6","lme7","lme8","pglmm1","pglmm2","pglmm3","pglmm4")
K_sets=as.data.frame(K_sets)

##Combine RMSEs
RMSE_sets=cbind(unique(model1$RMSE),
unique(model2$RMSE),
unique(model3$RMSE),
unique(model4$RMSE),
unique(model5$RMSE),
unique(model6$RMSE),
unique(model7$RMSE),
unique(model8$RMSE),
unique(pglmm1$RMSE),
unique(pglmm2$RMSE),
unique(pglmm3$RMSE),
unique(pglmm4$RMSE))
colnames(RMSE_sets)=c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8","PG1","PG2","PG3","PG4")
RMSE_sets=as.data.frame(RMSE_sets)

#Box plots
bee_RMSE=RMSE_sets%>%gather(.,c("LM1","PG1"),key ="Model",value = "RMSE")%>%ggplot(aes(x=Model,y=RMSE,fill=Model))+geom_boxplot()+theme_bw()+ theme(aspect.ratio=1)
bee_RMSE

