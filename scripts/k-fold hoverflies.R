#k-fold hoverflies
#libraries
library(foreign)
library(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
library(plyr)


#MODEL1 IT + Region + Sex
hov_model1<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),
                              holdoutpred=rep(0,nrow(hov_mean)),
                              MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),
                              MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),
                              DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))

for(i in 1:10){
  train=hov_model1[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model1[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Sex + Region, 
                            random = ~Species,
                            family = "gaussian", prior=prior1,
                            singular.ok=TRUE,data= train, 
                            verbose=FALSE, thin=Nthin, 
                            nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##IT + Sex + Sub*IT + Region
hov_model2<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),
                              holdoutpred=rep(0,nrow(hov_mean)),
                              MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),
                              MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),
                              DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model2[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model2[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Sex + Subfamily + Region + log(IT):Subfamily, 
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
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 3
##IT + Subfamily + Sex*IT
hov_model3<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model3[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model3[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Sex + Subfamily + log(IT):Sex, 
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
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 4 - IT + Sex
hov_model4<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model4[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model4[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Sex, 
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
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 5 IT + Region
hov_model5<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model5[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model5[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + Region, 
                            random = ~Species,
                            family = "gaussian", prior=prior1,
                            singular.ok=TRUE,data=train, 
                            verbose=FALSE, thin=Nthin, 
                            nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation,allow.new.levels=TRUE))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 6 IT + Subfamily + Region*IT
hov_model6<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model6[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model6[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Subfamily + Region + log(IT):Region, 
                            random = ~Species,
                            family = "gaussian", prior=prior1,
                            singular.ok=TRUE,data=train, 
                            verbose=FALSE, thin=Nthin, 
                            nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation,allow.new.levels=TRUE))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 7 IT + Subfamily + Region*IT
hov_model7<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model7[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model7[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT)  + 
                              Subfamily  + log(IT):Subfamily, 
                            random = ~Species,
                            family = "gaussian", prior=prior1,
                            singular.ok=TRUE,data=train, 
                            verbose=FALSE, thin=Nthin, 
                            nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation,allow.new.levels=TRUE))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model7[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##MODEL 8 IT
hov_model8<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),DIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:10){
  train=hov_model8[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model8[hov_fold$subsets[hov_fold$which == i], ]
  newlm=MCMCglmm.updateable(log(Spec.wgt) ~ log(IT), 
                            random = ~Species,
                            family = "gaussian", prior=prior1,
                            singular.ok=TRUE,data=train, 
                            verbose=FALSE, thin=Nthin, 
                            nitt=Nnitt, burnin=Nburnin)
  newpred=exp(predict(newlm,newdata=validation,allow.new.levels=TRUE))
  true=(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$holdoutpred <- newpred
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$RMSE=rmse
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$MSE=mse
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$MAE=mae
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$R2=R2
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$DIC=DIC(newlm)
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model8[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}
str(hov_model1)
Hov_K_sets=rbind(
  hov_model1%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model2%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model3%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model4%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model5%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model6%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model7%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T),
  hov_model8%>%dplyr::select(.,14:19)%>%map_dbl(median,na.rm=T))
Hov_K_sets

##Combine RMSEs
Hov_RMSE_sets=cbind(unique(hov_model1$RMSE),
                unique(hov_model2$RMSE),
                unique(hov_model3$RMSE),
                unique(hov_model4$RMSE),
                unique(hov_model5$RMSE),
                unique(hov_model6$RMSE),
                unique(hov_model7$RMSE),
                unique(hov_model8$RMSE))
colnames(Hov_RMSE_sets)=c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8")
Hov_RMSE_sets=as.data.frame(Hov_RMSE_sets)

#Box plots
Hov_RMSE=Hov_RMSE_sets%>%gather(.,1:8,key ="Model",value = "RMSE")%>%ggplot(aes(x=Model,y=RMSE,fill=Model))+geom_boxplot()+theme_bw()+ theme(aspect.ratio=1)
Hov_RMSE
