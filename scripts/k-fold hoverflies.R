#k-fold hoverflies
#libraries
library(foreign)
library(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
library(plyr)


hov_fold<-hov_mean%>%fold_cv(.,k=5)

hov_model1<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model1[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model1[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) + Sex  + Region+#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model1[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}


##IT + Sex
hov_model2<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model2[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model2[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) + Sex+#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model2[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##IT + Sex
hov_model3<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model3[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model3[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)*Sex + Region+#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model3[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##IT * Sex
hov_model4<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model4[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model4[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)*Sex +#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model4[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##IT + Subfamily
hov_model5<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model5[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model5[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) * Subfamily +#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model5[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

##IT + Subfamily
hov_model6<-hov_mean%>%mutate(Fold=rep(0,nrow(hov_mean)),holdoutpred=rep(0,nrow(hov_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=hov_model6[hov_fold$subsets[hov_fold$which != i], ]
  validation=hov_model6[hov_fold$subsets[hov_fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) +#Fixed
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
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
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$AIC=AIC(newlm)
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$BIC=BIC(newlm)
  hov_model6[hov_fold$subsets[hov_fold$which == i], ]$Fold=i
}

Hov_K_sets=rbind(
  hov_model1%>%select(.,16:21)%>%map_dbl(median,na.rm=T),
  hov_model2%>%select(.,16:21)%>%map_dbl(median,na.rm=T),
  hov_model3%>%select(.,16:21)%>%map_dbl(median,na.rm=T),
  hov_model4%>%select(.,16:21)%>%map_dbl(median,na.rm=T),
  hov_model5%>%select(.,16:21)%>%map_dbl(median,na.rm=T),
  hov_model6%>%select(.,16:21)%>%map_dbl(median,na.rm=T))
Hov_K_sets

##Combine RMSEs
Hov_RMSE_sets=cbind(unique(hov_model1$RMSE),
                unique(hov_model2$RMSE),
                unique(hov_model3$RMSE),
                unique(hov_model4$RMSE),
                unique(hov_model5$RMSE),
                unique(hov_model6$RMSE))
colnames(Hov_RMSE_sets)=c("LM1","LM2","LM3","LM4","LM5","LM6")
Hov_RMSE_sets=as.data.frame(Hov_RMSE_sets)

#Box plots
Hov_RMSE=Hov_RMSE_sets%>%gather(.,1:6,key ="Model",value = "RMSE")%>%ggplot(aes(x=Model,y=RMSE,fill=Model))+geom_boxplot()+theme_bw()+ theme(aspect.ratio=1)

