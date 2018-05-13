#libraries
require(foreign)
require(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
require(plyr)

gls1<-bee_phylo%>%mutate(Fold=rep(0,nrow(bee_phylo)),holdoutpred=rep(0,nrow(bee_phylo)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=gls1[fold_phy$subsets[fold_phy$which != i], ]
  validation=gls1[fold_phy$subsets[fold_phy$which == i], ]
  newlm=nlme::gls(log(Spec.wgt)~log(IT) * Region, method="ML",data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  gls1[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  gls1[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  gls1[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  gls1[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  gls1[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  gls1[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  gls1[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  gls1[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
gls1%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()

gls2<-bee_phylo%>%mutate(Fold=rep(0,nrow(bee_phylo)),holdoutpred=rep(0,nrow(bee_phylo)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=gls2[fold_phy$subsets[fold_phy$which != i], ]
  validation=gls2[fold_phy$subsets[fold_phy$which == i], ]
  newlm=nlme::gls(log(Spec.wgt)~log(IT) + Region, method="ML",data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  gls2[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  gls2[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  gls2[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  gls2[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  gls2[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  gls2[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  gls2[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  gls2[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
gls2%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()

gls3<-bee_phylo%>%mutate(Fold=rep(0,nrow(bee_phylo)),holdoutpred=rep(0,nrow(bee_phylo)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=gls3[fold_phy$subsets[fold_phy$which != i], ]
  validation=gls3[fold_phy$subsets[fold_phy$which == i], ]
  newlm=nlme::gls(log(Spec.wgt)~log(IT), method="ML",data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  gls3[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  gls3[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  gls3[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  gls3[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  gls3[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  gls3[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  gls3[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  gls3[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
gls3%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
