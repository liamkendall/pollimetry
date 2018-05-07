
#We implemented k-fold cross validation to test overall model performance and prediction error.
#Despite being a ubiquitous method in social and medical sciences, 
#these approaches are underutilised in ecology despite in distribution modeling 
#(eg..Wenger and Olden 2012, Boria et al 2014)
#Our mean dataset was divided into five equal set containing a random subset of species.
#Each model was then evaluated iteratively upon each k-1 set (training set), and then compared against the 
#k set (test set). This was done repeatedly so each set was both the test set and contained within the training sets.
#We then assessed model perfomrance on the basis of the average/median root-mean square error (RMSE) across five sets, where
#RMSE is the square error between the training (k-1) sets and test set.

INSERT k-fold reference

http://rpubs.com/ledongnhatnam/241926
#Followed this for loop

library(foreign)
library(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
library(plyr)

fold_cv=function(data,k){
  folds=cvTools::cvFolds(nrow(data),K=k)
  invisible(folds)
}

set.seed(123)
fold<-bee_mean%>%fold_cv(.,k=5)

str(fold)

##WITH REGION
temp<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
  for(i in 1:5){
    train=temp[fold$subsets[fold$which != i], ]
    validation=temp[fold$subsets[fold$which == i], ]
    newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)  + Family + Sex  + Region+#fix factors
                       log(IT):Family  + log(IT):Sex + log(IT):Region+ #interactions
                       (1|Measurement)+(1|Species),data=train)
    newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
    true=log(validation$Spec.wgt)
    error=(true-newpred)
    rmse=sqrt(mean(error^2))
    mse=mean((newpred-true)^2)
    R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
    mae=mean(abs(error))
    temp[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
    temp[fold$subsets[fold$which == i], ]$RMSE=rmse
    temp[fold$subsets[fold$which == i], ]$MSE=mse
    temp[fold$subsets[fold$which == i], ]$MAE=mae
    temp[fold$subsets[fold$which == i], ]$R2=R2
    temp[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
    temp[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
    temp[fold$subsets[fold$which == i], ]$Fold=i
  }
temp%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
temp%>%select(.,16:21)%>%map_dbl(median,na.rm=T)

##WITHout sex
temp2<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=temp2[fold$subsets[fold$which != i], ]
  validation=temp2[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)  + Family + Region  + #fix factors
                     log(IT):Family  + log(IT):Region + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp2[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  temp2[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp2[fold$subsets[fold$which == i], ]$MSE=mse
  temp2[fold$subsets[fold$which == i], ]$MAE=mae
  temp2[fold$subsets[fold$which == i], ]$R2=R2
  temp2[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  temp2[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp2[fold$subsets[fold$which == i], ]$Fold=i
}
temp2%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
temp2%>%select(.,15:20)%>%map_dbl(median,na.rm=T)
fold$subsets

##Just IT
temp3<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=temp3[fold$subsets[fold$which != i], ]
  validation=temp3[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp3[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  temp3[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp3[fold$subsets[fold$which == i], ]$MSE=mse
  temp3[fold$subsets[fold$which == i], ]$MAE=mae
  temp3[fold$subsets[fold$which == i], ]$R2=R2
  temp3[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  temp3[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp3[fold$subsets[fold$which == i], ]$Fold=i
}
temp3%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
temp3%>%select(.,16:21)%>%map_dbl(median,na.rm=T)

##PGLS - not done yet - not working ugh
temp4<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=temp4[fold$subsets[fold$which != i], ]
  validation=temp4[fold$subsets[fold$which == i], ]
  newlm=nlme::gls(log(Spec.wgt)~log(IT),
                  correlation=Bee_vcv, method="ML",data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp4[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  temp4[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp4[fold$subsets[fold$which == i], ]$MSE=mse
  temp4[fold$subsets[fold$which == i], ]$MAE=mae
  temp4[fold$subsets[fold$which == i], ]$R2=R2
  temp4[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  temp4[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp4[fold$subsets[fold$which == i], ]$Fold=i
}
temp4%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
temp4%>%select(.,16:21)%>%map_dbl(median,na.rm=T)

K_sets=cbind(unique(temp$RMSE),
unique(temp2$RMSE),
unique(temp3$RMSE))

colnames(K_sets)=c("1","2","3")
par(pty="s")
boxplot(K_sets,ylab="RMSE")
K_sets=as.data.frame(K_sets)

K_sets%>%gather(.,1:3,key ="Model",value = "Value")%>%ggplot(aes(x=Model,y=Value,fill=Model))+geom_boxplot()+theme_bw()

