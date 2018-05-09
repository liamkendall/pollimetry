
#We implemented k-fold cross validation to test overall model performance and prediction error.
#Despite being a ubiquitous method in social and medical sciences, 
#these approaches are underutilised in ecology despite in distribution modeling 
#(eg..Wenger and Olden 2012, Boria et al 2014)
#Our mean dataset was divided into five equal set containing a random subset of species.
#Each model was then evaluated iteratively upon each k-1 set (training set), and then compared against the 
#k set (test set). This was done repeatedly so each set was both the test set and contained within the training sets.
#We then assessed model perfomrance on the basis of the average/median root-mean square error (RMSE) across five sets, where
#RMSE is the square error between the training (k-1) sets and test set.

http://rpubs.com/ledongnhatnam/241926
#Followed this for loop

##BEEESS##

#libraries
library(foreign)
library(tidyverse)
require(compiler)
require(parallel)
require(boot)
require(lme4)
library(plyr)

##Create fold function for LME and PGLS
set.seed(123)
fold_cv=function(data,k){
  folds=cvTools::cvFolds(nrow(data),K=k)
  invisible(folds)
}

fold<-bee_mean%>%fold_cv(.,k=5)

##WITH REGION
model1<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
  for(i in 1:5){
    train=model1[fold$subsets[fold$which != i], ]
    validation=model1[fold$subsets[fold$which == i], ]
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
    model1[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
    model1[fold$subsets[fold$which == i], ]$RMSE=rmse
    model1[fold$subsets[fold$which == i], ]$MSE=mse
    model1[fold$subsets[fold$which == i], ]$MAE=mae
    model1[fold$subsets[fold$which == i], ]$R2=R2
    model1[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
    model1[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
    model1[fold$subsets[fold$which == i], ]$Fold=i
  }
model1%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model1%>%select(.,15:20)%>%map_dbl(median,na.rm=T)

##Without sex
model2<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model2[fold$subsets[fold$which != i], ]
  validation=model2[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)  + Family + Region  + Sex+ #fix factors
                     log(IT):Family  + log(IT):Region + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model2[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  model2[fold$subsets[fold$which == i], ]$RMSE=rmse
  model2[fold$subsets[fold$which == i], ]$MSE=mse
  model2[fold$subsets[fold$which == i], ]$MAE=mae
  model2[fold$subsets[fold$which == i], ]$R2=R2
  model2[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  model2[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  model2[fold$subsets[fold$which == i], ]$Fold=i
}
model2%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model2%>%select(.,15:20)%>%map_dbl(median,na.rm=T)
fold$subsets

##Without sex
model3<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model3[fold$subsets[fold$which != i], ]
  validation=model3[fold$subsets[fold$which == i], ]
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
  model3[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  model3[fold$subsets[fold$which == i], ]$RMSE=rmse
  model3[fold$subsets[fold$which == i], ]$MSE=mse
  model3[fold$subsets[fold$which == i], ]$MAE=mae
  model3[fold$subsets[fold$which == i], ]$R2=R2
  model3[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  model3[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  model3[fold$subsets[fold$which == i], ]$Fold=i
}
model3%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model3%>%select(.,15:20)%>%map_dbl(median,na.rm=T)


##Without region
model4<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model4[fold$subsets[fold$which != i], ]
  validation=model4[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)  + Family + Sex + #fix factors
                     log(IT):Family + log(IT):Sex + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model4[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  model4[fold$subsets[fold$which == i], ]$RMSE=rmse
  model4[fold$subsets[fold$which == i], ]$MSE=mse
  model4[fold$subsets[fold$which == i], ]$MAE=mae
  model4[fold$subsets[fold$which == i], ]$R2=R2
  model4[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  model4[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  model4[fold$subsets[fold$which == i], ]$Fold=i
}
model4%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model4%>%select(.,15:20)%>%map_dbl(median,na.rm=T)

##Without region and sex
model5<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model5[fold$subsets[fold$which != i], ]
  validation=model5[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT)  + Family  + #fix factors
                     log(IT):Family + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model5[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  model5[fold$subsets[fold$which == i], ]$RMSE=rmse
  model5[fold$subsets[fold$which == i], ]$MSE=mse
  model5[fold$subsets[fold$which == i], ]$MAE=mae
  model5[fold$subsets[fold$which == i], ]$R2=R2
  model5[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  model5[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  model5[fold$subsets[fold$which == i], ]$Fold=i
}
model5%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model5%>%select(.,15:20)%>%map_dbl(median,na.rm=T)

##Just IT
model6<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model6[fold$subsets[fold$which != i], ]
  validation=model6[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) + #interactions
                     (1|Measurement)+(1|Species),data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  model6[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  model6[fold$subsets[fold$which == i], ]$RMSE=rmse
  model6[fold$subsets[fold$which == i], ]$MSE=mse
  model6[fold$subsets[fold$which == i], ]$MAE=mae
  model6[fold$subsets[fold$which == i], ]$R2=R2
  model6[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  model6[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  model6[fold$subsets[fold$which == i], ]$Fold=i
}
model6%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
model6%>%select(.,15:20)%>%map_dbl(median,na.rm=T)

##PGLS - WORKING!!
#Set up folds
set.seed(123)
fold_phy<-bee_phylo_2%>%fold_cv(.,k=5)

pgls1<-bee_phylo_2%>%mutate(Fold=rep(0,nrow(bee_phylo_2)),holdoutpred=rep(0,nrow(bee_phylo_2)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=pgls1[fold_phy$subsets[fold_phy$which != i], ]
  validation=pgls1[fold_phy$subsets[fold_phy$which == i], ]
  tree=ape::drop.tip(bee.phy, setdiff(bee.phy$tip.label,train$Genus))
  tree=phytools::genus.to.species.tree(tree, species=train$Species)
  train_vcv=ape::corPagel(value=0.5,phy=tree,fixed=FALSE)
  
  newlm=nlme::gls(log(Spec.wgt)~log(IT)*Region,
                  correlation=train_vcv, method="ML",data=train)
  
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  pgls1[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
pgls1%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
pgls1%>%select(.,15:20)%>%map_dbl(median,na.rm=T)

##JUST IT + Region
pgls2<-bee_phylo_2%>%mutate(Fold=rep(0,nrow(bee_phylo_2)),holdoutpred=rep(0,nrow(bee_phylo_2)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=pgls2[fold_phy$subsets[fold_phy$which != i], ]
  validation=pgls2[fold_phy$subsets[fold_phy$which == i], ]
  tree=ape::drop.tip(bee.phy, setdiff(bee.phy$tip.label,train$Genus))
  tree=phytools::genus.to.species.tree(tree, species=train$Species)
  train_vcv=ape::corPagel(value=0.5,phy=tree,fixed=FALSE)
  newlm=nlme::gls(log(Spec.wgt)~log(IT) + Region,
                  correlation=train_vcv, method="ML",data=train)
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  pgls2[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
pgls2%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()
pgls2%>%select(.,15:20)%>%map_dbl(median,na.rm=T)


##JUST IT
pgls3<-bee_phylo_2%>%mutate(Fold=rep(0,nrow(bee_phylo_2)),holdoutpred=rep(0,nrow(bee_phylo_2)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=pgls3[fold_phy$subsets[fold_phy$which != i], ]
  validation=pgls3[fold_phy$subsets[fold_phy$which == i], ]
  tree=ape::drop.tip(bee.phy, setdiff(bee.phy$tip.label,train$Genus))
  tree=phytools::genus.to.species.tree(tree, species=train$Species)
  train_vcv=ape::corPagel(value=0.5,phy=tree,fixed=FALSE)
  
  newlm=nlme::gls(log(Spec.wgt)~log(IT) + Region,
                  correlation=train_vcv, method="ML",data=train)
  
  newpred=predict(newlm,newdata=validation,allow.new.levels=TRUE)
  true=log(validation$Spec.wgt)
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$holdoutpred <- newpred
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$RMSE=rmse
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$MSE=mse
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$MAE=mae
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$R2=R2
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$AIC=AIC(newlm)
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$BIC=BIC(newlm)
  pgls3[fold_phy$subsets[fold_phy$which == i], ]$Fold=i
}
pgls3%>%gather(.,MSE:BIC,key ="Metric",value = "Value")%>%ggplot(aes(x=Metric,y=Value,fill=Metric))+geom_boxplot()+coord_flip()+facet_wrap(~Metric,ncol=1,scales="free")+theme_bw()

K_sets=rbind(
model1%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
model2%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
model3%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
model4%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
model5%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
model6%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
pgls1%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
pgls2%>%select(.,15:20)%>%map_dbl(median,na.rm=T),
pgls3%>%select(.,15:20)%>%map_dbl(median,na.rm=T))
str(K_sets)
rownames(K_sets)=c("lme1","lme2","lme3","lme4","lme5","lme6","pgls1","pgls2","pgls3")
K_sets=as.data.frame(K_sets)

##Combine RMSEs
RMSE_sets=cbind(unique(model1$RMSE),
unique(model2$RMSE),
unique(model3$RMSE),
unique(model4$RMSE),
unique(model5$RMSE),
unique(model6$RMSE),
unique(pgls1$RMSE),
unique(pgls2$RMSE),
unique(pgls3$RMSE))
colnames(RMSE_sets)=c("LM1","LM2","LM3","LM4","LM5","LM6","PG1","PG2","PG3")
RMSE_sets=as.data.frame(RMSE_sets)

#Box plots
bee_RMSE=RMSE_sets%>%gather(.,1:9,key ="Model",value = "RMSE")%>%ggplot(aes(x=Model,y=RMSE,fill=Model))+geom_boxplot()+theme_bw()+ theme(aspect.ratio=1)


RMSE_Plots=grid.arrange(bee_RMSE,Hov_RMSE,ncol=2,nrow=1)
RMSE_Plots
