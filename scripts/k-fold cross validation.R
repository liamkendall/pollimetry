
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
fold_phy<-bee_phylo_2%>%fold_cv(.,k=5)

str(fold_phy)

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

##Just IT
model3<-bee_mean%>%mutate(Fold=rep(0,nrow(bee_mean)),holdoutpred=rep(0,nrow(bee_mean)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=model3[fold$subsets[fold$which != i], ]
  validation=model3[fold$subsets[fold$which == i], ]
  newlm=lme4::lmer(formula=log(Spec.wgt) ~ log(IT) + #interactions
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

##PGLS - not done yet - not working ugh


#Set up folds
fold_phy<-bee_phylo_2%>%fold_cv(.,k=5)

#Need these two steps in the loop - for validation set
bee.phy
tree=drop.tip(bee.phy, setdiff(bee.phy$tip.label,bee_phylo$Genus))
tree=genus.to.species.tree(bee_pruned, species=bee_phylo$Species)
Bee_vcv=corPagel(value=0.5,phy=tree,fixed=FALSE)

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

##JUST IT
pgls2<-bee_phylo_2%>%mutate(Fold=rep(0,nrow(bee_phylo_2)),holdoutpred=rep(0,nrow(bee_phylo_2)),MSE=rep(0,nrow(.)),RMSE=rep(0,nrow(.)),MAE=rep(0,nrow(.)),R2=rep(0,nrow(.)),AIC=rep(0,nrow(.)),BIC=rep(0,nrow(.)))
for(i in 1:5){
  train=pgls2[fold_phy$subsets[fold_phy$which != i], ]
  validation=pgls2[fold_phy$subsets[fold_phy$which == i], ]
  tree=ape::drop.tip(bee.phy, setdiff(bee.phy$tip.label,train$Genus))
  tree=phytools::genus.to.species.tree(tree, species=train$Species)
  train_vcv=ape::corPagel(value=0.5,phy=tree,fixed=FALSE)
  
  newlm=nlme::gls(log(Spec.wgt)~log(IT),
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

##Combine RMSEs
K_sets=cbind(unique(model1$RMSE),
unique(model2$RMSE),
unique(model3$RMSE),
unique(pgls1$RMSE),
unique(pgls2$RMSE))
colnames(K_sets)=c("1","2","3","4","5")
K_sets=as.data.frame(K_sets)

#Box plots
K_sets%>%gather(.,1:5,key ="Model",value = "Value")%>%ggplot(aes(x=Model,y=Value,fill=Model))+geom_boxplot()+theme_bw()

