#BEE TAX MODELS

bee_lm1=lmer(log(Spec.wgt) ~ log(IT)  + Family + 
  Sex + #fix factors
  log(IT):Family  + log(IT):Sex+(1|Region/Species),bee_mean)
options(na.action = "na.fail")
bee_models=get.models(dredge(bee_lm1,fixed=c("log(IT)","Family")),subset=TRUE)

names(bee_models)=as.character(rep(1:6,1))
names(bee_models)



#PHY MODELS
bee_phy_lm1=lmer(log(Spec.wgt) ~ log(IT) + 
             Sex + log(IT):Sex+(1|Region/Species),bee_mean)
bee_phy_models=get.models(dredge(bee_phy_lm1,fixed=c("log(IT)")),subset=TRUE)

names(bee_phy_models)=as.character(rep(1:3,1))
names(bee_phy_models)
bee_phy_models[[3]]
#HOV MODELS
hov_lm1=lmer(log(Spec.wgt) ~ log(IT)  + Subfamily + 
             Sex + #fix factors
             log(IT):Subfamily + log(IT):Sex+(1|Region/Species),hov_mean)
options(na.action = "na.fail")
hov_models=get.models(dredge(hov_lm1,fixed=c("log(IT)")),subset=TRUE)
names(hov_models)=as.character(rep(1:9,1))
names(hov_models)

