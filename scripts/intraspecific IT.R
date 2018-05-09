##INTRASPECIFIC Predictive models

##JUST FEMALES as predominant
bee_top5=rbind(bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",],
               bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",],
               bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",],
               bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",],
               bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",])

summary(lm(log(Spec.wgt)~log(IT),data=bee_species$Homalictus_urbanus[bee_species$Homalictus_urbanus$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),data=bee_species$Lasioglossum_pauxillum[bee_species$Lasioglossum_pauxillum$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),bee_species$Bombus_lucorum[bee_species$Bombus_lucorum$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),bee_species$Andrena_flavipes[bee_species$Andrena_flavipes$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),bee_species$Lasioglossum_lanarium[bee_species$Lasioglossum_lanarium$Sex == "Female",]))

summary(lm(log(Spec.wgt)~Species+log(IT),bee_top5))

int_bee1=ggplot(data=bee_top5,aes(x=log(Spec.wgt),y=log(IT),col=Species))+
  geom_point(pch=1)+
  geom_smooth(method="glm",se=FALSE)+
  theme_bw()+ theme(aspect.ratio=1)

int_bee1

#HOVERFLIES
hov_top5=rbind(hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",],
               hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",],
               hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",],
               hov_species$Melanostoma_mellinum[hov_species$Melanostoma_mellinum$Sex == "Female",],
               hov_species$Syritta_pipiens[hov_species$Syritta_pipiens$Sex == "Female",])
table(hov_top5$Species)

int_hov1=ggplot(data=hov_top5,aes(x=log(Spec.wgt),y=log(IT),col=Species))+
  geom_point(pch=1)+
  geom_smooth(method="glm",se=FALSE)+
  theme_bw()+ theme(aspect.ratio=1)
int_hov1

summary(lm(log(Spec.wgt)~log(IT),data=hov_species$Helophilus_parallelus[hov_species$Helophilus_parallelus$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),data=hov_species$Sphaerophoria_macrogaster[hov_species$Sphaerophoria_macrogaster$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),hov_species$Episyrphus_balteatus[hov_species$Episyrphus_balteatus$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),hov_species$Melanostoma_mellinum[hov_species$Melanostoma_mellinum$Sex == "Female",]))

summary(lm(log(Spec.wgt)~log(IT),hov_species$Syritta_pipiens[hov_species$Syritta_pipiens$Sex == "Female",]))

summary(lm(log(Spec.wgt)~Species+log(IT),hov_top5))

int_Plots=grid.arrange(int_bee1,int_hov1,ncol=2,nrow=1)
int_Plots
