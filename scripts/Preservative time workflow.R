##Preservation time
library(dplyr)
library(broom)

#JUST AUST and GER
bee_country=split(bee_all,bee_all$Country)
bee_pres=rbind(bee_country$Australia,bee_country$Germany)

#Filter so only species with more than 1 preservative time

bee_pres=as.data.frame(bee_pres %>%
  group_by(Species) %>% 
  filter(n_distinct(Pres.time)>1))


plot(bee_pres$Spec.wgt~bee_pres$Pres.time,pch=as.numeric(bee_pres$Species))
options(na.action = na.omit)
pres.lm=lmer(Spec.wgt~Pres.time+(1|Country)+(1|Species),bee_pres)

plot(pres.lm)
pres.lm.out=tidy(pres.lm)
pres.lm.out

##Preservative time co-efficient
Pres_coef=pres.lm.out[11,]

bee_2=bee_all
bee_2$Spec.wgt=bee_2$Spec.wgt+-0.006729*bee_2$Pres.time

bee_2[is.na(bee_2)] <- 0
-0.006729*160
