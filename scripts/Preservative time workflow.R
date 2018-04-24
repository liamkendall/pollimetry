##Preservation time
library(dplyr)
library(broom)

#JUST AUST
bee_country=split(bee_all,bee_all$Country)
bee_australia=bee_country$Australia


#Filter so only species with more than 1 preservative time

bee_aus=as.data.frame(bee_australia %>%
  group_by(Species) %>% 
  filter(n_distinct(Pres.time)>1))

plot(bee_aus$Spec.wgt~bee_aus$Pres.time,pch=as.numeric(bee_aus$Species))

pres.lm=lm(Spec.wgt~Species+Pres.time,bee_aus)

summary(pres.lm)
pres.lm.out=tidy(pres.lm)
pres.lm.out

##Preservative time co-efficient
Pres_coef=pres.lm.out[11,]

