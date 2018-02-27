##Preservation time
library(dplyr)

#JUST AUST
bee_country=split(bee_all,bee_all$Country)
bee_aus=bee_country$Australia

#Filter so only species with more than 1 preservative time

bee_aus=as.data.frame(bee_aus %>%
  group_by(Species) %>% 
  filter(n_distinct(Pres.time)>1))

pres.lme=lmer(log(Spec.wgt)~Pres.time+(1|Species),bee_aus)
summary(pres.lme)

