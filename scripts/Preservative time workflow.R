##Preservation time
library(dplyr)

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
pres.lm.out[10,]

bee_mean$Wgt.cor=bee_mean$Spec.wgt+pres.lm.out[10,2]*bee_mean$Pres.time

plot(Wgt.cor~Spec.wgt,bee_mean,xlim=c(0,0.05),ylim=c(0,0.05))
#average species co-efficient

pres.lm.out[1,2]+ #intercept 
mean(pres.lm.out[2:9,2]*-1)

bee_mean$Wgt.cor.1=bee_mean$Spec.wgt+(-5.211987e-06)*bee_mean$Pres.time

plot(IT~Wgt.cor.1,data=bee_mean)

plot(Spec.wgt~Wgt.cor.1,bee_mean)

bee_mean[which(bee_mean$Species=='Homalictus_urbanus' ),]
