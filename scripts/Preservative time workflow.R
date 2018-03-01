##Preservation time
library(dplyr)

#JUST AUST
bee_country=split(bee_all,bee_all$Country)
bee_aus=bee_country$Australia


#Filter so only species with more than 1 preservative time

bee_aus=as.data.frame(bee_aus %>%
  group_by(Species) %>% 
  filter(n_distinct(Pres.time)>1))

plot(bee_aus$Spec.wgt~bee_aus$Pres.time)

pres.lm=lm(Spec.wgt~Species+Pres.time,bee_aus)

summary(pres.lm)
pres.lm.out=tidy(pres.lm)
pres.lm.out
bee_mean[c("Homalictus_urbanus"),]
##Preservative time co-efficient
pres.lm.out[10,]

bee_all$Wgt.cor=bee_all$Spec.wgt+pres.lm.out[10,2]*bee_all$Pres.time

plot(Wgt.cor~Spec.wgt,bee_all)
#average species co-efficient

pres.lm.out[1,2]+ #intercept 
mean(pres.lm.out[2:9,2]*-1)

bee_mean$Wgt.cor.1=bee_mean$Spec.wgt+(-5.211987e-06)*bee_mean$Pres.time

plot(IT~Wgt.cor.1,data=bee_mean)

plot(Spec.wgt~Wgt.cor.1,bee_mean)

summary(lm(predict(pres.lme, bee_aus) ~ log(bee_aus$Spec.wgt)))
plot(predict(pres.lme, bee_aus)~ bee_aus$Spec.wgt)



correction = function(w, pt){
  out = a + b*pt
}

##NOT SURE HOW TO TRANSFORM THIS BACK / APPLY CORRECTION
-2.680169e-02+ 2.865256e-02
bee_mean[which(bee_mean$Species=='Homalictus_urbanus' ),]
