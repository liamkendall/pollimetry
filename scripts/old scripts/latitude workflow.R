##Latitude workflow

#Filter so only species with more than one latitude
bee_lat=as.data.frame(bee_all2 %>%
                        group_by(Genus) %>% 
                        filter(n_distinct(round(Latitude))>3))

bee_lat$Latitude=round(bee_lat$Latitude,2)

par(mfrow=c(1,2))

plot(bee_lat$Spec.wgt~round(bee_lat$Latitude))

boxplot(bee_lat$IT~bee_lat$Country)
lat_mod=lmer(log(Spec.wgt)~Genus+Latitude+(1|Country),bee_lat)
summary(lat_mod)
lat_mod.out=tidy(lat_mod)
lat_mod.out

plot(lat_mod)

lat_mod.out[17,2]

bee_mean$Wgt.lat=bee_mean$Spec.wgt+7.590931e-05*bee_mean$Latitude

par(pty="s")
par(mfrow=c(1,2))
plot(log(bee_mean$Spec.wgt)~log(bee_mean$IT))
plot(log(bee_mean$Wgt.lat)~log(bee_mean$IT))   
plot(log(bee_mean$Wgt.lat)~log(bee_mean$IT),col=as.numeric(as.factor(bee_mean$Country)))
summary(lat_mod)



plot(log(Spec.wgt)~log(IT),bee_lat,col=as.numeric(as.factor(bee_lat$Country)))

plot(log(Spec.wgt)~Latitude,bee_lat,col=1+as.numeric(as.factor(bee_lat$Country)))
summary(lat_mod)
        
round(bee_lat$Latitude,1)

##JUST BRITAIN AND IRELAND

bee_ukig=rbind(bee_country$Britain,bee_country$Ireland,bee_country$Germany,bee_country$Spain)

bee_ukig=as.data.frame(bee_ukig %>%
                         group_by(Genus) %>% 
                         filter(n_distinct(Country)>3))
bee_ukig

boxplot(bee_ukig$Spec.wgt~as.factor(bee_ukig$Country))

boxplot(bee_ukig$Spec.wgt~round(bee_ukig$Latitude))

NthHem_mod=lmer(log(Spec.wgt)~Latitude+(1|Species),bee_ukig)

SthHem_mod=lmer(log(Spec.wgt)~Latitude+(1|Species),bee_country$Australia)

summary(NthHem_mod)
summary(SthHem_mod)

plot(log(Spec.wgt)~Latitude,bee_country$Australia)
abline(predict(Latitude~NthHem_mod,newdata=bee_ukig))


ggplot(data=bee_all2,aes(log(Latitude),log(Spec.wgt)))+geom_point()+
  geom_smooth(aes(col=bee_all2$Tribe),method="lm",se=FALSE)

ggplot(data=bee_ukig,aes(log(IT),log(Spec.wgt)))+geom_point()+
  geom_smooth(aes(col=bee_ukig$Family),method="lm",se=FALSE)+theme_bw()

summary(lm(log(Spec.wgt)~log(IT)+as.factor(Genus)+Country,bee_ukig))
str(bee_ukig)
