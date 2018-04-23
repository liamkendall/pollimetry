#####ALL INDIVIDUALS AS DATA FRAME
bee_all$Spec.wgt=log(bee_all$Spec.wgt*1000)
bee_all$IT=log(bee_all$IT)

options(na.action="na.omit")
bee_all_lme=lmer(Spec.wgt ~ IT  + Family + Cl_simp + Latitude + Sex+Region+ #fix factors
                 IT:Family + IT:Region + IT:Cl_simp + IT:Latitude + IT:Sex + #interactions
                 (1|Measurement)+(1|Species), REML=FALSE,bee_all)
options(na.action="na.fail")
bee_all_dr=dredge(bee_all_lme)
head(bee_all_dr)

bee_all_mod=get.models(bee_all_dr[1],subset=TRUE)

bee_all_mod1=lmer(Spec.wgt ~ Cl_simp + Family + IT + Latitude + Region + (1 | Measurement) +  
                    (1 | Species) + Cl_simp:IT + Family:IT + IT:Latitude + IT:Region,
     data = bee_all)
r.squaredGLMM(bee_all_mod1)

#####
bee_all_mod2=lmer(log(Spec.wgt) ~  log(IT) +  (1 | Measurement) +  
                    (1 | Species),
                  data = bee_all)

summary(bee_all_mod2)
r.squaredGLMM(bee_all_mod2)
#r.squaredGLMM(bee_all_mod2)
#R2m       R2c 
#0.8510668 0.9367778 

bee_all$Spec.wgt
##PLOT - Comparing mean vs full model
ggplot(data=bee_all,aes(log(IT),log(Spec.wgt),col=Region))+theme_bw()+
    theme(aspect.ratio=1)+
    geom_smooth(formula=y~x-1,data=bee_all,aes(),method="glm")+
    geom_smooth(formula=y~x-1,data=bee_mean,aes(),method="glm")+
    coord_equal(xlim=c(-1,2.5),ylim=c(-2,7.5))+
    geom_point(data=bee_mean,pch=0)+
    geom_point(data=bee_phylo,pch=0,col=3)
  
  
  ≥##I SAY - USE FULL MODEL
  
  
