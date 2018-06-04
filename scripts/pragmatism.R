##TEST DIFFERENT RANDOM EFFECT STRUCTURES

options(na.action = "na.fail")
bee_test_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                 log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                 (1|Measurement/Species),REML=FALSE,bee_mean)

bee_test_lme=dredge(bee_test_lme,beta="none",rank="AIC") #think about "sd" and "AICc". AIC show same pattern.
head(bee_test_lme)

bee_test1_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                    log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                    (1|Country/Measurement/Species),REML=FALSE,bee_mean)

bee_test1_lme=dredge(bee_test1_lme,beta="none",rank="AIC") #think about "sd" and "AICc". AIC show same pattern.
head(bee_test1_lme)

bee_test2_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                     log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                     (1|Country/Measurement)+(1|Species),REML=FALSE,bee_mean)

bee_test2_lme=dredge(bee_test2_lme,beta="none",rank="AIC") #think about "sd" and "AICc". AIC show same pattern.
head(bee_test2_lme)

bee_test3_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                     log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                     (1|Country/Measurement),REML=FALSE,bee_mean)

bee_test3_lme=dredge(bee_test3_lme,beta="none",rank="AIC") #think about "sd" and "AICc". AIC show same pattern.
head(bee_test3_lme)

##Pragmatic bee mean dataframe
bee_prag=aggregate(Latitude~Family+Subfamily+Tribe+Region+Subfamily+Genus+Species+Sex,bee_all,mean)
bee_prag$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Subfamily+Tribe+Region+
                                                Subfamily+Genus+Species+Sex,bee_all,mean)[8]))
bee_prag$IT=(as.numeric(unlist(aggregate(IT~Family+Subfamily+Tribe+Region+
                                           Subfamily+Genus+Species+Sex,bee_all,mean)[8])))

#49 Apis australia
#319 Halictus rubicundus NA

##keep in for now

options(na.action = "na.fail")
bee_test_lm=lm(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                    log(IT):Family + log(IT):Region + log(IT):Sex,bee_prag)

bee_prdredge1=dredge(bee_test_lm,beta="none",rank="AICc") #think about "sd" and "AICc". AIC show same pattern.

head(bee_prdredge1)

bee_prag2=bee_prag[-319,]
bee_prag2=bee_prag2[-49,]


options(na.action = "na.fail")
bee_test_lm2=lm(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                 log(IT):Family + log(IT):Region + log(IT):Sex,bee_prag2)

bee_prdredge2=dredge(bee_test_lm2,beta="none",rank="AICc") #think about "sd" and "AICc". AIC show same pattern.

head(bee_prdredge2)

##SAME SO REMOVE

#Without region
bee_test_lm3=lm(log(Spec.wgt) ~ log(IT)  + Family + Sex + #fix factors
                  log(IT):Family + log(IT):Sex,bee_prag2)

bee_prdredge3=dredge(bee_test_lm3,beta="none",rank="AICc") #think about "sd" and "AICc". AIC show same pattern.

head(bee_prdredge3)

AIC(lm(log(Spec.wgt) ~ log(IT),bee_prag2))


##MCMCGlmm

bee_prag2$animal <- bee_prag2$Species

# set some constant values
prior1 <- list(R= list(V=1, n=1, fix=1), G= list(G1=list(V=1, n=1))) # simplest prior
Nnitt <- 50000 # default is 13000
Nburnin <- 3000 # default is 3000
Nthin <- 1 # default is 10

#Set tree for mcmc

bee_mcmc=bee_pruned
bee_mcmc=force.ultrametric(bee_mcmc)
is.ultrametric(bee_mcmc)

mod <- MCMCglmm(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                  log(IT):Family + log(IT):Region + log(IT):Sex, 
                family = "gaussian", data= bee_prag2, verbose=FALSE, thin=Nthin, nitt=Nnitt, burnin=Nburnin)
updateable(MCMCglmm)
dredge(mod)
