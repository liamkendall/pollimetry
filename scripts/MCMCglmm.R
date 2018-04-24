library(MCMCglmm)


##FULL MODEL
options(na.action = "na.omit")
MCMC1=MCMCglmm(log(Spec.wgt)~log(IT)+Family+Region+Sex+
               Family:log(IT)+Region:log(IT)+Sex:log(IT),
               random=~Measurement+Species,family="gaussian", mev=NULL,
               data=bee_all,start=NULL, prior=NULL, tune=NULL, nitt=50000, thin=10, burnin=10000, pr=FALSE,
               pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
               saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)


MCMC1=MCMCglmm(log(Spec.wgt)~log(IT),random=~Measurement+Species,family="gaussian", mev=NULL,
         data=bee_all,start=NULL, prior=NULL, tune=NULL,
         scale=TRUE, nitt=50000, thin=10, burnin=10000, pr=FALSE,
         pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
         saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)

MCMC2=MCMCglmm(log(Spec.wgt)~0+log(IT)+Region,random=~Measurement+Species,family="gaussian", mev=NULL,
               data=bee_all,start=NULL, prior=NULL, tune=NULL,
               scale=TRUE, nitt=50000, thin=10, burnin=10000, pr=FALSE,
               pl=FALSE, verbose=TRUE, DIC=TRUE, singular.ok=TRUE, saveX=TRUE,
               saveZ=TRUE, saveXL=TRUE, slice=FALSE, ginverse=NULL, trunc=FALSE)




DIC(MCMC1,MCMC2,MCMC3)

#df      DIC
#MCMC1  5 1666.686
#MCMC2  9 1670.570
#MCMC3 21 1663.671
#

###Interesting how


summary(MCMC3)
