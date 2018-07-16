library(rstan)
rstan_options(auto_write = TRUE)
library(brms)

library(parallel)
detectCores()

####
##phylo structure for brms
inv.phylo <- MCMCglmm::inverseA(bee.tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)

#nested
B=A
bee_mean$Rspecies=paste(bee_mean$Region,bee_mean$Species,sep="_")
region_species=unique(bee_mean[ order(match(bee_mean$Species, bee.tree$tip.label)), ]$Rspecies)
rownames(B)=unique(region_species)
isSymmetric(B,check.attributes=FALSE)


#PHYLOGENETIC SIGNAL
#hyp <- "sd_Region:Species__Intercept^2 / (sd_Region:Species__Intercept^2 + sigma^2) = 0"
#(hyp <- hypothesis(bee_p1, hyp, class = NULL))
#plot(hyp)

####
#The default prior is an improper flat prior over the reals

###TAXO - BEE MODELS
str(bee_models)


##SET NORMAL FLAT PRIORS FOR BOTH Bs and Random Terms
bprior1 <- prior(normal(1,3), class = b) + 
  prior(normal(0,1), class = sd) #based off what I observed using default priors, 
                                 #this improved posteriors of bee_model1 - the full model


#Divergent transitions after warmup so increased adapt_delta above 0.8

bee1<- brm(formula(bee_models$'1'), data = bee_mean,inits="0",
  family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1,
  control = list(adapt_delta = 0.99))
#0 divergent transitions GOOD

bee2<- brm(formula(bee_models$'2'), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99,max_treedepth=15))

##10 divergent transitions
bee3<- brm(formula(bee_models$'3'), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1
           ,control = list(adapt_delta = 0.99,max_treedepth=15))
#4 divergent transitions

bee4<- brm(formula(bee_models$'4'), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1
           ,control = list(adapt_delta = 0.99))
#GOOD

bee5<- brm(formula(bee_models$'5'), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1
           ,control = list(adapt_delta = 0.99))

#good

bee6<- brm(formula(bee_models$'6'), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1
           ,control = list(adapt_delta = 0.99))


#good

bee7<- brm(log(Spec.wgt)~log(IT)+(1|Region/Species), data = bee_mean,
           family = gaussian(),iter=11000,warmup=1000,thin=10,prior=bprior1
           ,control = list(adapt_delta = 0.99))
plot(bee7)
#good

##BEE_PHY

bee_p1<- brm(formula(bee_phy_models$'1'), data = bee_mean,
           family = gaussian(),cov_ranef = list("Region:Species" = B),prior=bprior1
           ,iter=11000,warmup=1000,thin=10,control = list(adapt_delta = 0.99))
plot(bee_p1)
#good

bee_p2<- brm(formula(bee_phy_models$'2'), data = bee_mean,prior=bprior1,
           family = gaussian(),cov_ranef = list("Region:Species" = B)
           ,iter=11000,warmup=1000,thin=10,control = list(adapt_delta = 0.99))
#good

bee_p3<- brm(formula(bee_phy_models$'3'), data = bee_mean,prior=bprior1,
           family = gaussian(),cov_ranef = list("Region:Species" = B),
           iter=11000,warmup=1000,thin=10,control = list(adapt_delta = 0.99,max_treedepth=15))

#1 divergent #increaed tree_depth and GONE

#bee_mcmc_list=list(bee1,bee2,bee3,bee4,bee5,bee6,bee7,bee_p1,bee_p2,bee_p3)
#bee_mcmc_list=mclapply(bee_mcmc_list, function(x) add_loo(x,reloo=TRUE))
#bee_mcmc_list=mclapply(bee_mcmc_list, function(x) add_ic(x,ic=c("kfold","waic","R2")))

bee_mcmc_list[[7]]$kfold

lapply(bee_mcmc_list, '[[', c("kfold"))


###HOVERFLY MODELS

hov1<- brm(formula(hov_models$'1'), data = hov_mean,
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99))
#good

hov2<- brm(formula(hov_models$'2'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99,max_treedepth=15))
#3 divergent - gone with increased treedepth

hov3<- brm(formula(hov_models$'3'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99))
#good

hov4<- brm(formula(hov_models$'4'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99))
#good

hov5<- brm(formula(hov_models$'5'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99,max_treedepth=15))

#4 divergent transitions - gone with increased treedepth

hov6<- brm(formula(hov_models$'6'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99))
#good

hov7<- brm(formula(hov_models$'7'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.999,max_treedepth=20))
#5 divergent transitions, 1 with treedepth 15 and 20, 
#reduced stepsize (0.1) made it worse, gone with delta 0.999

hov8<- brm(formula(hov_models$'8'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.99,max_treedepth=15))
#1 divergent transition - gone with increased treedepth 15

hov9<- brm(formula(hov_models$'9'), data = hov_mean, 
           family = gaussian(),
           iter=11000,warmup=1000,thin=10,prior=bprior1,
           control = list(adapt_delta = 0.999,max_treedepth=20))
#4 divergent transitions, increaed delta 0.999, treedepth 20 - fixed


##COMPUTE LOO INFORMATION CRITERION 

#WAIC was inappropriate, on the basis of p-aic, 
#Aki Vehtari, Andrew Gelman and Jonah Gabry (2017). Practical Bayesian model evaluation using leave-one-out cross-validation 
#and WAIC. Statistics and Computing, 27(5):1413–1432. doi:10.1007/s11222-016-9696-4. arXiv preprint arXiv:1507.04544

#bee_mcmc_list=list(bee1,bee2,bee3,bee4,bee5,bee6,bee7,bee_p1,bee_p2,bee_p3)

#bee_mcmc_list=lapply(bee_mcmc_list, function(x) add_loo(x,reloo=TRUE))

#bee_mcmc_list=lapply(bee_mcmc_list, function(x) add_ic(x,ic=c("waic","R2")))
#bee_kfold_list=lapply(bee_mcmc_list,function(x) kfold(x,compare=TRUE,K=10,save_fits=TRUE))

#bee_loo=compare_ic(x=bee_mcmc_list,ic="loo")


#lapply(bee_mcmc_list, '[[', c("loo")) 


hov_kfold_list=lapply(hov_mcmc_list,function(x) kfold(x,compare=TRUE,K=10,save_fits=TRUE))

hov_mcmc_list=list(hov1,hov2,hov3,hov4,hov5,hov6,hov7,hov8,hov9)
hov_mcmc_list=mclapply(hov_mcmc_list, function(x) add_loo(x))
hov_mcmc_list=mclapply(hov_mcmc_list, function(x) add_ic(x,ic=c("waic","R2")))




lapply(bee_mcmc_list, '[[', c("kfold"))

hov_loo=compare_ic(x=hov_mcmc_list,ic="loo")


bee_loo=compare_ic(x=bee_mcmc_list,ic="loo")
bee_mcmc_list[[1]]$

lapply(hov_mcmc_list, '[[', c("loo")) 

lapply(bee_mcmc_list,function(x) kfold(x,compare=TRUE,K=10,save_fits=TRUE))

hov_kfold_list=mclapply(hov_mcmc_list,function(x) kfold(x,compare=TRUE,K=10,save_fits=TRUE))

hov_kfold_list[[1]]

kfold(hov1,compare=TRUE,K=10,save_fits=TRUE)
