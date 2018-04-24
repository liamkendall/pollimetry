


Cane_all=Cane(bee_all$IT)
LMER_all=predict(bee_all_mod2,bee_all,interval="confidence")
MCMC_all=predict(MCMC1,bee_all,interval="confidence")
PGLS_all=predict(Bee_PGLS1,bee_all,interval="confidence")

confint(Bee_PGLS1)
plot(bee_all$IT~Cane_all)
points(bee_all$IT~exp(LMER_all),col="darkred")
points(bee_all$IT~exp(MCMC_all[,1]),col="darkgreen")
points(bee_all$IT~exp(MCMC_all[,2]),col="darkgreen")
points(bee_all$IT~exp(MCMC_all[,3]),col="darkgreen")


points(bee_all$IT~exp(PGLS_all),col="darkblue")


PGLS_all