#check for phylo signal with model 2)----
#Note to self: Model 1 can be tested too using MCMC.

Bee_tree=as.phylo(~Family/Subfamily/Genus/Species, data=bee_mean)
plot(Bee_tree)

#gives the tree random branch lengths (will try a bunch to see how it impacts the results)
Bee_tree.rand <- compute.brlen(Bee_tree,method="Grafen")

summary(Bee_tree.rand)

#first compute correlation matrix from tree
Bee_vcv=corPagel (0.5,Bee_tree.rand,fixed=FALSE)

Bee_GLS1<- gls(Spec.wgt~IT, data=bee_mean, method="ML")
summary(Bee_GLS1)

Bee_PGLS1 = gls(Spec.wgt~IT, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS1)

#lambda 0.3206538  

AIC(Bee_GLS1,Bee_PGLS1)
#df      AIC
#Bee_GLS1   3 501.8803
#Bee_PGLS1  4 468.6825

#check also lambda of the trait itself.

##Specimen weight
Bee_GLS_WGT1<- gls(Spec.wgt~1, data=bee_mean, method="ML")
summary(Bee_GLS_WGT1)

Bee_PGLS_WGT1 = gls(Spec.wgt~1, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_WGT1)
#lambda 0.00281186  

AIC(Bee_GLS_WGT1,Bee_PGLS_WGT1) #Phylo signal weak
#df      AIC
#Bee_GLS_WGT1   2 671.6540
#Bee_PGLS_WGT1  3 673.6317


##IT span
Bee_GLS_IT1<- gls(IT~1, data=bee_mean, method="ML")
summary(Bee_GLS_IT1)

Bee_PGLS_IT1 = gls(IT~1, data=bee_mean, correlation=Bee_vcv, method="ML")
summary(Bee_PGLS_IT1)

#lambda -0.0.003330929 

AIC(Bee_GLS_IT1,Bee_PGLS_IT1) #Really low phylo signal... good.
#df      AIC
#Bee_GLS_IT1   2 289.4437
#Bee_PGLS_IT1  3 291.4097

