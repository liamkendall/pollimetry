##bee foraging workflow

forage=read.csv(file="data/bee_foraging.csv",header=T)
head(forage)
str(forage)
table(forage$Tribe)

colnames(forage)
pairs(forage[,c(15,16,19:21)])

#Greenleaf 1 eq. per method of measure "type" 4 Types in greenleaf, but we have 12.
#Plus different for mean, max,...
# sample size greenleaf ~ 14 homing + 14 feeding + 70 other (in measurements, which is ~ 50 species)
# We do have ~ 107 species (x2!)
# test for sociality in a PGLS Forage ~ IT * Soc (PGLS)
# test rmse
# + 3 + 3 + 2 hoverflies ~ 7 ... 
# We provide an itertive framewwork!



options(stringsAsFactors = TRUE)

forage[1:152,c("Spec.wgt")]=exp(predict(Flm,newdata=forage[1:152,]))

#match with this which function
#bee_mean[which(bee_mean$Species=='Homalictus_urbanus' ),]


forage[135:263,c("Spec.wgt")]=exp(predict(Genus.lm,newdata=forage[135:265,]))
forage[135:263,c("Spec.wgt")]=exp(predict(Tribe.lm,newdata=forage[135:263,]))

forage$Mean[is.na(forage$Mean)] <- forage$Median[is.na(forage$Mean)]

options(na.action=na.omit)

for.lme=lmer(log(Max)~log(Spec.wgt)+Social_three+Type+(1|Reference)+(1|Species),forage)
summary(for.lme)

str(forage)

plot(log(Mean)~log(Spec.wgt),forage,col=Genus)

##REMOVE SMALL TRIBES?

as.data.frame(forage %>% group_by(Tribe) %>% 
                        filter(colSums(Tribe))>1)

                      filter(n_distinct(Tribe))>2
                      
                      