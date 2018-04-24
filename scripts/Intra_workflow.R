#Intraspecific variation ~ Sample size

#X axis = standard deviation | error of each trait: WGT, IT, BL
#Y axis = sample size


##variation within a species rather than all speceis
###More than ten species

table(bee$Species)
out = c()
for(i in 1:length(Bee_sz$IT)){
  subset1 = sample(Bee_sz$IT, i)
  out[i] = mean(subset1)
}
plot(out)


