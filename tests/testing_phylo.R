#PHYLO CHECKS
#working example
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Ceratina_dupla","Andrena_flavipes"))

bodysize(x = example, taxa = "bee", type = "phylo")
#CI's are quite large...

#unknown species----

example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("unknown","unknown"))

bodysize(x = example, taxa = "bee", type = "phylo")
#Liam, Region as random is considered, right? So the text should reflect that. - DONE

#no species vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#INSERTED STOP HERE 

#wrong sex----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Fmale","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#Good! - FINE AGAIN

#No sex vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#Good!

#Unknown Family----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Unknown","Unknown"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#good - fine / unnecessary

#No Family vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#WNot necessary

#Unknown Region----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("Unknown","Unknown"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#good

#No Region vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "bee", type = "phylo") 
#Needs an error as above!

##Similar checks should be made with type = Phylo and type = IT and taxa = hov

#In hove, why don't call h2 = phylo, h1 = sex (or is h1 needed? I would only show two models for simplicity).
#Note that help call the hov model phylo already. The help file also lacks the full reference. 
