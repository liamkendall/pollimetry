#manual tests for now, at some point we sould use testthat()

#working example
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                             Sex = c("Female","Male"), 
                             Family = c("Apidae","Andrenidae"),
                             Region = c("NorthAmerica","Europe"),
                             Species = c("Ceratina_dupla","Andrena_flavipes"))

bodysize(x = example, taxa = "bee", type = "taxo")
#CI's are quite large...

#unknown species----

example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("unknown","unknown"))

bodysize(x = example, taxa = "bee", type = "taxo")
#Liam, Region as random is considered, right? So the text should reflect that. - DONE

#no species vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#Good, but can contain the same error message as above. - DONE!

#wrong sex----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Fmale","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#Good!

#No sex vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#Good!

#Unknown Family----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Unknown","Unknown"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#good

#No Family vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#We need a better error here. (as Above) - DONE

#Unknown Region----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("Unknown","Unknown"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#good

#No Region vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"))
bodysize(x = example, taxa = "bee", type = "taxo") 
#Needs an error as above!

##Similar checks should be made with type = Phylo and type = IT and taxa = hov

#In hove, why don't call h2 = taxo, h1 = sex (or is h1 needed? I would only show two models for simplicity).
#Note that help call the hov model taxo already. The help file also lacks the full reference. 
