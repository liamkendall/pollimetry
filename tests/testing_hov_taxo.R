#HOV CHECKS
#working example
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Eristalinae","Eristalinae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Eristalis_tenax","Eristalis_tenax"))

bodysize(x = example, taxa = "hov", type = "taxo")
#CI's are quite large...

#unknown species----

example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Eristalinae","Eristalinae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("unknown","unknown"))

bodysize(x = example, taxa = "hov", type = "taxo")
#Liam, Region as random is considered, right? So the text should reflect that. - DONE

#no species vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Eristalinae","Eristalinae"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#INSERTED STOP HERE 

#wrong sex----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Fmale","Male"), 
                            Subfamily = c("Eristalinae","Eristalinae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#Good! - FINE AGAIN

#No sex vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Subfamily = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#Good!

#Unknown Subfamily----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Unknown","Unknown"),
                            Region = c("NorthAmerica","Europe"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#good


####
#No Subfamily vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#Good

#Unknown Region----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Syrphinae","Syrphinae"),
                            Region = c("Unknown","Unknown"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#good

#No Region vector----
example <- cbind.data.frame(ITD = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Subfamily = c("Syrphinae","Syrphinae"),
                            Species = c("Apis_mellifera","Apis_mellifera"))
bodysize(x = example, taxa = "hov", type = "taxo") 
#Added warning

##Similar checks should be made with type = taxo and type = IT and taxa = hov

#In hove, why don't call h2 = taxo, h1 = sex (or is h1 needed? I would only show two models for simplicity).
#Note that help call the hov model taxo already. The help file also lacks the full reference. 
