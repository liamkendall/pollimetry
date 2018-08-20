##BEE IT TEST

example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Ceratina_dupla","Andrena_flavipes"))

bodysize(x = example, taxa = "bee", type = "ITD")

#NO REGION
example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Species = c("Ceratina_dupla","Andrena_flavipes"))

bodysize(x = example, taxa = "bee", type = "ITD")
#GOOD

#NO Species
example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"))

bodysize(x = example, taxa = "bee", type = "IT")
#GOOD

#NO both
example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"))

bodysize(x = example, taxa = "bee", type = "ITD")
#GOOD

