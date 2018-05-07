library(ggplot2)
library(grid)
library(rworldmap)
table(bee_mean$Country)

Country <- cbind(c('AUS','GBR','DEU','IRL','ESP','CHE','USA'),
                 c(119,6,78,23,46,86,99))
CountryTable <- data.frame(Country = Country[,1], Richness=Country[,2])
CountryTable$Richness=as.numeric(c(119,6,78,23,46,86,99))

colnames(CountryTable)=c("Country","Richness")
bee_country <- joinCountryData2Map( CountryTable
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "Country")

mapDevice() #create world map shaped window
bee_map=mapCountryData(bee_country
               ,nameColumnToPlot="Richness")
write(bee_map,"bee_map.pdf")
