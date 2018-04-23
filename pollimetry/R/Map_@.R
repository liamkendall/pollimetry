library(ggplot2)
library(grid)
library(rworldmap)

countryExData


CountryTable <- data.frame(Country = Country[,1], Richness=Country[,2])
CountryTable$Richness=as.numeric(CountryTable$Richness)

CountryTable$Country=c('AUS','GBR','DEU','IRL','ESP','CHE')
colnames(CountryTable)=c("Country","Richness")
bee_country <- joinCountryData2Map( CountryTable
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "Country")

mapDevice() #create world map shaped window
bee_map=mapCountryData(bee_country
               ,nameColumnToPlot="Richness")
write(bee_map,"bee_map.pdf")
