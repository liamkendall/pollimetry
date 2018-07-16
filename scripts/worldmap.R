library(rworldmap)

ddf = read.table(text="
country Richness
                 Australia 93
                 Belgium 49
                 UK 4
                 Germany 63
                 Ireland 15
                 Spain 46
                 Switzerland 63
                 USA 73
                 Brazil 22
                 ", header=T)


#join to a coarse resolution map
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")

mapCountryData(spdf, nameColumnToPlot="Richness", catMethod="fixedWidth")
