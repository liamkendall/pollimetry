
# Get the world map
worldMap <- getMap()
worldMap = ggmap("world")
?get_map
Country <- cbind(c(  'Australia'   ,  'Britain'     ,'Germany' ,    'Ireland'  ,     'Spain', 'Switzerland'),c( 
94         ,  4       ,   66   ,       15  ,        46   ,       68 ))


beeCoords <- lapply(worldMap, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

Bee_SR=decostand(table(bee_all$Species,bee_all$Country),"pa")
colSums(Bee_SR)

europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]


p=ggplot() +