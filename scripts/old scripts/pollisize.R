#' @name pollisize
#' 
#' @title Provides species mean body weight 
#' 
#' @description Calculates dry body weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' 
#' @param Species Type of morphometric measurement. Can be either: "BL" - body length, "BLW" - body length x width or "HW" - head width.
#' 
#' @param Genus A vector of pollinator body length (BL) measurements (mm).
#' 
#' @return A dataframe with pollinator body size (mg) is returned for each species or genus.
#' 
#' @examples
#' pollisize(Species=c("Apis mellifera","Amegilla asserta","Amegilla bombiformis"))
#'pollisize(Genus=c("Megachile"))
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD ALL REFERENCES
#' 
#' @export
pollisize=function(Species = NULL, Genus = NULL{
if(!Species %in% c("DIP","HYM","LEP", "S93DB","Sabo02DB", "Sabo02DA", "Sabo02DBB", "R77D",
              "S93DC", "BN06D", "G97D", "GR84D", "JS00DA",
              "S80DCF", "S80DCR", "S80DMF", "S93DA", "W13D", "JS00DN", 
              "S93DN", "Sabo02DN","DIP","Brachycera","Nematocera",
              "Asilidae","Bombyliidae")){
  stop("Species not found. Check spelling and/or reference data file.'")
} else {

  lapply(bee_species[Species], function(aggregate) aggregate(Spec.wgt~Sex, mean, bee_species))
  
  
  bee_species[[Species[2]]]
  bee_species[[Species[3]]]
  aggregate((bee_species[Species],FUN=mean)
  
  out = c()
  for(i in 1:length(Bee_sz$IT)){
    subset1 = sample(Bee_sz$IT, i)
    out[i] = mean(subset1)
  }
  plot(out)
