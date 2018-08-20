#' @name bodysize
#' 
#' @title Converts the intertegular distance (ITD) and co-variates to body size in dry weight (mg) for bees or hoverflies.
#' 
#' @description Calculates body size in dry weight (mg) from Kendall et al. (2018) using ITD (and co-variate) values.  
#' 
#' @param x A data frame with columns containing ITD values and Sex ('Male' or 'Female'). 
#' Optional attributes depending on model choice: Taxonomic family (bees) or subfamily (hoverfly), Region (Only "NorthAmerica", "SouthAmerica", "Australasia" and "Europe" implemented) and Species ("Genus_species"). For non implemented regions, the model will work, but only consider population-level (fixed) effects. 
#'
#' @param taxa A vector specifying insect taxa of interest, can be either "bee" for bee models and "hov" for hoverfly models
#' 
#' @param type A vector specifying model type to be used: for bees this can be either "taxo" for taxonomic models, "phy" for phylogenetic model or "ITD" for ITD-only model. In hoverflies: it can either be "taxo" for full taxonomic and sex model or "ITD" for ITD-only model.
#' 
#' @return The original dataframe (x) is returned along with four additional columns: body size (dry weight (mg)), S.E. and 95 perent confidence intervals.
#' 
#' @details For bees, type option 'taxo' requires ITD, 
#' sex and taxonomic family.  Type option 'phylo' only requires ITD and Sex to run 
#' but should be only be used for with Species (and Region) included in model formulation n.b. the function checks for contained species.  
#' 
#'  For hoverflies, type 'taxo' requires ITD, Subfamily and Sex for each specimen. Type "ITD" for both 
#'  taxa only requires ITD values (Optional: region and species but check `setdiff`). 
#'  If specimens are from included regions or species (see above) we recommend 
#'  including these as additional columns. Estimates (and variance components) are 
#'  returned as four additional columns bound to the original dataframe. In the likely case that non-represented taxa and regions are included in inputted datasets, `allow_new_levels` is set to true for all models. Estimates will be then averaged after random variance components.
#' 
#' @importFrom stats predict
#' 
#' @importFrom utils data installed.packages
#' 
#' 
#' @import brms
#' 
#' @examples
#' example=cbind.data.frame(ITD=c(1.3,2.3),
#'                          Sex=c("Female","Male"), 
#'                          Family=c("Apidae","Andrenidae"),
#'                          Region=c("NorthAmerica","Europe"),
#'                          Species=c("Ceratina_dupla","Andrena_flavipes"))
#' bodysize(x=example,taxa="bee",type="taxo")
#' 
#' @references Kendall et al. (2018) Pollinator size and its consequences: Predictive allometry for pollinating insects. In prep.
#' 
#' @export
bodysize=function(x,taxa,type) {

  if(is.null(x$Species)==TRUE & is.null(x$Region)==FALSE){
    warning("Species have not been provided, these models will only consider fixed and random biogeographical effects.")
  }
  
  if(is.null(x$Species)==TRUE & is.null(x$Region)==TRUE){
    warning("Species and region have not been provided, these 
            models will only consider fixed effects.")
  }
  if(is.null(x$Species)==FALSE & is.null(x$Region)==TRUE){
    warning("Region has not been provided, these 
            models will only consider fixed and species-level random effects.")
  }
  
  data("pollimetry_dataset", envir = environment())
  check_sp <- x$Species %in% pollimetry_dataset$Species 
  if(any(check_sp==FALSE)){
    warning("Species are different from those used in model formulation, for those species, these models will only consider fixed and random biogeographical effects.")
  } 
  
  check_Region <- x$Region %in% c("Australasia","NorthAmerica","SouthAmerica","Europe")
  if(any(check_Region == FALSE)){
    warning("Only the following regions are supported currently; 'Australasia','NorthAmerica','SouthAmerica' and 'Europe'. Model will only consider fixed effects.")
  }
  if(type == "taxo" & taxa == "bee"){
    check_taxo <- x$Family %in% c("Andrenidae","Apidae","Colletidae","Halictidae","Melittidae","Megachilidae")
    if(any(check_taxo == FALSE)){
      stop("Family should be either 'Andrenidae','Apidae','Colletidae',Halictidae','Megachilidae','Melittidae'. If family is unknown, use type = 'ITD'.")
    }
  }
  if(type == "taxo" & taxa == "bee" & is.null(x$Family)==TRUE){
    stop("Family not provided. If family is unknown, use type = 'ITD'.")
  }
  if(type == "taxo" & taxa == "hov"){
    check_taxo <- x$Family %in% c("Eristalinae","Syrphinae")
    if(any(check_taxo == FALSE)){
      stop("Subfamily should be either 'Eristalinae' or 'Syrphinae'. If family is unknown, use type = 'ITD'.")
    }
  }
  if(type %in% c("taxo", "phylo")){
    if("Sex" %in% colnames(x)==FALSE) {
      stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
    }
  }  
  check_Sex <- x$Sex %in% c("Female","Male")
  if(any(check_Sex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
  }
  ##PHYLO ERRORS
  if(type=="phylo" & is.null(x$Species)==TRUE){
    stop("Species not provided. Use type 'taxo'.")
    
  }
  if(type=="phylo" & is.null(x$Region)==TRUE){
    warning("Region not provided. Model will only consider fixed and random species-level effects.")
  }
  ##HOV ERRORS
  check_hovregion <- x$Region %in% c("Europe","Australia")
  if(any(check_Sex==FALSE)){
    warning("Only specimens from Europe and Australia were used in model formulation. New regions will be modelled with group-level uncertainty in the predictions based on the variation of the existing levels.")
  }
  check_hovsex <- x$Sex %in% c("Female","Male")
  if(type=="taxo" & any(check_hovsex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
  }
  check_hovsub <- x$Subfamily %in% c("Eristalinae","Syrphinae")
  if(type=="taxo" & any(check_hovsub==FALSE)){
    stop("Only Eristalinae and Syrphinae supported currently. Use type = 'ITD'.")
  }
  if(type=="taxo" & is.null(x$Subfamily)==TRUE){
    stop("No subfamily column (Eristalinae and/or Syrphinae) found. Use type = 'ITD'.")
  }
  if(taxa=="hov" & is.null(x$Region)==TRUE){
    warning("Region not found. Only fixed effects will be inferred.") 
  }
  
  ##Incorrect type and taxa combos
  if(type=="phylo" & taxa =="hov"){
    stop("Bad combination: No phylogenetic model implemented for hoverflies yet!")
    
    #FIT MODELS
  } else if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == TRUE) {
    if(type=="taxo" & taxa=="bee"){
      mod=pollimetrydata::bee_tax_mod
    }
    if(type=="phylo" & taxa=="bee"){  
      mod=pollimetrydata::bee_phy_mod
    }
    if(type=="ITD" & taxa=="bee"){  
      mod=pollimetrydata::bee_IT
    }
    if(type=="taxo" & taxa=="hov"){  
      mod=pollimetrydata::hov_tax_mod
    }
    if(type=="ITD" & taxa=="hov"){  
      mod=pollimetrydata::hov_IT
    }
  }else {
    ###BEES
    if(type=="taxo" & taxa=="bee"){
      if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == FALSE){
        repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_tax_mod.rdata", envir = environment())
        mod=bee_tax_mod}
    }
  }
  
  if(type=="phylo" & taxa=="bee"){
    if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == FALSE){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_phy_mod.rdata", envir = environment())
      mod=bee_phy_mod
    }
  }
  
  if(type=="ITD" & taxa=="bee"){
    if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == FALSE){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_IT.rdata", envir = environment())
      mod=bee_IT}
  }
  
  ###HOVERFLIES
  if(type=="taxo" & taxa=="hov"){
    if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == FALSE){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/hov_tax_mod.rdata", envir = environment())
      mod=hov_tax_mod}
  }
  
  if(type=="ITD" & taxa=="hov"){
    if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == FALSE){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/hov_IT.rdata", envir = environment())
      mod=hov_IT
    }
  }
  #More tests can be implemented e.g. warn depreciated columns (e.g. if Family is provided with type Phylo, explain that it will be depreciated)
  ##OUTPUT
  out <- predict(object=mod,newdata=x,allow_new_levels=TRUE,transform=exp)
  colnames(out)=c("Est.Weight","SE","CI_Lower","CI_Upper")
  out<-cbind(x,out)
  out
  }
