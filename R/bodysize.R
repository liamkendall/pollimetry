#' @name bodysize
#' 
#' @title Converts the intertegular distance (ITD) and co-variates to body size in dry weight (mg) for bees or hoverflies.
#' 
#' @description Calculates body size in dry weight (mg) from Kendall et al. (2018) using ITD (and co-variate) values.  
#' 
#' @param x A data frame with columns containing IT values and Sex ('Male' or 'Female'). 
#' Optional attributes depending on model choice: Taxonomic family (bees) or subfamily (hoverfly), Region (Only "NorthAmerica", "SouthAmerica", "Australasia" and "Europe" implemented) and Species ("Genus_species"). For non implemented regions, the model will work, but only consider population-level (fixed) effects. 
#'
#' @param taxa A vector specifying insect taxa of interest, can be either "bee" for bee models and "hov" for hoverfly models
#' 
#' @param type A vector specifying model type to be used: for bees this can be either "taxo" for taxonomic models, "phy" for phylogenetic model or "IT" for ITD-only model. In hoverflies: it can either be "h1" for Weight ~ IT + Sex, "h2" for Weight ~ IT * Subfamily + Sex or "IT" for ITD-only model.
#' 
#' @return The original dataframe (x) is returned along with four additional columns: body size (dry weight (mg)), S.E. and 95 perent confidence intervals.
#' 
#' @details For bees, type option 'taxo' requires ITD, 
#' sex and taxonomic family.  Type option 'phylo' only requires ITD and Sex to run 
#' but should be only be used for with Species (and Region) included in model formulation n.b. the function checks for contained species.  
#' 
#'  For hoverflies, type 'taxo' requires ITD, Subfamily and Sex for each specimen. Type "IT" for both 
#'  taxa only requires ITD values (Optional: region and species but check `setdiff`). 
#'  If specimens are from included regions or species (see above) we recommend 
#'  including these as additional columns. Estimates (and variance components) are 
#'  returned as four additional columns bound to the original dataframe. In the likely case that non-represented taxa and regions are included in inputted datasets, `allow_new_levels` is set to true for all models. Estimates will be then averaged after random variance components.
#' 
#' @importFrom stats predict
#' 
#' @import brms
#' 
#' 
#' @examples
#' example=cbind.data.frame(IT=1:2,
#'                          Sex=c("Female","Male"), 
#'                          Family=c("Apidae","Andrenidae"),
#'                          Region=c("NorthAmerica","Europe"),
#'                          Species=c("Ceratina_dupla","Andrena_flavipes"))
#' bodysize(x=example,taxa="bee",type="taxo")
#' 
#' @references Kendall, Rader, Gagic... Bartomeus (2018) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' 
#' @export
bodysize=function(x,taxa,type) {
  data("pollimetry_dataset", envir = environment())
  check_sp <- x$Species %in% pollimetry_dataset$Species 
  if(any(check_sp==FALSE)){
    warning("Some species are different from those used in model formulation, for those, 
            Model will only consider population-level (fixed) effects")
  } 
  check_Region <- x$Region %in% c("Australasia","NorthAmerica","SouthAmerica","Europe")
  if(any(check_Region == FALSE)){
    warning("Only the following regions are supported currently; 'Australasia','NorthAmerica','SouthAmerica' and 'Europe'. 
            Model will only consider population-level (fixed) effects")
  }
  if(type == "taxo"){
    check_taxo <- x$Family %in% c("Andrenidae","Apidae","Colletidae","Halictidae","Melittidae","Megachilidae")
    if(any(check_taxo == FALSE)){
      stop("Family should be either 'Andrenidae','Apidae','Colletidae',Halictidae','Megachilidae','Melittidae'.
           If family is unknown, use type = 'IT'.")
    }
  }
  if(type %in% c("taxo", "phylo")){
  if("Sex" %in% colnames(x)==FALSE) {
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'")
    }
  }  
  check_Sex <- x$Sex %in% c("Female","Male")
  if(any(check_Sex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'")
  }
  ##Incorrect type and taxa combos
  if(type=="phylo" & taxa =="hov"){
    stop("Bad combination: No phylogenetic model implemented for hoverflies yet!")
    } else if(rownames <- "pollimetrydata" %in% rownames(installed.packages()) == TRUE) {
    if(type=="taxo" & taxa=="bee"){
      data(bee_tax_mod, envir = environment())
      mod=bee_tax_mod
    }
    if(type=="phylo" & taxa=="bee"){  
      data(bee_phy_mod, envir = environment())
      mod=bee_phy_mod
    }
    if(type=="IT" & taxa=="bee"){  
      data(bee_IT, envir = environment())
      mod=bee_IT
    }
    if(type=="taxo" & taxa=="hov"){  
      data(hov_tax_mod, envir = environment())
      mod=hov_tax_mod
    }
    if(type=="IT" & taxa=="hov"){  
      data(hov_IT, envir = environment())
      mod=hov_IT
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
     
      if(type=="IT" & taxa=="bee"){
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

if(type=="IT" & taxa=="hov"){
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
