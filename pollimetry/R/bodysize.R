#' @name bodysize
#' 
#' @title Converts the inter-tegular distance (ITD) to body size (Dry weight mg) for bees or hoverflies.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (2018) using ITD (and co-variate) values.  
#' 
#' @param x A data frame with columns containing ITD values (numeric) and Sex ('Male' or 'Female'). 
#' Optional: Family/Subfamily (character/factor), Region (Only NorthAmerica, SouthAmerica, Australasia and Europe) and Species ("Genus_species")
#'
#' @param taxa A vector specifying taxa of interest, can be either "bee" for bee models and "hov" for hoverfly models
#' 
#' @param type A vector specifying model type to be used: for bees it can be either "tax" for taxonomic models, "phy" for phylogenetic model or "IT" for ITD-only model. In hoverflies: it can either be "h1" for Weight ~ IT + Sex or "h2" for Weight ~ IT * Subfamily + Sex.
#' 
#' @return The original dataframe (x) is returned along with four additional columns: body size (dry weight (mg)), S.E. and 95% confidence intervals.
#' 
#' @examples
#' example=cbind.data.frame(IT=1:2,
#' Taxa=c("bee","bee"), 
#' Sex=c("Female","Male"), 
#' Family=c("Apidae","Andrenidae"),
#' Region=c("NorthAmerica","Europe"),
#' Species=c("Ceratina_dupla","Andrena_flavipes"))
#' bodysize(x=example,taxa="bee",type="taxo")
#' 
#' @references Kendall, Rader, Gagic... Bartomeus (2018) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' 
#' @export
#' 
bodysize=function(x,taxa,type) {
  check_Region <- x$Region %in% c("Australasia","NorthAmerica","SouthAmerica","Europe")
  if(any(check_Region == FALSE)){
    warning("Only the following regions are supported currently; 'Australasia','NorthAmerica','SouthAmerica' and 'Europe'. Model will only consider population-level (fixed) effects")
  }
  check_Sex <- x$Sex %in% c("Female","Male")
  if(any(check_Sex==FALSE)){
    warning("Sex should be either 'Female' and/or 'Male'")
  } 
  ##Incorrect type and taxa combos
  if(type=="phylo" & taxa =="hov"){
    stop("Bad combination: No phylogenetic model implemented for hoverflies yet!")
  } else {
    ###BEES
    if(type=="taxo" & taxa=="bee") {
      load("pollimetry/data/bee_tax_mod.Rdata")
      mod=bee2 #was second most complex hence name
      } 
    if(type=="phylo" & taxa=="bee") {
      load("pollimetry/data/bee_phy_mod.Rdata")
      mod=bee_p1 #most complex 
      }
    if(type=="IT" & taxa=="bee") {
      load("pollimetry/data/bee_IT.Rdata")
      mod=bee_p3 #PHYLO IT only
      }
    ###HOVERFLIES
    if(type=="h1" & taxa=="hov") {
      load("pollimetry/data/hov_mod.Rdata")
      mod=hov2 #TOp RANKED IT + SEx
      } 
    if(type=="h2" & taxa=="hov") {
      load("pollimetry/data/hov_tax_mod.Rdata")
      mod=hov7 #2nd IT*SUBFAMILY + Sex
      } 
    if(type=="IT" & taxa=="hov") {
      load("pollimetry/data/hov_IT.Rdata")
      mod=hov1 #IT only
      } 
    ##OUTPUT
    out = predict(object=mod,newdata=x,allow_new_levels=TRUE,transform=exp)
    rownames(out)=rownames(x)
    colnames(out)=c("Est.Weight","SE","CI_Lower","CI_Upper")
    out=cbind(x,out)
    out
  }
}

