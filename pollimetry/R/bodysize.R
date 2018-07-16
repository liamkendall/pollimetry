#' @name bodysize
#' 
#' @title Converts the intertegular distance (ITD) and co-variates to body size (dry weight (mg)) for bees or hoverflies.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (2018) using ITD (and co-variate) values.  
#' 
#' @param x A data frame with columns containing ITD values and Sex ('Male' or 'Female'). 
#' Optional: Family/Subfamily, Region (Only NorthAmerica, SouthAmerica, Australasia and Europe) and Species ("Genus_species")
#'
#' @param taxa A vector specifying insect taxa of interest, can be either "bee" for bee models and "hov" for hoverfly models
#' 
#' @param type A vector specifying model type to be used: for bees this can be either "tax" for taxonomic models, "phy" for phylogenetic model or "IT" for ITD-only model. In hoverflies: it can either be "h1" for Weight ~ IT + Sex, "h2" for Weight ~ IT * Subfamily + Sex or IT for ITD-only model.
#' 
#' @return The original dataframe (x) is returned along with four additional columns: body size (dry weight (mg)), S.E. and 95% confidence intervals.
#' 
#' @importFrom stats predict
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
      load("pollimetry/inst/testdata/bee_tax_mod.rda")
      mod=bee_tax_mod
      } 
    if(type=="phylo" & taxa=="bee") {
      load("pollimetry/inst/testdata/bee_phy_mod.rda")
      mod=bee_phy_mod #most complex 
      }
    if(type=="IT" & taxa=="bee") {
      load("pollimetry/inst/testdata/bee_IT.rda")
      mod=bee_IT #PHYLO IT only
      }
    ###HOVERFLIES
    if(type=="h1" & taxa=="hov") {
      load("pollimetry/inst/testdata/hov_mod.rda")
      mod=h1 #Top RANKED IT + SEx
      } 
    if(type=="h2" & taxa=="hov") {
      load("pollimetry/inst/testdata/hov_tax_mod.rda")
      mod=h2 #2nd IT*SUBFAMILY + Sex
      } 
    if(type=="IT" & taxa=="hov") {
      load("pollimetry/inst/testdata/hov_IT.rda")
      mod=hov_IT #IT only
      } 
    ##OUTPUT
    out = predict(object=mod,newdata=x,allow_new_levels=TRUE,transform=exp)
    rownames(out)=rownames(x)
    colnames(out)=c("Est.Weight","SE","CI_Lower","CI_Upper")
    out=cbind(x,out)
    out
  }
}
