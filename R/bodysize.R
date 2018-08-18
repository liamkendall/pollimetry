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
#' but is only recommended to be used for species (and regions) included in model formulation. 
#' Please run `setdiff` using `bee_mean_dataset` for region and species to check matches.
#'  For hoverflies, type 'h1' requires ITD and Sex for each specimen 
#'  and type 'h2' requires ITD, Sex and subfamily. Type "IT" for both 
#'  taxa only requires ITD values (Optional: region and species but again check `setdiff`). 
#'  If specimens are from included regions or species (see above) we recommend 
#'  including these as additional columns. Estimates (and variance components) are 
#'  returned as four additional columns bound to the original dataframe.
#' 
#' @importFrom stats predict
#' 
#' @import brms
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
  check_Region <- x$Region %in% c("Australasia","NorthAmerica","SouthAmerica","Europe")
  if(any(check_Region == FALSE)){
    warning("Only the following regions are supported currently; 'Australasia','NorthAmerica','SouthAmerica' and 'Europe'. Model will only consider population-level (fixed) effects")
  }
  check_Sex <- x$Family %in% c("Female","Male")
  if(any(check_Sex==FALSE)){
    warning("Sex should be either 'Female' and/or 'Male'")
    #LIAM, WHAT THE MODEL DO WHEN Sex IS Unknown?? 
    #Right now it fails! Hence, either we allow the model to run an average, or we stop the function here if Sex has a wrong value. and provide an informative error message.
  } 
  if(type = "taxo"){
    check_taxo <- x$Family %in% c("xxxx","xxxx")
    if(any(check_taxo==FALSE)){
      warning("Family should be either 'xxxx', 'xxxx'")
      #LIAM, WHAT THE MODEL DO WHEN FAMILY IS Unknown?? Now fails. Do same as with Sex.
    } 
  }
  if(type = "phylo"){
    check_sp <- x$Species %in% c("xxxx","xxxx") #in original data? Call the Rdata object?
    if(any(check_sp==FALSE)){
      warning("Species should be either contained into the phylogeny, try type = 'taxo'")
      #LIAM, WHAT THE MODEL DO WHEN species IS Unknown even in taxo models?? Explain as in Region. 
      #This general species test should be addedd at the main level, not only for phylo!
    } 
  }
  ##Incorrect type and taxa combos
  if(type=="phylo" & taxa =="hov"){
    stop("Bad combination: No phylogenetic model implemented for hoverflies yet!")
  } else {
    ###BEES
    if(type=="taxo" & taxa=="bee") {
            mod=bee_tax_mod
      } 
    if(type=="phylo" & taxa=="bee") {
            mod=bee_phy_mod #most complex 
      }
    if(type=="IT" & taxa=="bee") {
            mod=bee_IT #PHYLO IT only
      }
    ###HOVERFLIES
    if(type=="h1" & taxa=="hov") {
            mod=h1 #Top RANKED IT + SEx
      } 
    if(type=="h2" & taxa=="hov") {
           mod=h2 #2nd IT*SUBFAMILY + Sex
      } 
    if(type=="IT" & taxa=="hov") {
           mod=hov_IT #IT only
    } 
    #More tests can be implemented e.g. warn depreciated columns (e.g. if Family is provided with type Phylo, explain that it will be depreciated)
    ##OUTPUT
    out = predict(object=mod,newdata=x,allow_new_levels=TRUE,transform=exp)
    colnames(out)=c("Est.Weight","SE","CI_Lower","CI_Upper")
    out=cbind(x,out)
    out
  }
}

