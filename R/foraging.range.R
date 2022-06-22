#' @name foraging.range
#' 
#' @title Predict foraging range for bees using intertegular distance and degree of sociality
#' 
#' @description Calculates realized* foraging range from Kendall et al. (2022) using intertegular distance (ITD) values
#'  and degree of sociality (highly eusocial, primitively eusocial or solitary). *Potential estimates are not recommended to be used but can be obtained by selecting `measure.type = "potential"`.
#'  
#' @param data  A dataframe containing between two to four columns: bee intertegular distance (ITD) measurements in mm, species (Format: "Genus_species") and sociality (Format: "Highly Eusocial","Primitively Eusocial", "Solitary").
#' 
#' @param random.effects A sting stating if random effects are to be used using predictive purposes. Options are 1) phylogenetic effect and species effects "full", 
#' 2) only phylogenetic effect ("phylo"), or 3) fixed effects only ("reduced"). Default is "reduced".
#' 
#' @param model.type A string stating which model should be used: "social", for the model that includes sociality, body size (ITD) and measurement type, or "ITD", for the reduced model that only includes body size and measurement type.
#' 
#' @param measure.type A string stating whether to return realized ("realized"), potential ("potential") estimates. 
#' 
#' @return A dataframe with bee foraging range (km) is returned for each bee species (row).
#'
#' @importFrom stats predict
#' 
#' @importFrom stringr word
#' 
#' @import brms
#' 
#' @import dplyr
#' 
#' @examples
#' ITD=c(2.9,2.9,2.9)
#' species=c("Apis_mellifera","Osmia_cornifrons","Bombus_bifarius")
#' sociality=c("Highly Eusocial","Solitary","Primitively Eusocial")
#' example.data <- data.frame(ITD,species,sociality)
#' foraging.range(example.data,random.effects = "full",measure.type = "realized")
#' 
#' @references \itemize{
#' \item Kendall et al. (2022) The potential and realized foraging movements of bees are differentially determined by body size and sociality \emph{Ecology} \url{TBC}.
#' }
#'
#' @export

foraging.range <- function(data,
                           model.type = "social",
                           random.effects = "reduced",
                           measure.type = "realized"){
  
  all_IT_bm_mod <- all_social_bm_mod <- NULL
  
  Est.Error  <- Estimate  <- Q2.5  <- Q97.5  <- pollimetrydata  <- range.4  <- range.type <- species <- size.out <- wgt10 <- NULL
  
  data.nrow = nrow(data)
  data.out = data %>% 
    dplyr::slice(rep(1:dplyr::n(), each = 4)) %>% 
    dplyr::mutate(genus=stringr::word(species,1,sep="_")) %>% 
    dplyr::mutate(range.4=rep(c("Typ-Realised",
                         "Typ-Potential",
                         "Max-Realised",
                         "Max-Potential"),data.nrow))
  
  measures.out = ifelse(rep(measure.type,2)%in%"realized",
                        c("Realized typical range",
                          "Realized maximum range"),
                        c("Potential typical range",
                          "Potential maximum range"))
  
  
  if(!model.type %in% c("social","ITD")) {
    stop("model.type should be one of 'social' or 'ITD'")
  }
  if(!random.effects %in% c("full","phylo","reduced")) {
    stop("random.effects should be one of 'full','phylo' or 'reduced'")
  }
  if(!measure.type%in% c("potential","realized")){
    stop("measure.type should be 'realized' or 'potential'")
  }
  if(measure.type %in% c("potential")){
    warning("Potential estimates are not recommeded for predictive purposes, 
             consider using realized estimates",call. = FALSE)
  }
  if(random.effects %in%c("full") & any(grepl("species",colnames(data)))==FALSE){
    stop("species not provided. We suggest using random.effects = 'phylo' or 'reduced'")
  }
  if(requireNamespace('pollimetrydata')){

      it.mod=pollimetrydata::all_IT_bm_mod
      soc.mod=pollimetrydata::all_social_bm_mod
      
  }else if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/all_IT_bm_mod.rdata",
                          envir = environment())
    it.mod <- all_IT_bm_mod
      
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/all_social_bm_mod.rdata",
                            envir = environment())
        soc.mod=all_social_bm_mod
 
  }
  size.out <- suppressWarnings(bodysize(data.frame(ITD = data.out$ITD,Sex = "Female"),
                      type="sex",taxa="bee"))
  data.out$wgt <- size.out[,c("Est.Weight")]
  data.out$wgt10 <- log10(data.out$wgt)
  
  ###missing species
  miss_sp <- setdiff(data.out$species,it.mod$data$species)
  miss_gen<- setdiff(data.out$genus,it.mod$data$genus)
  
  if(random.effects == "full" & 
     length(c(miss_sp)>0)){
    warning(paste("Some species not represented in model set:",miss_sp),call. = FALSE)
  }
  if(random.effects == "phylo" & 
     length(c(miss_gen)>0)){
    warning(paste("Some genera not represented in model set:",miss_gen),call. = FALSE)
  }
  
  if(model.type == "ITD"){
    
  out <- exp(predict(it.mod,
                     newdata=data.out,
                     allow_new_levels=T,
                     re_formula = ifelse(random.effects=="full",
                                         "~(1|genus)+(1|species)",
                                         ifelse(random.effects=="phylo",
                                         "~(1|genus)",
                                         NA))))
  
  } else if(model.type == "social"){
  
  out <- exp(predict(soc.mod,
                     newdata=data.out,
                     allow_new_levels=T,
                     re_formula = ifelse(random.effects=="full",
                                         "~(1|genus)+(1|species)",
                                         ifelse(random.effects=="phylo",
                                         "~(1|genus)",
                                         NA))))
  
  }
  
  out2 <- cbind(data.out,out) %>% 
    dplyr::mutate(range.type=plyr::revalue(range.4,
                              c("Typ-Realised"  = "Realized typical range",
                                "Typ-Potential" = "Potential typical range",
                                "Max-Realised"  = "Realized maximum range",
                                "Max-Potential" = "Potential maximum range"))) %>% 
    dplyr::select(-range.4,-Est.Error,wgt10) %>% 
    dplyr::relocate(range.type,.before=Estimate)%>%
    dplyr::rename(range.estimate=Estimate,
           range.lower=Q2.5,
           range.upper=Q97.5) %>% 
    dplyr::filter(range.type%in%measures.out)
  
  out2
}


 
  