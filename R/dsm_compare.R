#' Fit a DSM with permutations of covariates then rank results
#'
#' @param segments  Segments
#' @param observation_data Formatted observation data
#' @param dummy_ds Dummy detection function
#' @param candidate_splines Character vector of candidate splines
#' @param base_formula Base formula
#'
#' @return A list with a slot for ranked summary and a slot for the model objects themselves
#' @export
#'
dsm_compare <- function(segments,
                        observation_data,
                        dummy_ds,
                        candidate_splines,
                        base_formula = 'density.est ~ s(x,y)'){
  
  if(FALSE){
    base_formula <- 'density.est ~ s(x,y)'
    candidate_splines <- c('s(yday)', 's(z)', 'year')
  }
  
  (n <- length(candidate_splines))
  
  # Setup candidate formulae
  i=1
  forms <- data.frame()
  for(i in 1:n){
    (formi <- combn(candidate_splines, i) %>% t %>% as.data.frame %>% apply(., 1, function(x){paste(x, collapse=' + ')}))
    formsi <- data.frame(n = i, form = formi)
    forms <- rbind(forms, formsi)
  }
  forms$formula <- paste(base_formula,' + ', forms$form)
  forms
  
  # Loop through each
  dsm_objects <- list()
  dsm_summary <- data.frame()
  for(i in 1:nrow(forms)){
    message(i, ' of ', nrow(forms),' candidate models')
    (formi <- forms[i,])
    dsmi <- dsm(formula = formula(formi$formula),
                ddf.obj = dummy_ds,
                segment.data = segments,
                observation.data = observation_data,
                convert.units=1/1000,
                family=tw(),
                method = "REML")
    summary(dsmi) %>% names
    formi$model_id <- i
    formi$AICc <- MuMIn::AICc(dsmi)
    formi$dev.expl <- summary(dsmi)$dev.expl
    dsm_summary <- rbind(dsm_summary, formi)
    dsm_objects[[length(dsm_objects) + 1]] <- dsmi
  }
  
  dsm_share <- dsm_summary %>% select(-form) %>% arrange(AICc)
  
  return(list(dsm = dsm_share,
              objects = dsm_objects))
  
}
  
  
  