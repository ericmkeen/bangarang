#' Random draw from truncated log-normal distribution based on min, best, max group size estimte
#'
#' @param sightings Dataframe of sightings
#'
#' @return Modified sightings dataframe, with the `best` column updated with the random draws.
#' @export
#' @import dplyr
#'
group_size_draw <- function(sightings){
  
  sightings <- 
    sightings %>% 
    rowwise %>% 
    mutate(grp_data = ifelse(all(c(!is.na(best), !is.na(min), !is.na(max))), TRUE, FALSE)) %>% 
    mutate(best_ln = ifelse(grp_data == TRUE,
                            ifelse(min != max, 
                                   EnvStats::rlnormTrunc(1, meanlog = best, sdlog = sd(c(min, best, max)), min = min, max = max),
                                   min),
                            best)) %>% 
    mutate(best = best_ln) %>% 
    ungroup %>% 
    select(-best_ln, )
    
  return(sightings)
  
}