#' Bootstrap summary
#'
#' @param bootstraps Dataframe of bootstrap results
#'
#' @return Dataframe table
#' @export
#' @import dplyr
#'
bootstrapper_summary <- function(bootstraps){
  
  bootstraps <-
    bootstraps %>%
    group_by(iteration, year, circuit, circuit_name, circuit_yday) %>%
    summarize(dmean = mean(d)) %>%
    ungroup() %>%
    group_by(year, circuit, circuit_name, circuit_yday) %>%
    summarize(SD = sd(dmean),
              L95 = quantile(dmean, .025),
              U95 = quantile(dmean, .975)) %>% 
    ungroup
  bootstraps
  
  return(bootstraps)
}