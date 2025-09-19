#' Format sightings into observation data for `dsm`
#'
#' @param sightings Dataframe of Bangarang sightings.
#'
#' @return Observation data formatted.
#' @export
#' @import dplyr
#'
format_observation_data <- function(sightings){
  
  observation_data <-
    sightings %>%
    mutate(object = 1:n()) %>%
    mutate(size = best) %>%
    rowwise %>%
    mutate(size = ifelse(is.na(size), 1, size)) %>%
    ungroup %>%
    mutate(distance = ifelse(zone == '1', 75,
                             ifelse(zone == '2', 150,
                                    ifelse(zone == '221', 150, NA))))
  return(observation_data)
}