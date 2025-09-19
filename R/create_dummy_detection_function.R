#' Create dummy detection function
#'
#' @param observation_data Sightings, formatted
#'
#' @return Detection function object from package `dsm` 
#' @export
#'
create_dummy_detection_function <- function(observation_data){
  
  dummy_ds <-
    dsm::dummy_ddf(object = observation_data$object,
                        size = observation_data$size,
                        width = 150,
                        left = 0,
                        transect = 'line')
  
  return(dummy_ds)
}