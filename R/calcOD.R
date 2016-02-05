#' Calculated total travel distance in a dataframe of turns

calc.OD <- function(turns){
  library(swfscMisc)
  # take output dataframe from ocean.route() and calculate total distance
  dist <- vector()
  for(i in 1:(nrow(turns)-1)){
    lat1 <- turns$Y[i]
    lon1 <- turns$X[i]
    lat2 <- turns$Y[i+1]
    lon2 <- turns$X[i+1]
    disti <- distance(lat1,lon1,lat2,lon2,method="Vincenty",units="km")
    dist <- c(dist,disti)
  }
  tot <- sum(dist)
  return(tot)
}