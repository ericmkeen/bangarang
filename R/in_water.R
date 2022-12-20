#' Are coordinates properly within the water within the KFS?
#'
#' @param xy Dataframe with coordinates, decimal degrees, in columns 'x' and 'y'
#' @param toplot Boolean with default `FALSE`.
#'
#' @return The `xy` dataframe, filtered only to coordinates that are (1) in the KFS and (2) not on land. 
#' @export
#'
in_water <- function(xy,
                     toplot=FALSE){
  #data('whale_sightings')
  #xy <- whale_sightings
  
  data('kfs_land')
  mri <- xy
  #xy <- mri
  mri <- mri[!is.na(mri$x) & !is.na(mri$y),] ; nrow(mri)
  x <- mri$x
  y <- mri$y

  if(toplot){
    plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
    points(x,y,pch=16,cex=.4,col="steelblue3")
  }

  coords = cbind(as.numeric(as.character(x)),as.numeric(as.character(y))) ; coords
  sp = SpatialPoints(coords)
  proj4string(sp) <- proj4string(kfs_land)  
  
  otest <- over( sp , kfs_land , fn = NULL,returnList=FALSE)
  mri$valid <- FALSE
  mri$valid[is.na(otest)] <- TRUE
  table(mri$valid)
  mrin <- mri[mri$valid,] ; nrow(mrin)

  if(toplot){
    points(mrin$x,mrin$y,pch=16,cex=.5,col="firebrick")
  }
  
  return(mri)
}
