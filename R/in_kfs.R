#' Are coordinates within the KFS?
#'
#' @param xy Dataframe with coordinates, decimal degrees, in columns 'x' and 'y'
#' @param toplot Boolean with default `FALSE`.
#'
#' @return The `xy` dataframe, filtered only to coordinates that are in the KFS. 
#' @export
#'
in_kfs <- function(xy,
                   toplot=FALSE){
  #data('whale_sightings')
  #xy <- whale_sightings
  
  data('kfs_boundary')
  mri <- xy
  mri <- mri[!is.na(mri$x) & !is.na(mri$y),] ; nrow(mri)
  x <- mri$x
  y <- mri$y

  if(toplot){
    bangarang::plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
    points(x,y,pch=16,cex=.4,col="steelblue3")
  }

  coords = cbind(as.numeric(as.character(x)),as.numeric(as.character(y))) ; coords
  sp = SpatialPoints(coords)
  proj4string(sp) <- proj4string(kfs_boundary)  
  
  otest <- over( sp , kfs_boundary , fn = NULL, returnList=FALSE)
  mri$inkfs <- TRUE
  mri$inkfs[is.na(otest)] <- FALSE
  table(mri$inkfs)
  mrin <- mri[mri$inkfs,] ; nrow(mrin)

  if(toplot){
    points(mrin$x,mrin$y,pch=16,cex=.5,col="firebrick")
  }
  return(mri)
}
