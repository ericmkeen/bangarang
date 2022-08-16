#' Assign coordinates to KFS block
#'
#' @param x Longitude, decimal degrees.
#' @param y Latitude, decimal degrees. 
#' @param toplot Boolean with default `FALSE`.
#'
#' @return
#' @export
#'
in_block <- function(x,y,toplot=FALSE){
  #x <- -129.5
  #y <- 53.2
  data('kfs_blocks_bbox')
  blox <- kfs_blocks_bbox
  inblock <- rep("",times=length(x))
  i=12
  for(i in 1:nrow(blox)){
    bloxi <- blox[i,] ; bloxi
    px <- c(bloxi$left,bloxi$left,bloxi$right,bloxi$right,bloxi$left)
    py <- c(bloxi$bottom,bloxi$top,bloxi$top,bloxi$bottom,bloxi$bottom)

    if(toplot){
      bangarang::plotKFS()
      points(x,y,col="grey",cex=.4,pch=16)
      lines(px,py)
    }

    pipresult <- sp::point.in.polygon(x,y,px,py)
    ins <- which(pipresult>0)

    if(toplot){
      points(x[ins],y[ins],col="firebrick",cex=.6,pch=16)
    }

    (inblock[ins] <- paste0(inblock[ins],"-",bloxi$id))

  }
  inblock
  return(inblock)
}
