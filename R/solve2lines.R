#' Find solution of 2 intersecting lines
#' @description Find the intersection point of two lines.
#' @param line1 A list containing two coordinate pairs defining the first line segment. Format: \code{list(c(X1,Y1),c(X2,Y2))}.
#' @param line2 A list containing two coordinate pairs defining the second line segment. Format: \code{list(c(X3,Y3),c(X4,Y4))}.
#' @param toplot If\code{TRUE} (the default), then a plot will be generated to show the lines and their intersection point. Plot range is defined by the end points of the lines.
#' @details Developed for the function \code{\link{whalemap}} to test whether a bearing line from a vessel to a whale sighting intersects the shoreline before the horizon. 
#' In that scenario, \code{line1} would be the bearing line between the vessel and the whale. \code{line2} would be formed between two points in a shoreline polygon.
#' This function would be looped to test for intersection between all shoreline points. It does not mess with Great-Sphere distances.
#' @return A 2-element vector, the X and Y of the intersection point. If there is no solution (i.e., parallel lines), both elements are \code{NA}.
#' @seealso \code{\link{whalemap}}
#' @author Eric Keen, Scripps Institution of Oceanography, \email{ekeen@@ucsd.edu} 
#' @examples
#' line1 <- list(c(-129,53),c(-129.5,53.2))
#' line2 <- list(c(-128.5,53.12),c(-129.15,53.62))
#' solve2lines(line1,line2,toplot=TRUE)

solve2lines <- function(line1,line2,toplot=TRUE){
  # Do two linear functions share a solution?
  # Returns a 2-element vector: X and Y of the solution
  # If there is no solution, both elements are NA
  pts.ocean <- line1
  pts.shore <- line2
  
  # Ocean points
  xO1 <- pts.ocean[[1]][1]
  yO1 <- pts.ocean[[1]][2]
  xO2 <- pts.ocean[[2]][1]
  yO2 <- pts.ocean[[2]][2]
  if(xO1==xO2){xO2<-xO2+.0001}
  a <- (yO2-yO1) / (xO2-xO1)
  b = yO1 - a*xO1
  # y = a*x + b
  
  # Shore points
  xS1 <- pts.shore[[1]][1]
  yS1 <- pts.shore[[1]][2]
  xS2 <- pts.shore[[2]][1]
  yS2 <- pts.shore[[2]][2]
  if(xS1==xS2){xS2<-xS2+.0001}
  c <- (yS2-yS1) / (xS2-xS1)
  d = yS1 - c*xS1
  # y = y*x + wS
  
  # Test to see if they are the same line (which means the shore is obstructing the path)
  if(d==c & a==b){
    xtest <- 999 ; ytest <- 999
  }else{
  
  # Test for solution
  xtest = (d-b)/(a-c)
  xreal <- is.finite(xtest) # If xreal= TRUE, then the 2 lines share a solution
  if(xreal){ytest <- a*xtest + b}else{ytest <- NA ; xtest <- NA}
  
  # Graph the two lines
  if(toplot){
    ylims=c(min(c(ytest,52),na.rm=TRUE)-.1,
            max(c(ytest,54),na.rm=TRUE)+.1)
    xlims=c(min(c(xtest,-130),na.rm=TRUE)-.1,
            max(c(xtest,-128),na.rm=TRUE)+.1)
    
    yO = a*xlims + b
    yS = c*xlims + d 
    
    par(mfrow=c(1,1))
    plot(yO~xlims,ylim=ylims,xlim=xlims,type="l",ylab="Y",xlab="X",col="blue",lwd=2)
    par(new=TRUE)
    plot(yS~xlims,ylim=ylims,xlim=xlims,type="l",ann=FALSE,axes=FALSE,col="forest green",lwd=2)
    points(xtest,ytest,cex=2,col="firebrick")
  }
  
  return(c(xtest,ytest))
  } # end of else for test of whether two lines are the exact same
} # end of function



