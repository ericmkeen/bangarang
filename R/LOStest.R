#' Line-of-Sight test for two water locations 
#' @description Determines whether land is obstructing a direct path between two pairs of ocean coordinates.
#' @param pts.ocean A list containing two coordinate pairs. Format: \code{list(c(X1,Y1),c(X2,Y2))}.
#' @param buffer Distance (in decimal degrees) to increase the boundaries of shoreline under consideration beyond the range defined by the two points. 
#' @param toplot If \code{TRUE} (the default), then two plot will be generated to show the process. 
#' The top plot is a "context" plot showing the two points.  
#' The bottom plot is a zoom of the two points. A blue line connects them, 
#' and the green line along the shore indicates shoreline coordinates that have been tested for line-of-sight obstruction.
#' When an obstruction has been found, a circle marks the shoreline intersection point (corresponding to element two of returned list).
#' @details A test function used frequently throughout the \code{routeKFS} function, but can be used on its own for any part of the 
#' northeast Pacific coastline (Oregon to Alaska). The function takes a shoreline dataset from the package \code{PBSmapping},
#' and explores nearby shoreline vertices (in pairs) to see if the line connecting them intersects 
#' with a straight path between the two ocean points in question. Then, if the solution of the two lines
#' is contained with a box that bounds the two shoreline vertices, the function concludes that 
#' the shore must be obstructing line of sight.
#' @return Returns a two element list. The first list is a Boolean: If \code{TRUE}, 
#' then the two points are indeed line-of-sight with no shore obstructions. If \code{FALSE}, 
#' then the land is obstructing the most direct path between the points. 
#' The second element in the list is a 2-element vector, the X and Y of the intersection point with shore.
#' This vector is \code{c(NA,NA)} when the two points are line of sight. 
#' @seealso \code{\link{whalemap}}, \code{\link{routeKFS}}, \code{\link{solve2lines}}
#' @author Eric Keen, Scripps Institution of Oceanography, \email{ekeen@@ucsd.edu} 
#' @examples
#' ### Define the two ocean pts
#' xO1 <- -129.3
#' yO1 <- 53.3
#' xO2 <- -129.23
#' yO2 <- 53.33
#' # Combine into list
#' pts.ocean <- list(c(xO1,yO1),c(xO2,yO2))
#' # Run the test:
#' LOStest(pts.ocean)
#####################################################################
#####################################################################
LOStest <- function(pts.ocean,
                    buffer=.025,
                    toplot=TRUE){  
  # Line Of Sight (LOS) test between two coordinate pairs
  library(PBSmapping) ; data(nepacLLhigh)
  library(swfscMisc)
  
  # Plot the two points
  pt1 <- pts.ocean[[1]]
  pt2 <- pts.ocean[[2]]
  
  EID <- c(1,2)
  X <- c(pt1[1],pt2[1])
  Y <- c(pt1[2],pt2[2])
  puntos <- data.frame(EID,X,Y)
  puntos <- as.EventData(puntos,projection="LL") # format for mapping
  pts.ocean <- list(c(X[1],Y[1]),c(X[2],Y[2])) # format for line solving
  
  # Determine bounds of area encompassed by two points
  xlimraw=c((min(X,na.rm=TRUE)),(max(X,na.rm=TRUE)))
  ylimraw=c((min(Y,na.rm=TRUE)),(max(Y,na.rm=TRUE)))
  
  xlims=c(xlimraw[1]-buffer,xlimraw[2]+buffer)
  ylims=c(ylimraw[1]-buffer,ylimraw[2]+buffer)
  
  if(toplot){
    # Study area context
    par(mfrow=c(2,1), mar=c(.1,.1,.1,.1))
    plotKFS()
    addPoints(puntos)
    
    # Zoomed in
    plotKFS(area="Other",X=xlims,Y=ylims)
    addPoints(puntos)
  }
  
  # Limit the shoreline coordinates to those occurring within the rectangle that includes that line
  nearbyshore <- nepacLLhigh[nepacLLhigh$X >= xlims[1] & 
                               nepacLLhigh$X <= xlims[2] & 
                               nepacLLhigh$Y >= ylims[1] & 
                               nepacLLhigh$Y <= ylims[2]
                             ,]
  # LOS TEST
  LOS <- TRUE
  intersection <- vector()
  
  if(nrow(nearbyshore)>1){
    
    if(toplot){
      # Demonstrate on the map the two points connected
      PID <- c(1:1) ; POS <- 1:2 ; showpts <- as.PolySet(data.frame(PID,POS,X,Y),projection="LL")
      addLines(showpts,col="blue",lwd=2)
    }
    
    # For all adjacent pairs of shoreline
    for(i in 1:(nrow(nearbyshore)-1)){
      if(LOS & nearbyshore$PID[i]==nearbyshore$PID[i+1]){ 
        #dont compare shoreline pts from separate polygons
        # calculations should only run if we haven't hit shore yet
        # Test for solution
        Xshore <- c(nearbyshore$X[i], nearbyshore$X[i+1])
        Yshore <- c(nearbyshore$Y[i], nearbyshore$Y[i+1])
        if(toplot){
          # Demonstrate on the map which shore segment is being tested
          PID <- c(1:1) ; POS <- 1:2 ; X <- Xshore ; Y <- Yshore 
          showshore <- as.PolySet(data.frame(PID,POS,X,Y),projection="LL")
          addLines(showshore,lwd=2,col="forest green")
        }
        pts.shore <- list(c(Xshore[1],Yshore[1]),c(Xshore[2],Yshore[2]))
        solved <- solve2lines(pts.ocean,pts.shore,toplot=FALSE)
        Xsolved <- solved[1]
        Ysolved <- solved[2]
        
        # If solution is within range and finite, LOS=FALSE
        if(is.finite(Xsolved) & is.finite(Ysolved)){
          # Account for horizontal or vertical lines in pts.ocean
          if(diff(range(xlimraw))==0){xlimraw[1] <- xlimraw[1]-.005 ; xlimraw[2] <- xlimraw[2]+.005} 
          if(diff(range(ylimraw))==0){ylimraw[1] <- ylimraw[1]-.005 ; ylimraw[2] <- ylimraw[2]+.005}
          # Account for horizontal or vertical lines in pts.shore
          shore.xlims = c(min(Xshore,na.rm=TRUE),max(Xshore,na.rm=TRUE))
          shore.ylims = c(min(Yshore,na.rm=TRUE),max(Yshore,na.rm=TRUE))
          if(diff(range(shore.xlims))==0){shore.xlims[1] <- shore.xlims[1]-.005 ; shore.xlims[2] <- shore.xlims[2]+.005} 
          if(diff(range(shore.ylims))==0){shore.ylims[1] <- shore.ylims[1]-.005 ; shore.ylims[2] <- shore.ylims[2]+.005}
          # Test to see if solution within bounding rectangle of 2 ocean pts
          if(Xsolved>=xlimraw[1] & Xsolved<=xlimraw[2] &
             Ysolved>=ylimraw[1] & Ysolved<=ylimraw[2]){
            # Test to see if solution is within bounding rectangle of 2 shore points
            if(Xsolved >= shore.xlims[1] & Xsolved <= shore.xlims[2] & 
               Ysolved >= shore.ylims[1] & Ysolved <= shore.ylims[2]){
              LOS <- FALSE ; i
              intersection <- c(Xsolved,Ysolved)
            }
          }
       }
        
        # If this is the intersection point, plot it
        if(!LOS & toplot){
          EID <- 1 ; X <- Xsolved ; Y <- Ysolved
          shorecross <- as.EventData(data.frame(EID,X,Y),projection="LL")
          addPoints(shorecross)
        }
        
      } # end of if(LOS)
    } # end of for loop
    
  } # End of if(nrow(nearbyshore)>1)
  
  if(LOS){intersection <- c(NA,NA)}
  par(mfrow=c(1,1))
  return(list(LOS=LOS,intersection=intersection))

} 