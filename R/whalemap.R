#' Find true position of sighting 
#' @description Calculates the true position of a sighting in the northeast Pacific, in offshore or confined coastal waters.
#' @param X  Observer longitude (decimal degrees, where W is negative, e.g. -129.3 for the Kitimat Fjord System).
#' @param Y Observer latitude (decimal degrees).
#' @param bearing Magnetic bearing to sighting, in degrees. This is true bearing, not referenced to the vessel's heading.
#' @param reticle Reticle reading (must be positive). #' @param eye.height Height above sea level of observer's eyes, in meters.
#' @param vessel.hdg The vessel's heading at the time of the sighting (Optional). It is assumed this information is 
#' taken from a GPS, and therefore no magnetic declination correction is applied to it. 
#' If provided, trackline distance to the sighting will be calculated, which assumes that the vessel is
#' headed along the trackline at the time of sighting.
#' @param deg.per.ret Reticle-to-degree conversion. Default is 0.279, 
#' which is specific to Fujinon 7x50 binocs (see references). 
#' On the \emph{Bangarang}, we treated a full reticle as the distance between 
#' long and short reticle lines in the optic of our Fujinon FMTRC-SX 7x50 binocs 
#' (after Kinzey \& Gerrodette 2001).
#' This value needs to replaced with the correct conversion for the optics you use.
#' @param mag Magnetic declination, in degrees, used to correct bearing readings from your magnetic compass.
#' Default is -18.25 deg, which is the approximate declination in the Kitimat Fjord System, British Columbia.
#' @param toplot When TRUE (not the default), a map will be generated that shows the process at work.
#' @details  This function can work offshore or within confined coastal zones, as tests to see whether the reticle
#' reading is referenced to the horizon or to shore. Also calculates the trackline distance of the
#' sighting, if vessel heading is provided, for use in distance sampling analysis. 
#' @references
#' \enumerate{
#'   \item Kinzey, D. and Gerrodette, T. 2001. Conversion factors for binocular reticles. 
#'   Mar. Mammal Sci. 17(2):353-61.
#'   \item Lerczak, J.A. and Hobbs, R.C. 1998. Calculating sighting distances 
#' from angular readings during shipboard, aerial, and shore-based marine mammal surveys. 
#' Mar. Mammal Sci. 14(3):590-9. [See Errata. 1998. Mar. Mammal Sci. 14(4):903]. 
#' }
#' @return   Returns a list with the following elements:
#' \describe{
#' \item{\code{$X}}{X of animal}
#' \item{\code{$Y}}{Y of animal} 
#' \item{\code{$radial.dist}}{Distance from observer to animal, in km (radial distance)}
#' \item{\code{$boundary}}{Reference boundary used (\code{"shore"} or \code{"horizon"})}
#' \item{\code{$boundary.dist}}{Observer's distance to boundary, in km}
#' \item{\code{$perp.dist}}{Trackline distance to sighting, in km (if vessel heading was provided). Ignores curvature of the earth.}
#' \item{\code{$track.bearing}}{Difference between sighting bearing and vessel heading, in degrees (simple subtraction)}
#' }
#' If \code{topot=TRUE} (not the DEFAULT), the function also generates a map key of the process. Key: 
#' Black dot = Observer position;  
#' Blue cross = Location of the horizon along the bearing line, based on eye height;  
#' Blue line = Bearing line, from observer to the horizon based on eye height;  
#' Green squares = (If any) Show nearby shoreline segments that were tested for obstruction of the bearing line;   
#' Green lines = (If any) they show the shoreline segments that were tested for intersection with the bearing line before an intersection was found;  
#' Dotted line = Vessel heading;  
#' Red circles = Location of whale! Go cats!
#' 
#' @seealso \code{\link[bangarang]{solve2lines}}
#' @author Eric Keen, Scripps Institution of Oceanography, \email{ekeen@@ucsd.edu} 
#' @examples
#' ###### SHORE EXAMPLE ######
#' # You are on your way to Cameron Cove 
#' # and see a whale in Taylor Bight:
#' whalemap(-129.21,53.0855,
#'         bearing=60,
#'         reticle=.1,
#'         eye.height=3.46,
#'         vessel.hdg=90,
#'         toplot=TRUE)
#'         
#' ### HORIZON EXAMPLE ###############
#' # You are on your way back from the Wall 
#' # and you see the bubble net group!
#' whalemap(X=-129.35577392578125,
#'          Y=52.860217383433515,
#'          bearing=14,
#'          reticle=.01,
#'          eye.height=3.46,
#'          vessel.hdg=45,
#'          toplot=TRUE)       

whalemap <- function(X,Y,
                     bearing,
                     reticle,
                     eye.height,
                     vessel.hdg=NA,
                     mag=-18.25,
                     deg.per.ret=0.279,
                     toplot=FALSE){
  if(is.na(X) |
       is.na(Y) |
       is.na(bearing) |
       is.na(eye.height)){
    Xsit = X
    Ysit = Y
    sitdist = NA
    distused = "vessel"
    refdist = NA
    trackdist = NA
    trackbear = NA
  }else{
    library(PBSmapping)
    library(swfscMisc)
    data(nepacLLhigh) # shoreline dataset
    #################################################################
    # Calculate lateral viewing distance to horizon 
    # Equation: 
    # H = ((2*r*h)+h^2)^(1/2)
    r <- 6371 # km, radius of earth
    horizon <-((2*r*(eye.height/1000))+((eye.height/1000)^2))^(1/2) # in meters
    #
    #################################################################
    # Correct sighting bearing with magnetic declination
    bearing <- bearing + mag
    #
    #################################################################
    # Create imaginary line along bearing from vessel to horizon.
    Xhor <- destination(Y,X,bearing,horizon,units="km")[2]
    Yhor <- destination(Y,X,bearing,horizon,units="km")[1]
    pts.ocean <- list(c(X,Y),c(Xhor,Yhor))
    # Define boundary of where the sighting can occur
    sit.xlims = c(min(c(X,Xhor)),max(c(X,Xhor)))
    sit.ylims = c(min(c(Y,Yhor)),max(c(Y,Yhor)))
    #
    #################################################################
    # Interpolate that line
    Xline <- seq(X,Xhor,length=400)
    Yline <- seq(Y,Yhor,length=400)
    bearline <- data.frame(Xline,Yline)
    #
    #################################################################
    # Do same for vessel.hdg
    Xhorhdg <- destination(Y,X,vessel.hdg,horizon,units="km")[2]
    Yhorhdg <- destination(Y,X,vessel.hdg,horizon,units="km")[1]
    Xlinehdg <- seq(X,Xhorhdg,length=20)
    Ylinehdg <- seq(Y,Yhorhdg,length=20)
    hdgline <- data.frame(Xlinehdg,Ylinehdg)
    #
    #################################################################
    # Limit the shoreline coordinates to those occurring within the rectangle that includes the vessel and the horizon line
    viewshed <- nepacLLhigh[nepacLLhigh$X >= (min(X,Xhor)-.02) & 
                              nepacLLhigh$X <= (max(X,Xhor)+.02) & 
                              nepacLLhigh$Y >= (min(Y,Yhor)-.02) & 
                              nepacLLhigh$Y <= (max(Y,Yhor)+.02)
                            ,]
    #################################################################
    # Remove coordinates that are TOO close to the observer (within 100m)
    # It is assumed that reticles would only be taken to animals further out.
    # This prevents errors if the function is being used for land-based sightings.
    #if(nrow(viewshed)>0){
    #  ds <- vector()
    #  for(V in 1:nrow(viewshed)){
    #    Xv <- viewshed$X[V]
    #    Yv <- viewshed$Y[V]
    #    dv <- distance(lat1=Y,lon1=X,lat2=Yv,lon2=Xv,units="km",method="Vincenty")
    #    ds <- c(ds,dv)
    #  }
    #}
    #################################################################
    #################################################################
    # PLOT!
    if(nrow(viewshed)<2 | bearing==180 | bearing==360 | bearing==90 | bearing==270){
      xlims <- c((X-.15),(X+.15))
      ylims <- c((Y-.15),(Y+.15))
      if(toplot){print("Plot range modified due to a problem: either line orientation or no shoreline to plot!")}
    }else{
      xlims <- c(min(c(X,min(viewshed$X)))-.05,
             max(c(X,max(viewshed$X)))+.05)
      ylims <- c(min(c(Y,min(viewshed$Y)))-.05,
             max(c(Y,max(viewshed$Y)))+.05)
    }
    if(toplot){ 
      par(mfrow=c(1,1))
      par(mar=c(5,2,2,2))
      plotMap(nepacLLhigh,xlim=xlims,ylim=ylims,col="light gray", border="dark grey")
      # Add vessel position
      vessel <- data.frame(EID=1,X,Y)
      vessel <- as.EventData(vessel, projection="LL")
      addPoints(vessel,col="black",cex=1.5,pch=16)
      # Add vessel heading line
      EID <- 1:nrow(hdgline)
      hdgline <- data.frame(EID,X=hdgline$Xlinehdg,Y=hdgline$Ylinehdg)
      hdgline <- as.EventData(hdgline, projection="LL")
      addPoints(hdgline,col="black",cex=.1,pch=1)  
      # Add horizon
      EID <- 1
      hormap <- data.frame(EID,X=Xhor,Y=Yhor)
      hormap <- as.EventData(hormap, projection="LL")
      addPoints(hormap,col="blue",cex=2,pch="+")
      # Add viewshed vertices
      if(nrow(viewshed)>2){
      EID <- 1:nrow(viewshed)
      viewmap <- data.frame(EID,X=viewshed$X,Y=viewshed$Y)
      viewmap <- as.EventData(viewmap, projection="LL")
      addPoints(viewmap,col="forest green",cex=1.5,pch=0)
      }
      # Add bearing line
      EID <- 1:nrow(bearline)
      bearmap <- data.frame(EID,X=bearline$X,Y=bearline$Y)
      bearmap <- as.EventData(bearmap, projection="LL")
      addPoints(bearmap,col="blue",cex=.1,pch=1)
    }
    #################################################
    #################################################
    # Find shared solution of horizon and shore lines
    LOS <- TRUE # means we haven't found intersection yet
    intersection <- NA
    
    if(nrow(viewshed)>1){
      # For all adjacent pairs of shoreline, see if the bearing line cuts through it.
      for(i in 1:(nrow(viewshed)-1)){
        if(LOS & viewshed$PID[i]==viewshed$PID[i+1]){ 
          #dont compare shoreline pts from separate polygons
          # calculations should only run if we haven't hit shore yet
          
          # Set up shore points
          Xshore <- c(viewshed$X[i], viewshed$X[i+1])
          Yshore <- c(viewshed$Y[i], viewshed$Y[i+1])
          pts.shore <- list(c(Xshore[1],Yshore[1]),c(Xshore[2],Yshore[2]))
          
          if(toplot){
            # Demonstrate on the map which shore segment is being tested
            PID <- c(1:1) ; POS <- 1:2  
            showshore <- as.PolySet(data.frame(PID,POS,X=Xshore,Y=Yshore),projection="LL")
            addLines(showshore,lwd=2,col="forest green")
          }
          
          # Test for solution
          solved <- solve2lines(pts.ocean,pts.shore,toplot=FALSE)
          Xsolved <- solved[1]
          Ysolved <- solved[2]
          
          # Test to see if solution is within bounding rectangle of the 2 shore points
          # If solution is within range and finite, LOS=FALSE
          if(is.finite(Xsolved) & is.finite(Ysolved)){
            shore.xlims = c(min(Xshore,na.rm=TRUE),max(Xshore,na.rm=TRUE))
            shore.ylims = c(min(Yshore,na.rm=TRUE),max(Yshore,na.rm=TRUE))
            # Account for horizontal or vertical lines
            if(diff(range(shore.xlims))==0){shore.xlims[1] <- shore.xlims[1]-.005 ; shore.xlims[2] <- shore.xlims[2]+.005} 
            if(diff(range(shore.ylims))==0){shore.ylims[1] <- shore.ylims[1]-.005 ; shore.ylims[2] <- shore.ylims[2]+.005}
            
            if(  Xsolved >= shore.xlims[1] & Xsolved <= shore.xlims[2] & # Is solution within shorepoints box?
                 Ysolved >= shore.ylims[1] & Ysolved <= shore.ylims[2] & 
                 Xsolved >= sit.xlims[1] & Xsolved <= sit.xlims[2] & # Is solution within sighting box? 
                 Ysolved >= sit.ylims[1] & Ysolved <= sit.ylims[2]  
                   ){
              LOS <- FALSE
              intersection <- c(Xsolved,Ysolved)
            }
          } # end of if solve2lines had solution
        } # end of loop through shoreline vertices
      } # end of if LOS or PID test
    } # end of if nrow viewshed > 1
    
    ###################################################################
    ###################################################################
    # Which distance to use as the "horizon"? Shore or true horizon?
    if(is.finite(intersection[1])){
      distused <- "shore"
      refdist <- distance(Y,X,intersection[2],intersection[1],units="km",method="Vincenty")
    }else{
      distused <- "horizon"
      refdist <- horizon
    }
    
    ###################################################################
    ###################################################################
    # Use that ref distance to calculate distance of animal
    #
    # Calculate B, the angle from the shoreline/horizon to the sighting (using reticles)
    deg <- reticle * deg.per.ret
    theta <- deg*(pi/180)
    
    # Calculate D, the distance (in km) along the surface from the observer to the sighting
    R <- 6371
    h <- eye.height*(10^-3)  # convert to km
    dist <- refdist # already in km
    #    
    if(distused=="horizon"){
      a <- atan((sqrt( (2*h*R) + (h^2) ) ) / R)
      B <- (pi/2) - a - theta
    }
    if(distused=="shore"){
      a <- dist / R
      Lo <- sqrt( (R^2) + ((R+h)^2) - 2*R*(R+h)*cos(a) )
      B <- acos( ((2*h*R) + (h^2) + (Lo^2)) / (2*(R+h)*Lo) ) - theta
    }
    #
    Do <- (R+h)*cos(B) - sqrt( ((R+h)^2)*(cos(B)^2) - (2*h*R + (h^2)) )    
    delt <- asin( sin(B)*(Do/R))
    sitdist <- delt*R 
    #
    #################################################################
    #################################################################
    # Calculate position of animal
    Ysit <- as.numeric(destination(Y,X,bearing,sitdist,"km")[1])
    Xsit <- as.numeric(destination(Y,X,bearing,sitdist,"km")[2])
    
    # Add animal position
    if(toplot){
      EID <- 1
      sitmap <- data.frame(EID,X=Xsit,Y=Ysit)
      sitmap <- sitmap[!is.na(sitmap$X) & !is.na(sitmap$Y),]
      if(nrow(sitmap)>0){
        sitmap <- as.EventData(sitmap, projection="LL")
        addPoints(sitmap,col="firebrick",cex=3,pch=1)
        addPoints(sitmap,col="firebrick",cex=2,pch=1)
        addPoints(sitmap,col="firebrick",cex=1,pch=1)
      }
    }
    ##############################################
    # Calculate trackline distance # needed for line transect analysis
    # Without the curvature of the earth
    c <- as.numeric(sitdist)
    trackdist <- NA
    trackbear <- NA
    if(is.na(c)==FALSE & is.na(vessel.hdg)==FALSE){
      theta <- bearing - vessel.hdg
      trackbear <- theta
      if(is.na(theta)==FALSE){
        if(theta < 0){theta <- 360-theta}  
        theta <- theta * (pi/180)
        trackdist <- c*sin(theta)
      }
    }   
  ##############################################
  ##############################################
  } # end of if any inputs are NA
  return(list(X=Xsit,
              Y=Ysit,
              radial.dist=sitdist,
              boundary=distused,
              boundary.dist=refdist,
              perp.dist=trackdist,
              track.bearing=trackbear)
         )
  ##############################################
} 