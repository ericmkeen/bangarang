#' Primary functions for routeKFS
#' 
############################################################################
############################################################################
############################################################################
### OCEAN DISTANCE
ocean.route <- function(pts.ocean,
                  buffer=.01,
                  LOSplots=FALSE,
                  show.progress=TRUE){
  ############################################################################
  # Find the route between two ocean pts
  ############################################################################
  ############################################################################
  library(swfscMisc)
  ### LOAD DECISION POINTS
  data(nodesraw)
  data(nodesLOSmatrix)
  data(NodesFirstStepsMatrix)
  data(deadendkey)
  # Change object names
  nodepts <- nodesraw
  nodeLOS <- nodesLOSmatrix
  first <- NodesFirstStepsMatrix
  first$N0 <- NULL
  key <- deadendkey
  ############################################################################
  ############################################################################
  ############################################################################
  # Setup vectors of turn point results
  Xturn <<- Yturn <<- vector() 
  ############################################################################
  # Deal with inputs
  x.start <- pts.ocean[[1]][1]
  x.end <- pts.ocean[[2]][1]
  y.start <- pts.ocean[[1]][2]
  y.end <- pts.ocean[[2]][2]
  node.start <- c(x.start,y.start)
  node.end <- c(x.end,y.end)
  # Store first given point as first turn point
  addturn(c(x.start,y.start))
  ############################################################################
  # Initial line of sight test
  test1 <- LOStest(pts.ocean,buffer=buffer,toplot=LOSplots)[[1]]
  if(!test1){
    # If two points are NOT line of sight, launch OCEAN DISTANCE calculations  
    ############################################################################
    goodnodes <- nodepts
    ############################################################################
    # Find LOS travel nodes to start point: 
    dist2start <- dist2end <- vector()
    for(i in 1:nrow(goodnodes)){
      dist <- distance(lat1=node.start[2],lon1=node.start[1],
                       lat2=goodnodes$Y[i], lon2=goodnodes$X[i] ,units="km",method="Vincenty")
      dist2start <- c(dist2start,dist)
      dist <- distance(lat1=node.end[2],lon1=node.end[1],
                       lat2=goodnodes$Y[i], lon2=goodnodes$X[i] ,units="km",method="Vincenty")
      dist2end <- c(dist2end,dist)
    }
    goodnodes$dist2start <- dist2start
    goodnodes$dist2end <- dist2end
    
    ############################################################################
    # Best and closest node to START
    # Subset to closest 5 nodes
    starts <- goodnodes[order(goodnodes$dist2start),]
    starts <- starts[1:10,]
    # Subset these to LOS only
    startlos <- vector()
    for(i in 1:nrow(starts)){
      pts.start <- list(node.start,c(starts$X[i],starts$Y[i]))
      losi <- LOStest(pts.start,buffer=.005,toplot=FALSE)[[1]]
      startlos <- c(startlos,losi)
    }
    if(any(startlos)){starts <- starts[startlos,]}
    startnodes <- starts$EID
    nextstep <- closest2end(node.start,node.end,startnodes,goodnodes)
    
    ############################################################################
    # Best and closest node to END
    # Subset to closest 5 nodes
    ends <- goodnodes[order(goodnodes$dist2end),]
    ends <- ends[1:10,]
    # Subset these to LOS only
    endlos <- vector()
    for(i in 1:nrow(ends)){
      pts.end <- list(node.end,c(ends$X[i],ends$Y[i]))
      losi <- LOStest(pts.end,buffer=.005,toplot=FALSE)[[1]]
      endlos <- c(endlos,losi)
    }
    if(any(endlos)){ends <- ends[endlos,]}
    endnodes <- ends$EID
    finalstep <- closest2end(node.end,node.start,endnodes,goodnodes)

    ############################################################################
    # Only travel if this is not a one-step jounrey:
    if(finalstep==nextstep){
      onenodetest <- TRUE
    }else{
      onenodetest <- FALSE
      ############################################################################
      # Prepare to add first step
      firstnode <- c(goodnodes$X[goodnodes$EID==nextstep],goodnodes$Y[goodnodes$EID==nextstep])
      ############################################################################
      # Get traveling!
      currentLOS=FALSE # status variable, whether or not currentnode is LOS to end.pt
      nextx=FALSE # status variable, whether or not next step is LOS with final node. If it is, skip travel and just make sure final node is saved.
      finalcol <- first[,finalstep] # column of first steps corresponding to final node
      for(i in 1:nrow(goodnodes)){
        if(!currentLOS){
          if(i==1){firststep <- nextstep}
          nextstep <- finalcol[nextstep]
          if(nextstep=="x"){
            nextx <- TRUE ; currentLOS <- TRUE ; test2 <- TRUE
            if(i==1){
              addturn(firstnode)
              if(show.progress){print(paste0("Step 1: went to node ",firststep," on way to final node: ",finalstep,"!!!"))}
            }
          }else{
            nextstep <- as.numeric(nextstep)
            currentnode <- c(goodnodes$X[goodnodes$EID==nextstep],goodnodes$Y[goodnodes$EID==nextstep])
            
            # Make sure first step is the best one
            if(i==1){
              pts.test <- list(node.start,currentnode)
              teststep2 <- LOStest(pts.test,buffer=.005,toplot=FALSE)[[1]]
              if(!teststep2){
                # If this second step is not LOS of starting pt, go ahead and add the original first step
                # (A precaution in case the closest2end function didn't find the best first step)
                addturn(firstnode)
                if(show.progress){print(paste0("Step 1: went to node ",firststep," on way to final node: ",finalstep,"!!!"))}
              }
            }
            
            # Make step
            addturn(currentnode)
            if(show.progress){print(paste0("Step ", i+1 ,": went to node ",nextstep," on way to final node: ", finalstep,"!!!"))}
            
            # LOS test to make sure this node isn't LOS with node.end
            pts.test <- list(node.end,currentnode)
            test2 <- LOStest(pts.test,buffer=.005,toplot=FALSE)[[1]]
            if(test2){currentLOS <- TRUE}
          } # end of if nextstep==x
        } # end of if !currentLOS
      } # end of travel for loop
    } # end of if firststep == finalstep (it's a one-node journey)
    }# end of test1: if start and end are not LOS
  #####################################################################
  #  If, after 93 tries, there was still no solution, abandon
  if(test1){currentLOS<-TRUE; test2 <- TRUE ; onenodetest <- FALSE ; nextx <- FALSE}
  if(onenodetest){test2 <- FALSE ; currentLOS <- TRUE ; nextx <- FALSE}
  if(!currentLOS){Xturn <<- NA ; Yturn <<- NA}
  #####################################################################
  # Store final step, if the traveling didn't find a more direct route
  if(!test2 | nextx){
    finalnode <- c(goodnodes$X[goodnodes$EID==finalstep],goodnodes$Y[goodnodes$EID==finalstep])
    addturn(finalnode)
  }

  #####################################################################
  # Store last given point as last turn point
  addturn(c(x.end,y.end))
  
  #####################################################################
  turns <- data.frame(Xturn,Yturn)
  names(turns)[1] <- "X"
  names(turns)[2] <- "Y"
  return(turns)
} # end of function

#####################################################################
#####################################################################
