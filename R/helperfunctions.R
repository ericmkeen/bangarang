#' Helper functions for routeKFS
#####################################################################
#####################################################################
# Function: addturn
addturn <- function(currentnode){
  # Add a row to the turn results dataframe
  if(length(Xturn)>0 & length(Yturn)>0){
    if(!is.na(Xturn)){Xturn <<- c(Xturn,currentnode[1])}
    if(!is.na(Yturn)){Yturn <<- c(Yturn,currentnode[2])}
  }else{
    Xturn <<- c(Xturn,currentnode[1])
    Yturn <<- c(Yturn,currentnode[2])
  }
}
#####################################################################
#####################################################################
#' @describeIn addturn Determine node closest to end destination 
#' 
closest2end <- function(startpt,endpt,nextnodes,nodepts){
  library(swfscMisc)
  # Which of a set of candidate LOS nodes are nearest the end destination?
  startdists <- enddists <- bearings <- vector()
  xstart <- startpt[1]
  ystart <- startpt[2]
  xend <- endpt[1]
  yend <- endpt[2]
  basedist <- distance(ystart,xstart,yend,xend,units="km",method="Vincenty")
  if(length(nextnodes)==1){
    bestnode <- nextnodes
  }else{
  for(i in 1:length(nextnodes)){
    lat2 <- nodepts$Y[nodepts$EID==nextnodes[i]]
    lon2 <- nodepts$X[nodepts$EID==nextnodes[i]]
    startdists <- c(startdists,distance(ystart,xstart,lat2,lon2,units="km",method="Vincenty"))
    enddists <- c(enddists,distance(yend,xend,lat2,lon2,units="km",method="Vincenty"))
  }
  results <- data.frame(nextnodes)
  results$startdist <- startdists
  results$enddist <- enddists
  results$totdist <- enddists + startdists
  results$startrank <- rank(startdists) # rank by dist from start pt
  results$endrank <- rank(enddists) # rank by dist from end pt
  results$overshoot <- startdists > basedist # which nodes would overshoot end?
  results$backtrack <- enddists > basedist # which nodes would backtrack?
  # Use Heron's formula to calculate triangle area
  A <- vector()
  for(i in 1:nrow(results)){
   a <- basedist
   b <- results$startdist[i]
   c <- results$enddist[i]
   s <- (a+b+c)/2
   Ai <- sqrt(s*(s-a)*(s-b)*(s-c))
   A <- c(A,Ai)
  }
  results$area <- A
  results$arearank <- rank(A)
  results$totsum <- results$area + results$enddist
  results$totprod <- results$area * results$enddist
  
  # Whittle down options
  # Take first 3 closest nodes to the start, choose the one closest to the end
  newresults <- results[order(results$startrank),]
  newresults <- newresults[1:min(nrow(newresults),4),]
  bestnode <- newresults$nextnodes[which.min(newresults$endrank)]
  
  # Remove any backtrack or overshoots
  #newresults <- results[results$backtrack == FALSE & results$overshoot == FALSE,]
  #if(nrow(newresults)==0){newresults <- results[results$overshoot == FALSE,]}
  #if(nrow(newresults)==0){newresults <- results}
  # Weight against any overshoots or backtracks:
  #results$enddist[results$backtrack==TRUE] <- 999
  #results$enddist[results$overshoot==TRUE] <- 899
  #bestnode <- results$nextnodes[which.min(results$enddist)]
  }
  return(bestnode)
}
#####################################################################
#####################################################################
#' @describeIn addturn Determine which nodes are line-of-sight of a point 

find.LOS.nodes <- function(node,nodepts,toprint=TRUE){
  # Function: find.LOS.nodes # which nodes are LOS of a point?
  ptlos <- vector()
  for(i in 1:nrow(nodepts)){
    nodei <- c(nodepts$X[i],nodepts$Y[i])
    losj <- LOStest(list(node,nodei),buffer=.005,toplot=FALSE)[[1]]
    ptlos <- c(ptlos,losj)
    if(toprint){print(paste0("LOS test done: Node ",nodepts$EID[i],", ",i,"  of ",nrow(nodepts)," = ",losj," !!!"))}
  }  
  losnodes <- which(ptlos)
  losnodes <- nodepts$EID[losnodes]
  return(losnodes)
}
#####################################################################
#####################################################################
#' @describeIn addturn Calculated distance between two nodes
#' 
dist2nodes <- function(pt1,pt2){
  # Function: distance between two nodes:
  library(swfscMisc)
  lat1 <- pt1[2] 
  lon1 <- pt1[1]
  lat2 <- pt2[2]
  lon2 <- pt2[1]
  return(distance(lat1,lon1,lat2,lon2,method="Vincenty",units="km"))
}
#####################################################################
#####################################################################
#' @describeIn addturn Eliminate potential nodes based on their distance from the current node

find.unusables <- function(currentID,currentnode,endpt,nextnodes,nodepts){
  # Function: find.unusables
  library(swfscMisc)
  currentdist <- dist2nodes(currentnode,endpt)
  nextdists <- vector()
  for(i in 1:length(nextnodes)){
    ptnext <- which(nodepts$EID==nextnodes[i])
    ptnext <- c(nodepts$X[ptnext],nodepts$Y[ptnext])
    nextdists <- c(nextdists,dist2nodes(ptnext,endpt))
  }
  badnodes <- nextnodes[which(nextdists > currentdist)]
  badnodes <- c(badnodes,currentID)
  return(badnodes)
}
