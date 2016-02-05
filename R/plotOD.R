#####################################################################
#' Map travel route computed by routeKFS

plot.OD <- function(turns){ 
  library(PBSmapping)
  data(nepacLLhigh)
  data(nodesraw)
  ##### Prep nodes
  nodemap <- as.EventData(nodesraw)
  ##### Prep start and end points
  Xturn <- turns$X
  Yturn <- turns$Y
  x.start <- Xturn[1]
  x.end <- Xturn[length(Xturn)]
  y.start <- Yturn[1]
  y.end <- Yturn[length(Yturn)]
  EID <- 1:2 ; X <- c(x.start,x.end) ; Y <- c(y.start,y.end)
  destinations <- as.EventData(data.frame(EID,X,Y))
  ##### Plot it
  plotKFS()
  addPoints(nodemap,col="grey",pch=16,cex=.5)
  addPoints(destinations,cex=1,col="firebrick")
  addPoints(destinations,cex=2,col="firebrick")
  ###### Prep turn route
  if(!any(is.na(Xturn)) | !any(is.na(Yturn))){
    PID <- rep(1,times=length(Xturn))
    POS <- 1:length(Xturn)
    X <- Xturn
    Y <- Yturn
    turnset <- data.frame(PID,POS,X,Y)
    turnset <- as.PolySet(turnset,projection="LL")
    addLines(turnset)
  }
}

#####################################################################
#####################################################################
