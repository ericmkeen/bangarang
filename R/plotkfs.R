#' Maps for the Kitimat Fjord System
#' @description Mapping functions in the Kitimat Fjord System tailored to research needs
#' of the Bangarang, Gitga'at, and NCCS teams.
#' @param area Map range. Default is "Study Area" of the Bangarang project. Other range options:
#' \itemize{
#'   \item "Caamano"
#'   \item "Estevan"
#'   \item "Campania"
#'   \item "Squally"
#'   \item "S. Squally" (Dougan to McDonald)
#'   \item "N. Squally" (McDonald to Union)
#'   \item "Whale"
#'   \item "Wright"
#'   \item "McKay" (includes S. Ursula up to Bishop)
#'   \item "Verney" (includes N. Ursula down to Bishop)
#'   \item "Gil Basin" (the channels surrounding Gil Island)
#'   \item "Gribbell Island" (the channels surround Gribbell)
#'   \item "Other" (custom range, entered using \code{X} and \code{Y} inputs)
#' }
#' @param X Custom longitude range (e.g., \code{c(-129.68,-128.85)}). Only applies when "Other" is entered as area.
#' @param Y Custom latitude range (e.g., \code{c(52.9,53.3)}). Only applies when "Other" is entered as area.
#' @param mar Plot margins, passed to \code{\link{par}}.
#' @param mai Margin size specific in inches, passted to \code{\link{par}}.
#' @param land.col Land color.
#' @param shore.border Color of shoreline border.
#' @param shore.lwd Shoreline border thickness.
#' @param tankers If \code{TRUE} (not default), proposed tanker route is displayed.
#' @param tankers.width Tanker track line width.
#' @param tankers.col Tanker track color
#' @param places If \code{TRUE} (not default), research locations are labeled: Hartley Bay, Whale Point, GW Camp, and the Wall.
#' @param places.scale Text size of place labels.
#' @param stat If \code{TRUE} (not default), shows grid of 24 Bangarang stations.
#' @param stat.col Station color.
#' @param stat.scale Size of station dots.
#' @param tracks If \code{TRUE} (not default), shows Bangarang survey tracklines.
#' @param track.col Trackline color.
#' @param blocking Controls which block stratification, if any, to show. Default is nothing. 
#' If \code{"mini"}, shows a 54-block scheme. 
#' \code{"minirect"} shows the rectangles used to partition the study area into those blocks. 
#' \code{"sub"} shows a 24-block scheme, the scheme used in the HW-Wave study.
#' \code{"channel"} shows an 8-block scheme.
#' \code{"province"} combines those 8 channels into 4 large provinces: offshore, central outer, central inner, and inland.
#' @param block.col Color of blocking borders.
#' @param block.lwd Blocking border width.
#' @param block.lab.scale Default is \code{FALSE}. If a number, it prints labels within the blocks (only for "mini" and "minirect"), scaled to the number given.
#' @return Generates map.
#' @export
#' @import PBSmapping
#' @import maps
#' @author Eric Keen, Scripps Institution of Oceanography, \email{ekeen@@ucsd.edu} 
#' @examples
#' ##############################################
#'  # Simple plot of study area
#'  plotKFS()
#' ##############################################
#' # Zoom in of Squally Channel
#' plotKFS(area="Squally")
#' ##############################################
#' # Self-defined map range: Kitimat Fjord System with tanker route and study tracklines
#' plotKFS(area="Other",
#'        X=c(-129.9,-127.9),
#'        Y=c(52.5,54.05),
#'        tankers=TRUE,
#'        tankers.col=adjustcolor("firebrick",alpha.f=.4),
#'        tracks=TRUE)
#' ##############################################
#' # Research headquarters
#' plotKFS(places=TRUE,places.scale=1.5)
#' ##############################################
#' # 2015 Bangarang sampling plan
#' plotKFS(stat=TRUE,stat.scale=1.5,tracks=TRUE)
#' ##############################################
#' # Examples of study area blocking for various analyses
#' plotKFS(blocking="minirect",block.lab.scale=1.5) # 54 mini blocks -- never used in analysis
#' plotKFS(blocking="mini") # Shoreline within those 54 miniblocks
#' plotKFS(blocking="sub") # Each waterway split into 3 subblocks
#' plotKFS(blocking="channel") # The 8 main waterways of Bangarang analyses
#' plotKFS(blocking="province") # Four main provinces
############################################################################
############################################################################
plotKFS <- function(area="Study Area",
        X=c(-129.68,-128.85),
        Y=c(52.8,53.55),
        mar=c(4.2,3,.2,.2),
        mai=c(.75,.75,.1,.1),
        land.col="light gray",
        shore.border="dark gray",
        shore.lwd=1,
        tankers=FALSE,
        tankers.width=5,
        tankers.col="tan1",
        places=FALSE,
        places.scale=1,
        stat=FALSE,
        stat.col="black",
        stat.scale=1,
        tracks=FALSE,
        track.col="black",
        blocking=NULL,
        block.col="black",
        block.lwd=1,
        block.lab.scale=FALSE){
  ############################################################################
  ############################################################################

  data(nepacLLhigh)
  if(is.null(blocking)){blocking <- 1}

  #### LONGITUDE
  if(area=="Study Area"){long <- c(-129.68,-128.85)}    
  if(area=="Caamano"){long <- c(-129.5,-129.1)}    
  if(area=="Estevan"){long <- c(-129.67,-129.4)}    
  if(area=="Campania"){long <- c(-129.34,-129.15)}    
  if(area=="Squally"){long <- c(-129.51,-129.28)}    
  if(area=="S. Squally"){long <- c(-129.5,-129.28)}    
  if(area=="N. Squally"){long <- c(-129.51,-129.3)}    
  if(area=="Whale"){long <- c(-129.2,-129.0)}    
  if(area=="Wright"){long <- c(-129.4,-129.15)}    
  if(area=="McKay"){long <- c(-129.15,-128.85)}    
  if(area=="Verney"){long <- c(-129.2,-128.9)}  
  if(area=="Gil Basin"){long <- c(-129.5,-129.05)}    
  if(area=="Gribbell Island"){long <- c(-129.2,-128.85)}    
  if(area=="Other"){long <- X}
    
  #### LATITUDE
  if(area=="Study Area"){lat <- c(52.8,53.55)}    
  if(area=="Caamano"){lat <- c(52.805,53.0)}    
  if(area=="Estevan"){lat <- c(53.04,53.23)}    
  if(area=="Campania"){lat <- c(52.905,53.12)}    
  if(area=="Squally"){lat <- c(53.05,53.35)}    
  if(area=="S. Squally"){lat <- c(53.05,53.2)}    
  if(area=="N. Squally"){lat <- c(53.19,53.35)}    
  if(area=="Whale"){lat <- c(53.05,53.32)}    
  if(area=="Wright"){lat <- c(53.2,53.4)}    
  if(area=="McKay"){lat <- c(53.27,53.48)}    
  if(area=="Verney"){lat <- c(53.35,53.55)}  
  if(area=="Gil Basin"){lat <- c(53.05,53.38)}    
  if(area=="Gribbell Island"){lat <- c(53.27,53.55)}    
  if(area=="Other"){lat <- Y}
  
  #### plot
  par(mar=mar,mai=mai)
  plotMap(nepacLLhigh, xlim=long, ylim=lat,col=land.col, border=shore.border,lwd=shore.lwd,plt=NULL)
  
  #####################################
  #####################################
  #### Tankers
  #####################################
  if(tankers){
    data(shiplane)
    ngp <- as.PolySet(shiplane,projection="LL")
    addLines(ngp,lwd=tankers.width,col=tankers.col)
  }    
  #####################################
  #####################################
  #### Stations
  #####################################
  if(stat){
    data(stations)
    sta <- stations
    names(sta)[2]<-"block"
    names(sta)[3]<-"stations"
    names(sta)[1]<-"EID"  
    ### Make into an EID
    eid <- sta  
    eidframe <- data.frame(eid[,1],eid[,6],eid[,5],eid[,2],eid[,3],eid[,4],eid[,7])
    names(eidframe)[1] <- "EID"
    names(eidframe)[2] <- "X"
    names(eidframe)[3] <- "Y"
    names(eidframe)[4] <- "block"
    names(eidframe)[5] <- "stations"
    names(eidframe)[6] <- "mode"
    names(eidframe)[7] <- "color"
    eidframe$EID <- as.numeric(eidframe$EID)
    eidframe$mode <- as.character(eidframe$mode)
    eidframe$station <- as.numeric(eidframe$station)
    eidframe$color <- as.character(eidframe$color)
    # Subset to Lite stations
    eidlite <- subset(eidframe, eidframe$mode=="lite")
    eidlite<-as.EventData(eidlite, projection="LL")
    addPoints(eidlite,cex=(1*stat.scale),col=stat.col)
    addPoints(eidlite,cex=(.75*stat.scale),col=stat.col)
    addPoints(eidlite,cex=(.5*stat.scale),col=stat.col)
  }
  #####################################
  if(tracks){
    data(stations)
    names(stations)[2]<-"block"
    names(stations)[3]<-"stations"
    names(stations)[1]<-"EID"
    ### Make into an EID
    eid <- stations  
    eidframe <- data.frame(eid[,1],eid[,6],eid[,5],eid[,2],eid[,3],eid[,4],eid[,7])
    names(eidframe)[1] <- "EID"
    names(eidframe)[2] <- "X"
    names(eidframe)[3] <- "Y"
    names(eidframe)[4] <- "block"
    names(eidframe)[5] <- "stations"
    names(eidframe)[6] <- "mode"
    names(eidframe)[7] <- "color"
    eidframe$EID <- as.numeric(eidframe$EID)
    eidframe$mode <- as.character(eidframe$mode)
    eidframe$station <- as.numeric(eidframe$station)
    eidframe$color <- as.character(eidframe$color)
    #########################################################
    # Subset to Lite stations
    eidlite <- subset(eidframe, eidframe$mode=="lite")
    eidframe<-as.EventData(eidframe, projection="LL")
    eidlite<-as.EventData(eidlite, projection="LL")
    ##### Add lines
    stationspoly <- data.frame(seq(1,1,length=nrow(eidframe)), seq(1,nrow(eidframe),by=1), eidframe$X, eidframe$Y,eidframe$block)
    names(stationspoly)[1]<-"PID"
    names(stationspoly)[2]<-"POS"
    names(stationspoly)[3]<-"X"
    names(stationspoly)[4]<-"Y"
    names(stationspoly)[5]<-"block"
    stationspoly$POS <- as.numeric(stationspoly$POS)
    stationspoly <- as.PolySet(stationspoly, projection="LL")
    #### Break it into blocks
    caa<-subset(stationspoly,stationspoly$block=="CAA")
    cmp<-subset(stationspoly,stationspoly$block=="CMP")
    wha<-subset(stationspoly,stationspoly$block=="WHA")
    sqn<-subset(stationspoly,stationspoly$block=="SQN")
    sqs<-subset(stationspoly,stationspoly$block=="SQS")
    wri<-subset(stationspoly,stationspoly$block=="WRI")
    ver<-subset(stationspoly,stationspoly$block=="VER")
    mck<-subset(stationspoly,stationspoly$block=="MCK")
    est<-subset(stationspoly,stationspoly$block=="EST")
    fcm<-subset(stationspoly,stationspoly$block=="CMPF")
    fsqs <- subset(stationspoly,stationspoly$block == "SQSF")
    fnqs <- subset(stationspoly,stationspoly$block == "SQNF")
    # Format the transect lines
    caa <- as.PolySet(caa, projection="LL")
    est <- as.PolySet(est, projection="LL")
    cmp <- as.PolySet(cmp, projection="LL")
    wha <- as.PolySet(wha, projection="LL")
    sqs <- as.PolySet(sqs, projection="LL")
    sqn <- as.PolySet(sqn, projection="LL")
    wri <- as.PolySet(wri, projection="LL")
    ver <- as.PolySet(ver, projection="LL")
    mck <- as.PolySet(mck, projection="LL")
    fcmp <- as.PolySet(fcm, projection="LL")
    fsqs <- as.PolySet(fsqs,projection="LL")
    fnqs <- as.PolySet(fnqs,projection="LL")
    addLines(caa,col=track.col)
    addLines(est,col=track.col)
    addLines(cmp,col=track.col)
    addLines(fcmp,col=track.col)
    addLines(wha,col=track.col)
    addLines(fsqs,col=track.col)
    addLines(sqs,col=track.col)
    addLines(fnqs,col=track.col)
    addLines(sqn,col=track.col)
    addLines(wri,col=track.col)
    addLines(ver,col=track.col)
    addLines(mck,col=track.col)
  }

  
  #####################################
  ##### Place names
  #####################################
  if(places){
    EID <- 1:4
    namec <- c("Hartley Bay","GWs","Whale Pt","The Wall")
    X <- c(-129.252566,-129.313755,-129.183379,-129.3527)
    Y <- c(53.422351,52.8356,53.102704,52.858036)
    cexc <- c(.7,.7,.7,.7)
    colc <- c("firebrick","firebrick","blue","blue")
    collabo <- data.frame(EID,X,Y,namec,cexc,colc)
    collabo <- as.EventData(collabo,projection="LL")
    addPoints(collabo,cex=collabo$cexc,col="black",pch=16)
    text(collabo$X,collabo$Y,collabo$namec,pos=c(2,1,2,2),cex=(.6*places.scale))
  }
    #####################################
  
  #####################################
  ##### Blocks
  #####################################
  if(blocking=="minirect"){
    data(blocks)
    segments(x0=blocks$left,x1=blocks$right,y0=blocks$top,y1=blocks$top,col=block.col)
    segments(x0=blocks$left,x1=blocks$right,y0=blocks$bottom,y1=blocks$bottom,col=block.col)
    segments(x0=blocks$left,x1=blocks$left,y0=blocks$top,y1=blocks$bottom,col=block.col)
    segments(x0=blocks$right,x1=blocks$right,y0=blocks$top,y1=blocks$bottom,col=block.col)
    if(as.numeric(block.lab.scale)!=0){
      text(x=blocks$center.x,y=blocks$center.y,labels=blocks$ID,cex=(.7*block.lab.scale))
    }
  }
  
  if(blocking=="mini"){
    data(waterpolygons)
    waters <- waterpolygons
    waters.col <- rep("black",times=54)
    waterblox <- unique(waters$block)
    if(length(block.col)<54){block.col <- rep(block.col[1],times=54)}
    for(i in 1:length(waterblox)){
      blocki <- waterblox[i]
      waters.col[i] <- block.col[blocki]
    }
    waters <- as.PolySet(waters,projection="LL")
    addLines(waters,lwd=block.lwd,col=waters.col)
  }
  if(blocking=="sub"){
    data(subblocks)
    tri <- as.PolySet(subblocks,projection="LL")
    addLines(tri,lwd=block.lwd,col=block.col)    
  }
  if(blocking=="channel"){
    data(channels)
    channels <- data.frame(channels)
    chan <- as.PolySet(channels,projection="LL")
    addLines(chan,lwd=block.lwd,col=block.col)
  }
  if(blocking=="province"){
    data(provinces)
    provinces <- data.frame(provinces)
    prov <- as.PolySet(provinces,projection="LL")
    addLines(prov,lwd=block.lwd,col=block.col)
  }
}
