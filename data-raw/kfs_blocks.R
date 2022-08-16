torun <- FALSE
if(torun){
  plotKFS()
  bloxim <- data.frame()
  uid <- unique(blox$block) ; uid
  i=1
  for(i in 1:length(uid)){
    idi <- uid[i]
    bloxi <- blox[blox$block==idi,]
    c1 = cbind(bloxi$X, bloxi$Y)
    P1 = Polygon(c1)
    Ps1 = Polygons(list(P1), ID = "a")
    SPs = SpatialPolygons(list(Ps1))
    bb <- bbox(SPs) ; bb
    px <- c(bb[1,1],bb[1,1],bb[1,2],bb[1,2],bb[1,1]) ; px
    py <- c(bb[2,1],bb[2,2],bb[2,2],bb[2,1],bb[2,1]) ; py
    lines(px,py)
    dfi <- data.frame(id=idi,
                      left=bb[1,1],right=bb[1,2],
                      bottom=bb[2,1],top=bb[2,2])
    dfi
    bloxim <- rbind(bloxim,dfi)

  }
  bloxim

  write.csv(bloxim,"../data/blocks.csv",row.names=FALSE,quote=FALSE)
}

kfs_blocks_bbox <- read_csv('data-raw/blocks.csv')
usethis::use_data(kfs_blocks_bbox, overwrite = TRUE)
