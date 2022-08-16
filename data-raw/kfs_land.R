library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(PBSmapping)
data("nepacLLhigh")

data("kfs_boundary")
#proj4string(kfs) <-CRS("+proj=utm +zone=10+datum=WGS84")
proj4string(kfs_boundary) <-CRS("+proj=utm +zone=10+datum=WGS84")

#########################################################
nepac <- nepacLLhigh
head(nepac)
pids <- unique(nepac$PID) ; pids
plist <- list()
i=1
for(i in 1:length(pids)){
  pidi <- pids[i] ; pidi
  mri <- nepac[nepac$PID==pidi,] ; head(mri)
  c1 = cbind(mri$X, mri$Y)
  #r1 = rbind(c1, c1[1, ])  # join
  P1 = Polygon(c1)
  Ps1 = Polygons(list(P1), ID = pidi)
  plist[[i]] <- Ps1
}
length(plist)

SPs = SpatialPolygons(plist)
SPs
proj4string(SPs) <-CRS("+proj=utm +zone=10+datum=WGS84")

#########################################################
?gIntersection
kfsland <- gIntersection(SPs, kfs, byid = F)


#########################################################
if(toplot){
  bangarang::plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
  plot(kfs,add=T)
  plot(kfsland,add=T)
}

kfs_land <- kfsland
usethis::use_data(kfs_land, overwrite = TRUE)

