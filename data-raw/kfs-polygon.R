library(rgdal)
kfs <- readOGR(
  dsn= "data-raw/kfs_shapefile/layers/",
  layer="POLYGON",
  verbose=FALSE
)
#proj4string(kfs) <-CRS("+proj=utm +zone=10+datum=WGS84")

class(kfs)

kfs_boundary <- kfs
usethis::use_data(kfs_boundary, overwrite = TRUE)
