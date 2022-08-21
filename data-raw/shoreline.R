
library(PBSmapping)
library(sfheaders)
library(ggplot2)
library(sf)
data("nepacLLhigh")
nepacLLhigh %>% head

shoreline <- sfheaders::sf_multipolygon(obj = nepacLLhigh,
                                        polygon_id = "PID",
                                        x = "X", y = "Y")
st_crs(shoreline) <- 4326

nepac_shore_sf <- shoreline
usethis::use_data(nepac_shore_sf, overwrite = TRUE)

