#' `ggplot()` of the Kitimat Fjord System
#'
#' @param lat_range Two-element vector of latitudinal range
#' @param long_range Two-element vector of longitudinal range
#' 
#' @return Returns a `ggplot()` map object. 
#' @export
#'
gg_kfs <- function(lat_range = c(52.8, 53.55),
                   lon_range = c(-129.68, -128.85)){

  data(nepac_shore_sf)
  
  ggplot(shoreline) +
    geom_sf(color='grey30', lwd=.25) +
    ylim(52.8, 53.55) +
    xlim(-129.68, -128.85) +
    coord_sf() + 
    theme_light()
  
}
