#' `ggplot()` of the Kitimat Fjord System
#'
#' @param lat_range Two-element vector of latitudinal range
#' @param long_range Two-element vector of longitudinal range
#' @param land_fill Land fill color
#' @param land_alpha Land fill alpha
#' @param water_fill Water fill color
#' @param water_alpha Water fill alpha
#' 
#' @return Returns a `ggplot()` map object. 
#' @import ggplot2
#' @export
#'
gg_kfs <- function(lat_range = c(52.8, 53.55),
                   lon_range = c(-129.68, -128.85),
                   land_fill = 'darkslategrey',
                   land_alpha = .2,
                   water_fill = 'lightblue',
                   water_alpha = .1){
  
  if(FALSE){
    lat_range = c(52.8, 53.55)
    lon_range = c(-129.68, -128.85)
    land_fill = 'darkslategrey'
    land_alpha = .2
    water_fill = 'lightblue'
    water_alpha = .1
  }
  
  data(nepac_shore_sf)
  
  ggplot2::ggplot(nepac_shore_sf) +
    ggplot2::geom_sf(fill='darkslategrey',
                     alpha=land_alpha,
                     color='grey30', 
                     lwd=.25) +
    ggplot2::ylim(lat_range[1], lat_range[2]) + 
    ggplot2::xlim(lon_range[1], lon_range[2]) + 
    ggplot2::coord_sf() + 
    ggplot2::theme_light() + 
    theme(panel.background = element_rect(fill = adjustcolor(water_fill, alpha.f=water_alpha),
                                          #colour = "lightblue",
                                          size = 0.5, linetype = "solid"))
  
}
