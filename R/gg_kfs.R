#' `ggplot()` of the Kitimat Fjord System
#'
#' @param lat_range Two-element vector of latitudinal range
#' @param long_range Two-element vector of longitudinal range
#' @param land_fill Land fill color
#' @param land_line Land border color
#' @param land_stroke Land border thickness
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
                   land_line = 'grey30',
                   land_stroke = .25,
                   land_alpha = .2,
                   water_fill = 'lightblue',
                   water_alpha = .1){
  
  if(FALSE){
    lat_range = c(52.8, 53.55)
    lon_range = c(-129.68, -128.85)
    land_fill = 'darkslategrey'
    land_line = 'grey30'
    land_alpha = .2
    water_fill = 'lightblue'
    water_alpha = .1
  }
  
  data(nepac_shore_sf)
  
  ggplot2::ggplot(nepac_shore_sf) +
    ggplot2::geom_sf(fill=land_fill,
                     alpha=land_alpha,
                     color=land_line, 
                     lwd=land_stroke) +
    ggplot2::coord_sf(ylim=lat_range,
                      xlim=lon_range) + 
    ggplot2::theme_light() + 
    theme(panel.background = element_rect(fill = adjustcolor(water_fill, alpha.f=water_alpha),
                                          size = 0.5, linetype = "solid"))
  
}
