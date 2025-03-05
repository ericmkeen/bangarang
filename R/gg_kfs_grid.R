#' Plot a chloropleth surface in the KFS
#'
#' @param values The vector of values to plot. Length must be same number of rows as `kfs_grid` input.
#' @param kfs_grid `data.frame` of grid coordinates, with `x` and `y` columns. If `NULL`, the built-in `grid` dataset from the `bangarang` package will be used.  
#' @param point_size Size of each grid point. May need to be adjusted based on output size of your plot. 
#' @param color_map The color palette to use from the `viridis` package.  
#' @param scale_breaks Vector of values to note in the color scale legend.
#' @param scale_limits Two-element vector of minimum and maximum values to plot.
#' @param pseudo_log Boolean; if `TRUE`, the color scale will be log-transformed to better-show minute variations at small values. Zero will still be plotted. 
#' @param map_title Optional.
#' @param legend_title Optional.
#' @param lat_range Two-element vector of latitudinal range
#' @param long_range Two-element vector of longitudinal range
#' @param land_fill Land fill color
#' @param land_line Land border color
#' @param land_stroke Land border thickness
#' @param land_alpha Land fill alpha
#' @param water_fill Water fill color
#' @param water_alpha Water fill alpha
#'
#' @return Plot.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import viridis
#' @import scales
#'
gg_kfs_grid <- function(values,
                        kfs_grid = NULL,
                        point_size = 2.35,
                        color_map = 'viridis',
                        scale_breaks = NULL,
                        scale_limits = NULL,
                        pseudo_log = FALSE,
                        map_title = NULL,
                        legend_title = 'Values',
                        lat_range = c(52.8, 53.55),
                        lon_range = c(-129.68, -128.85),
                        land_fill = I('#d5dcde'),
                        land_line = 'grey30',
                        land_stroke = .25,
                        land_alpha = 1,
                        water_fill = 'lightblue',
                        water_alpha = .1){
  
  if(FALSE){  #=================================================================
    data(grid, package='bangarang')
    kfs_grid <- grid
    values = grid$z
    point_size = 2.35
    color_map = 'viridis'
    map_title = 'Map title'
    scale_breaks <- c(0,100,200,300,400,500,600)
    scale_limits <- NULL
    pseudo_log = TRUE
    legend_title = 'Values'
    lat_range = c(52.8, 53.55)
    lon_range = c(-129.68, -128.85)
    land_fill = I('#d5dcde') # darkslategrey alpha .2
    land_line = 'slategray3'
    land_stroke = .25
    land_alpha = 1
    water_fill = 'lightblue'
    water_alpha = .1
    
    gg_kfs_grid(values, legend_title = 'values')
  } #===========================================================================
  
  if(is.null(kfs_grid)){
    data(grid, package='bangarang')
    kfs_grid <- grid
  }
  
  gridr <- kfs_grid
  gridr$varcol <- values
  head(gridr)
  
  # Plot it
  data(nepac_shore_sf, package='bangarang')
  (vt <- ifelse(pseudo_log,
                scales::transform_pseudo_log(sigma = 0.001),
                'identity')[[1]])
  p <- 
    ggplot2::ggplot() +
    geom_point(data=gridr,
               mapping=aes(x=x, y=y,
                           color=varcol),
               pch=15,
               cex=point_size)  +
    ggplot2::scale_color_viridis_c(name=legend_title,
                                   breaks=scale_breaks,
                                   limits=scale_limits,
                                   transform = vt,
                                   option = color_map) +
    ggplot2::geom_sf(data=nepac_shore_sf,
                     fill=land_fill,
                     alpha=land_alpha,
                     color=land_line, 
                     linewidth=land_stroke) +
    ggplot2::coord_sf(ylim=lat_range,
                      xlim=lon_range) + 
    ggplot2::theme_light() + 
    theme(panel.background = element_rect(fill = adjustcolor(water_fill, alpha.f=water_alpha),
                                          size = 0.5, linetype = "solid")) + 
    xlab(NULL) + 
    ylab(NULL) + 
    labs(title = map_title)
  
  return(p)
  
}
