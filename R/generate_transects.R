#' Generate a transect plan
#' 
#' This function generates a new random layout of transect lines within the Kitimat Fjord System study area
#' using the study design generated with the `R` package `dssd` and saved in the `bangarang` dataset `study_design`, 
#' which is meant to guide long-term line-transect distance sampling surveys by Gitga'at collaborators for the years 2024 and beyond.  
#'
#' @param write_to The directory to which files should be saved; default is your working directory. 
#' @param show_plot Print plot of transect plan?
#' @param save_to_file Save transect plan to file?
#' @param verbose Print status updates to the Console?
#' @param my_crs The projection you wish to use when writing the layout to file. Default is WS84, whicih is suitable for British Columbia, Canada. 
#'
#' @return Unless options are turned off, this function will display a map of the newly generated transect layout, then save that transect layout to a 
#' map file (PDF), a spreadsheet (CSV), and to a GPX file, which can then be loaded onto a boat chartplotter of handheld GPS in order to guide observers along the transects. 
#' @export
#'
generate_transects <- function(write_to = '.',
                               show_plot = TRUE,
                               save_to_file = TRUE,
                               verbose=TRUE,
                               my_crs = 4326){
  
  if(FALSE){
    # code for debugging
    write_to = '.'
    show_plot = TRUE
    save_to_file = TRUE
    verbose=TRUE
    my_crs = 4326
  }
  
  # Store system time
  (stime <- gsub('-','',gsub(':','',round(Sys.time()))))
  
  # Load study design
  data("study_design", package='bangarang')
  #study_design$transects_ex
  
  # Generate transects
  if(verbose){message('Generating a new random permutation of the transect layout...')}
  transects <- dssd::generate.transects(study_design$design)
  
  if(verbose){
    print(transects)
    message('')
  }
  
  # Prepare plot
  p <- dssd::plot(study_design$region, 
                  transects, covered.area=TRUE, main='') + 
    ggplot2::labs(title = paste0('Generated at: ', stime))
  
  if(show_plot){
    if(verbose){message('Producing map...')}
    print(p)
  }
  
  if(save_to_file){
    
    # Save plot
    (fn <- paste0(write_to,'/transects ',stime,'.pdf'))
    if(verbose){message('Saving map to ', fn,' ...')}
    ggplot2::ggsave(fn, p, width=6, height=6.5)
    
    # Write csv
    (fn <- paste0(write_to,'/transects ',stime,'.csv'))
    if(verbose){message('Saving CSV to ', fn,' ...')}
    dssd::write.transects(transects, 
                          dsn = fn,
                          proj4string = my_crs)
    
    # Write gpx
    (fn <- paste0(write_to,'/transects ',stime,'.gpx'))
    if(verbose){message('Saving GPX to ', fn,' ...')}
    tgpx <- transects
    tgpx@samplers <- sf::st_transform(tgpx@samplers, 
                                      my_crs)
    sf.column <- attr(tgpx@samplers, "sf_column")
    tgpx@samplers <- sf::st_cast(tgpx@samplers, 
                                 "MULTILINESTRING")
    sf::st_write(tgpx@samplers[[sf.column]], 
                 dsn = fn, 
                 driver = "GPX", 
                 dataset_options = "GPX_USE_EXTENSIONS=YES",
                 layer = 'lines')
  }
  
  if(verbose){
    message('')
    message('Finished!')
  }
  
}
