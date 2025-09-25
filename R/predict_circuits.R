#' Predict DSM densities for each circuit of effort
#'
#' @param dsm_keep DSM model object to use.
#' @param segments Dataframe of segment summary data.
#' @param grid Grid dataframe: see `data(grid)`
#' @param circuits Dataframe with start/end timestamps for each circuit. See `data(circuit)` for reference.  
#' @param cex_scale Scale points on DSM maps to be more legible using a number (1 = no scaling).
#' @param toplot `TRUE` or `FALSE`?
#'
#' @return A dataframe with the grid replicated for each circuit, with an additional column, 
#' `d`, containing the density estimate for each grid-cell in each circuit.  
#' 
#' @export
#'
predict_circuits <- function(dsm_keep, 
                             segments, 
                             grid,
                             circuits = NULL,
                             cex_scale = 1, 
                             toplot=FALSE){
  
  segs <- segments
  
  if(is.null(circuits)){
    circuits <- segs$circuit %>% unique %>% sort
  }else{
    circuits <- circuits$circuit
  }
  
  #segs <- segment.data
  #obs <- observation.data
  #toplot <- TRUE ; cex_scale = 1
  results <- data.frame()
  #(circuits <- segs$circuit %>% unique)
  i=1
  for(i in 1:length(circuits)){
    (circi <- circuits[i])
    message(' --- --- predicting for circuit ', circi, ' ...')
    (segi <- segs %>% filter(circuit == circi))
    predf <- grid
    ds <- predict_dsm_kfs(dsm_keep,
                          segments = segi,
                          grid = predf,
                          cex_scale = cex_scale,
                          toplot=toplot)
    predf$d <- ds
    predf$year <- segi$year[1]
    predf$circuit <- circi
    predf$circuit_name <- segi$circuit_name[1]
    predf$circuit_yday <- segi$circuit_yday[1]
    results <- rbind(results,predf)
  }
  return(results)
}
