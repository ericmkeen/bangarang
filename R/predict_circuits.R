predict_circuits <- function(dsm_keep, 
                             segments, 
                             circuits = NULL,
                             cex_scale = 1, 
                             toplot=TRUE){
  
  segs <- segments
  
  if(is.null(circuits)){
    circuits <- segs$circuit %>% unique %>% sort
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