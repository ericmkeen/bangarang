#' Create a bootstrap of segments and their sightings
#'
#' @param segments `data.frame` of segment summary data (each row is a segment),
#' with a columns named `Sample.Label`.  
#' 
#' @param sightings `data.frame` of sightings (each row is a sighting),
#' with a column named `Sample.Label`.  
#'
#' @return A `list` with two slots: a bootstrap version of `segments` and a bootstrap version of `sightings`. 
#' Each `data.frame` has a revised `Sample.Label` column and a new column, `old_Sample`, which records the original sample label.
#' 
#' @export
#' @import dplyr
#' @import tidyr
#'
bootstrapper <- function(segments, sightings){
  
  if(FALSE){
    data("segments_5km")
    data(seabirds)
    segments_5km %>% head
    seabirds %>% head
    seabirds <- segment_sync(segments_5km, seabirds)
    seabirds %>% names
    segments <- segments_5km
    sightings <- seabirds
    
    results <- bootstrapper(segments, sightings)
    results$segments$old_Sample %>% table %>% hist
    nrow(segments)
    nrow(results$segments)
    nrow(sightings)
    nrow(results$sightings)
  }
  
  # Make segment key
  segs <- unique(segments$Sample.Label)
  (seg_key <- 
    data.frame(Sample.Label = sample(segs, size=length(segs), replace=TRUE)) %>% 
    mutate(new_Sample = 1:n()))
  
  # Loop through each segment
  new_segs <- data.frame()
  new_sits <- data.frame()
  i=1
  for(i in 1:nrow(seg_key)){
    (keyi <- seg_key[i,])
    (segi <- 
        segments %>% 
        filter(Sample.Label == keyi$Sample.Label) %>% 
        mutate(old_Sample = Sample.Label) %>% 
        mutate(Sample.Label = keyi$new_Sample))
    new_segs <- rbind(new_segs, segi)
    
    (siti <- sightings %>% filter(Sample.Label == keyi$Sample.Label))
    if(nrow(siti) > 0){
      (siti <- 
        siti %>% 
        mutate(old_Sample = Sample.Label) %>% 
        mutate(Sample.Label = keyi$new_Sample))
      new_sits <- rbind(new_sits, siti)
    }
  }
  
  results <- list(segments = new_segs,
                  sightings = new_sits)
  return(results)
}