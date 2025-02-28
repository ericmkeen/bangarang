#' Find the segment for each sightings
#'
#' @param segments `data.frame` of segment summary data (each row is a segment),
#' with columns named `date` and `Sample.Label`.  
#' 
#' @param sightings `data.frame` of sightings (each row is a sighting),
#' with columns named `date` and `Sample.Label`.  
#'
#' @return `sightings` `data.frame` with new column, `Sample.Label`.  
#' 
#' @export
#'
segment_sync <- function(segments, sightings){
  
  if(FALSE){
    data("segments_5km")
    data(seabirds)
    segments_5km %>% head
    seabirds %>% head
    segments <- segments_5km
    sightings <- seabirds
  }
  
  if('datetime' %in% names(segments)){
    segments$date <- segments$datetime
  }
  if('datetime' %in% names(sightings)){
    sightings$date <- sightings$datetime
  }
  
  dts <- sightings$date
  (x <- dts[1])
  sit_segments <-
    sapply(dts, function(x){
      diffs <- abs(as.numeric(difftime(x, segments$date, units='secs')))
      (mini <- which.min(diffs))
      (diffi <- diffs[mini])
      segi <- NA
      if(diffi <= 5400){
        segi <- segments$Sample.Label[mini[1]]
      }
      return(segi)
    })
  sightings$Sample.Label <- sit_segments
  
  return(sightings)
}

