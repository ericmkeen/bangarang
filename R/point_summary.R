#' Point estimate summary
#'
#' @param point_estimate Dataframe of point estimates the same length as the number of rows in `grid`
#'
#' @return Summary table
#' @export
#' @import dplyr
#'
point_summary <- function(point_estimate){
  point_estimate %>%
    group_by(year, circuit, circuit_name, circuit_yday) %>%
    summarize(dmean = mean(d),
              dsd = sd(d)) %>%
    ungroup() 
}