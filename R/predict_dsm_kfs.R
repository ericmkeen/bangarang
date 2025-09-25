#' Predict density surface for segments using `dsm` model
#' 
#' @param dsm_keep Model object to use for prediction
#' @param segments Segment data to predict on
#' @param grid Grid dataframe (see `data(grid)`)
#' @param cex_scale  Scaling factor to scale the points in the plot.
#' @param toplot True or False
#'
#' @return Vector of density estimates to match the rows in the `grid` input
#' @export
#' @import dsm
#' @import dplyr
#' @import mrds
#' @import spatstat.geom
#' @import spatstat.explore
#' @import data.table
#'
predict_dsm_kfs <- function(dsm_keep, 
                            segments, 
                            grid, 
                            cex_scale = 1, 
                            toplot=FALSE){
  
  # Rename inputs
  grids <- grid
  #sample_table <- segments
  grids %>% head
  if(any(grepl('year', dsm_keep$formula))){
    grids$year <- segments$year %>% mean
  }
  if(any(grepl('yday', dsm_keep$formula))){
    grids$yday <- segments$yday %>% mean
  }
  
  # Predict on samples
  d_mean <- predict(dsm_keep, grids, off.set=5)
  grids$d_mean <- d_mean
  
  if(FALSE){
    d_mean <- predict(dsm_keep, sample_table, off.set=(sample_table$Effort))
    
    # Prepare raster
    df <- data.frame(x=sample_table$x, y=sample_table$y, z= d_mean)
    df <- df[complete.cases(df),] ; nrow(df)
    e <- raster::extent(df[,1:2])
    r <- raster::raster(e,ncol=150, nrow=150)
    x <- raster::rasterize(df[, 1:2], r, df[,3], fun=mean)
    
    # Interpolate with inverse distance weighting
    dfwin <- spatstat.geom::owin(xrange = range(grids$x), yrange = range(grids$y))
    dfppp <- spatstat.geom::ppp(x = df$x, y=df$y, window = dfwin, marks = df$z)
    xi <- spatstat.explore::idw(dfppp, at='pixels')
    class(xi)
    
    #  Prepare fine-scale grid of densities
    xdf <- as.data.frame(xi) ; head(xdf)
    z <- as.data.frame(xi) ; z %>% head
    names(z)[3] <- 'z'
    z %>% head
    nrow(z)
    nrow(xdf)
    xdf$value <- z$z
    xdf %>% head
    mean(xdf$value)
    
    # Now use fine-scale raster to find predicted density at each grids location
    dist <- function(a, b){
      dt <- data.table::data.table((df2$x-a)^2+(df2$y-b)^2)
      return(which.min(dt$V1))}
    df1 <- data.table::data.table(x=grids$x, y=grids$y) ; head(df1)
    df2 <- data.table::data.table(x=xdf$x, y=xdf$y) ; head(df2)
    results <- df1[, j = list(Closest =  dist(x, y)), by = 1:nrow(df1)]
    results
    grids$d_mean <- xdf$value[results$Closest]
  }
  
  # Test plot
  if(toplot){
    cexmax <- max(grids$d_mean, na.rm=TRUE)
    (grids$cexes <- (grids$d_mean) * cex_scale)
    #(grids$cexes <- (grids$d_mean / cexmax) * cex_scale)
    grids %>% head
    plot(y~x, data=grids,cex=cexes)
  }
  
  return(grids$d_mean)
}
