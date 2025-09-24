#' Wrapper for bootstrap routine for density estimation for each circuit
#'
#' @param dsm_formula String
#' @param segments Segment data
#' @param observation_data Formatted observation (sightings) data
#' @param grid Grid dataframe (see `data(grid)`)
#' @param group_draw `TRUE` or `FALSE`: incorporate uncertainty 
#' from min-max-best group size uncertainty using a draw from a truncated log-normal distribution?
#' @param on_the_line `TRUE` or `FALSE`: incorporate uncertainty from sightings that occur on the outside boundary of the strip?
##' @param B Number of iterations
#' @param output_filename If you want the result to be saved to a data file in 
#' your working directory, specify filename here. File type will be `.rds`. 
#' @param toplot True or False
#'
#' @return Dataframe of bootstrap results. 
#' @export
#' @import dsm
#' @import dplyr

bootstrapper_wrapper <- function(dsm_formula,
                                 segments,
                                 observation_data,
                                 grid,
                                 group_draw = TRUE,
                                 on_the_line = FALSE,
                                 B = 1000,
                                 output_filename = 'bootstraps',
                                 toplot = FALSE){
  
  if(FALSE){
    dsm_formula <- 's(x,y) + s(yday) + year + s(z)'
  }
  
  # initiate object that can store the bootstrap results
  RESULTS <- data.frame()
  # for loop
  i = 1 # for debugging
  #B <- 1000
  for(i in 1:B){
    message('Iteration ', i)
    
    # Create a bootstrap permutation of the data:
    # same sample size of segments,
    # but composed of re-sampled segments *with replacement*
    # (some segments occur more than once, some not at all)
    # and the sightings from those re-sampled segments.
    # Sample.Labels are revised to stay unique.
    message('--- creating bootstrap dataset...')
    bootstrap <-
      bangarang::bootstrapper(segments = segments,
                              sightings = observation_data)
    #bootstrap$segments$old_Sample %>% table %>% hist
    boot.segment.data <- bootstrap$segments
    boot.observation.data <- bootstrap$sightings
    
    # Incorporate uncertainty from group size?
    if(group_draw){
      message('--- incorporating group size min-max-best uncertainty...')
      boot.observation.data <- group_size_draw(boot.observation.data)
    }
    
    # Incorporate uncertainty from on-the-line birds?
    if(on_the_line){
      message('--- incorporating uncertainty from groups on the outside line of the strip...')
      message('    UNDER CONSTRUCTION!')
    }
    
    # Re-run model with boostrapped data
    message('--- re-running DSM model...')
    
    # insert winning model here & rerun with bootstrap data
    dsm_form <- dsm_formula
    #(dsm_form <- paste0('density.est ~ ', dsm_formula))
    #print(dsm_form)
    dsm_keep <- dsm(formula = formula(dsm_form),
                    ddf.obj = dummy_ds,
                    segment.data = boot.segment.data,
                    observation.data = boot.observation.data,
                    convert.units=1/1000,
                    family=tw(),
                    method = "REML")
    ##
    
    # Predict density results for each month
    message('--- predicting monthly density surfaces...')
    boot_estimate <- predict_circuits(dsm_keep, 
                                      boot.segment.data, 
                                      grid,
                                      toplot = toplot)
    boot_estimate %>% as.data.frame %>% head
    
    # Add iteration column
    boot_estimate$iteration <- i
    
    # Add the results of this iteration to the overall
    # results data.frame
    RESULTS <- rbind(RESULTS, boot_estimate)
    message(' ')
    
    # Save RESULTS to an RDS object
    if(!is.null(output_filename)){
      fn <- paste0(output_filename,'.rds')
      save(RESULTS, file=fn)
    }
  }
  
  return(RESULTS)
}