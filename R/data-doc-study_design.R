#' Study design datasets for long-term monitoring, 2024 on.
#'
#' A list containing all elements needed to generate transects based on the long-term monitoring plan initiated in 2024. 
#' These materials were generated using the study design functions in the package `dssd`.
#' See the `bangarang` function `generate_transects()` for more information.  
#'
#' @format A list. 
#' \describe{
#'   \item{design}{The `design` object created by `dssd`.}
#'   \item{strata}{A `sf` `MULTIPOLYGON` of the strata boundaries.}
#'   \item{transects_ex}{A `ggplot2` object demonstrating an example of the transect layout produced by this study design.}
#'   \item{region}{The `region` object created by `dssd`.}
#'   \item{coverage}{The `coverage` object created by `dssd`.}
#'   ...
#' }
#' @source Written and compiled by Eric Keen.
"study_design"