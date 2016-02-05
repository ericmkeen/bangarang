#' Reference of which travel nodes are within Line-Of-Sight of each other in the Kitimat Fjord System
#'
#' A symmetrical dataframe that reports the line-of-sight status (yes or no) for every pairwise 
#' combination of travel nodes (n=93) in the dataset \code{nodesraw}.
#' 
#' @format A symmetricl data frame (93 rows x 93 columns) 
#' Row and column names are the same, and correspond to travel node ID number in dataset \code{nodesraw}. 
#' If travel nodes \code{i} and \code{j} are line-of-sight (LOS), then cells \code{[i,j]} and \code{[j,i]} are 1. 
#' If they are not LOS, these cells have 0.
#' 
#' @source Manually compiled by Eric Keen
"nodesLOSmatrix"



