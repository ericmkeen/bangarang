#' Reference of First Steps to take when traveling between node network in Kitimat Fjord System.
#'
#' A symmetrical dataframe that reports the first step to take when traveling the straightest possible 
#' path between nodes in the network of travel nodes laid out in the Kitimat Fjord System in the dataset \code{nodesraw}.
# This key lays out the framework for taking the most efficient steps between distant points in the KFS.
#' 
#' @format A symmetricl data frame (93 rows x 94 columns (col1 is rownames)) 
#' Row and column names correspond to travel node ID number in dataset \code{nodesraw}. 
#' Cell \code{[i,j]} reports which node to travel to next in order to get from node \code{i} to node \code{j}. 
#' If the nodes are line-of-sight of each other, their cell is marked with a lower case "x".
#' 
#' @source Manually compiled by Eric Keen
"NodesFirstStepsMatrix"



