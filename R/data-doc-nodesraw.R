#' List of travel nodes used for shortest-path calculations in the Kitimat Fjord System.
#'
#' The primary reference for the location and ID of each travel node, the coordinate network
#' used in the routeKFS pathfinding functions. 
#'  
#' @format A dataframe with 93 rows and 4 columns, formatted for easy mapping as EventData in PBSmapping. 
#' The fourth column, "block", is used to reference the dataset \code{deadendkey}. 
#' Column "EID" is the unique identifier for each node, and corresponds to the rows and columns in datasets
#' \code{nodesLOSmatrix} and \code{NodesFirstStepsMatrix}.
#' @source Manually compiled by Eric Keen
"nodesraw"



