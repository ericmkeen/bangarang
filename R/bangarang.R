#' bangarang: A package of R tools for data from the Kitimat Fjord System
#'
#' bangarang is a collection of functions useful 
# 'for the analysis of survey and oceanography data 
#' in the Kitimat Fjord System, British Columbia 
#' (Gitga'at First Nation marine territory), 
#' though some functions are applicable to the 
#' northeast Pacific coast in general. 
#' Developed for the Bangarang Project 
#' but generalized for use by the 
#' Gitga'at Oceanographic Initiative, 
#' NCCS, DFOs and others.
#' 
#' @section Primary functions:
#' The primary functions in this package are:
#' \describe{
#' \item{\code{\link{plotKFS}}}{Mapping tools}
#' \item{\code{\link{whalemap}}}{Locating a whale in confined channels based on survey data}
#' \item{\code{\link{routeKFS}}}{Travel distance between two points in the fjord system (around islands)}
#' }
#' 
#' @section Secondary functions:
#' There are some secondary functions used by these primaries,
#' and they may be useful in and of themselves:
#' \describe{
#' \item{\code{\link{solve2lines}}}{Determine the intersection point of two lines}
#' \item{\code{\link{LOStest}}}{Determine whether two points are within line-of-sight 
#' of each other or whether the shoreline is in the way}
#' }
#' 
#' @section Background functions:
#' These functions also have small helper functions that 
#' probably don't have a use otherwise or on their own:
#' \describe{
#' \item{\code{\link{calcOD}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{addturn}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{closest2end}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{find.LOS.nodes}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{dist2nodes}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{find.unusables}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{oceandistance}}}{in \code{\link{routeKFS}}}
#' \item{\code{\link{plotOD}}}{in \code{\link{routeKFS}}}
#' }
#' 
#' @author Eric Keen, Scripps Institution of Oceanography, \email{ekeen@@ucsd.edu} 

#' @docType package
#' @name bangarang
NULL
#> NULL