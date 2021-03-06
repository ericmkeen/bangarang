% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LOStest.R
\name{LOStest}
\alias{LOStest}
\title{Line-of-Sight test for two water locations}
\usage{
LOStest(pts.ocean, buffer = 0.01, toplot = TRUE)
}
\arguments{
\item{pts.ocean}{A list containing two coordinate pairs. Format: \code{list(c(X1,Y1),c(X2,Y2))}.}

\item{buffer}{Distance (in decimal degrees) to increase the boundaries of shoreline under consideration beyond the range defined by the two points.}

\item{toplot}{If \code{TRUE} (the default), then two plot will be generated to show the process.
The top plot is a "context" plot showing the two points.
The bottom plot is a zoom of the two points. A blue line connects them,
and the green line along the shore indicates shoreline coordinates that have been tested for line-of-sight obstruction.
When an obstruction has been found, a circle marks the shoreline intersection point (corresponding to element two of returned list).}
}
\value{
Returns a two element list. The first list is a Boolean: If \code{TRUE},
then the two points are indeed line-of-sight with no shore obstructions. If \code{FALSE},
then the land is obstructing the most direct path between the points.
The second element in the list is a 2-element vector, the X and Y of the intersection point with shore.
This vector is \code{c(NA,NA)} when the two points are line of sight.
}
\description{
Determines whether land is obstructing a direct path between two pairs of ocean coordinates.
}
\details{
A test function used frequently throughout the \code{routeKFS} function, but can be used on its own for any part of the
northeast Pacific coastline (Oregon to Alaska). The function takes a shoreline dataset from the package \code{PBSmapping},
and explores nearby shoreline vertices (in pairs) to see if the line connecting them intersects
with a straight path between the two ocean points in question. Then, if the solution of the two lines
is contained with a box that bounds the two shoreline vertices, the function concludes that
the shore must be obstructing line of sight.
}
\examples{
### Define the two ocean pts
xO1 <- -129.3
yO1 <- 53.3
xO2 <- -129.23
yO2 <- 53.33
# Combine into list
pts.ocean <- list(c(xO1,yO1),c(xO2,yO2))
# Run the test:
LOStest(pts.ocean)
}
\author{
Eric Keen, Scripps Institution of Oceanography, \email{ekeen@ucsd.edu}
}
\seealso{
\code{\link{whalemap}}, \code{\link{routeKFS}}, \code{\link{solve2lines}}
}

