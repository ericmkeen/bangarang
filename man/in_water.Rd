% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/in_water.R
\name{in_water}
\alias{in_water}
\title{Are coordinates properly within the water within the KFS?}
\usage{
in_water(xy, filter_to_valid = TRUE, toplot = FALSE)
}
\arguments{
\item{xy}{Dataframe with coordinates, decimal degrees, in columns 'x' and 'y'}

\item{filter_to_valid}{Boolean, should the dataset be filtered to only valid coordinates?}

\item{toplot}{Boolean with default `FALSE`.}
}
\value{
The `xy` dataframe, filtered only to coordinates that are (1) in the KFS and (2) not on land.
}
\description{
Are coordinates properly within the water within the KFS?
}
