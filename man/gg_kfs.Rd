% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_kfs.R
\name{gg_kfs}
\alias{gg_kfs}
\title{`ggplot()` of the Kitimat Fjord System}
\usage{
gg_kfs(
  lat_range = c(52.8, 53.55),
  lon_range = c(-129.68, -128.85),
  land_fill = "darkslategrey",
  land_line = "grey30",
  land_stroke = 0.25,
  land_alpha = 0.2,
  water_fill = "lightblue",
  water_alpha = 0.1
)
}
\arguments{
\item{lat_range}{Two-element vector of latitudinal range}

\item{land_fill}{Land fill color}

\item{land_line}{Land border color}

\item{land_stroke}{Land border thickness}

\item{land_alpha}{Land fill alpha}

\item{water_fill}{Water fill color}

\item{water_alpha}{Water fill alpha}

\item{long_range}{Two-element vector of longitudinal range}
}
\value{
Returns a `ggplot()` map object.
}
\description{
`ggplot()` of the Kitimat Fjord System
}
