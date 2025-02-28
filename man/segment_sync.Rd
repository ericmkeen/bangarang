% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segment_sync.R
\name{segment_sync}
\alias{segment_sync}
\title{Find the segment for each sightings}
\usage{
segment_sync(segments, sightings)
}
\arguments{
\item{segments}{`data.frame` of segment summary data (each row is a segment),
with columns named `date` and `Sample.Label`.}

\item{sightings}{`data.frame` of sightings (each row is a sighting),
with columns named `date` and `Sample.Label`.}
}
\value{
`sightings` `data.frame` with new column, `Sample.Label`.
}
\description{
Find the segment for each sightings
}
