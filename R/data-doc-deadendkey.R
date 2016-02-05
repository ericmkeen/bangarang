#' A key for straightest-path decision making between channels of the Kitimat Fjord System
#'
#' A symmetrical dataframe that lists the potentially viable channels to travel through for 
#' a given combination starting point (row name) and ending point (column name). If there are multiple
#' viable channels, they are separated by a \code{"-"}. Used behind the scenes in the \code{routeKFS} functions.
#'
#' @format A data frame with 13 rows and 14 columns (the first column is row.names). 
#' Row and column names are the same, and correspond to channels in the 
#' Kitimat Fjord System as they are partitioned in Bangarang analyses: 
#' caa, est, cmp, ash, ssq, nsq, wha, lew, wri, dug, mck, ver, bish
#' 
#' @source Written and compiled by Eric Keen
"deadendkey"



