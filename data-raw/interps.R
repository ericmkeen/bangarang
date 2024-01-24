# Interpolated dataframes

library(dplyr)
library(ggplot2)
library(usethis)

getwd()

#===============================================================================
# 2013

subdir <- "/Users/ekezell/Downloads/bangarang data/interp-2013/csv/"
(lf <- list.files(subdir))

i=1
mr <- data.frame()
for(i in 1:length(lf)){
  (lfi <- paste0(subdir,lf[i]))
  (lfname <- lf[i])
  message(lfname)
  splits <- strsplit(lfname, '-')[[1]]
  mri <- read.csv(lfi)
  head(mri)
  mrii <- 
    mri %>%
    mutate(year = splits[2],
           circuit = splits[3],
           metric = splits[1]) %>% 
    select(year:metric, 
           lat = Y, 
           lon = X, 
           value = Z,
           color = zcol)
  mr <- rbind(mr, mrii)
}

nrow(mr) 
mr13 <- mr

#===============================================================================
# 2014

subdir <- "/Users/ekezell/Downloads/bangarang data/interp-2014/csv/"
(lf <- list.files(subdir))

i=1
mr <- data.frame()
for(i in 1:length(lf)){
  (lfi <- paste0(subdir,lf[i]))
  (lfname <- lf[i])
  message(lfname)
  splits <- strsplit(lfname, '-')[[1]]
  mri <- read.csv(lfi)
  head(mri)
  mrii <- 
    mri %>%
    mutate(year = splits[2],
           circuit = splits[3],
           metric = splits[1]) %>% 
    select(year:metric, 
           lat = Y, 
           lon = X, 
           value = Z,
           color = zcol)
  mr <- rbind(mr, mrii)
}

nrow(mr) 
mr14 <- mr

#===============================================================================
# 2015

subdir <- "/Users/ekezell/Downloads/bangarang data/interp-2015/csv/"
(lf <- list.files(subdir))

i=1
mr <- data.frame()
for(i in 1:length(lf)){
  (lfi <- paste0(subdir,lf[i]))
  (lfname <- lf[i])
  message(lfname)
  splits <- strsplit(lfname, '-')[[1]]
  mri <- read.csv(lfi)
  head(mri)
  mrii <- 
    mri %>%
    mutate(year = splits[2],
           circuit = splits[3],
           metric = splits[1]) %>% 
    select(year:metric, 
           lat = Y, 
           lon = X, 
           value = Z,
           color = zcol)
  mr <- rbind(mr, mrii)
}

nrow(mr) 
mr15 <- mr

#===============================================================================
# Combine

mr <- rbind(mr13, mr14, mr15)
nrow(mr)

ocean <- mr
save(ocean, file='data-raw/ocean.rds')
use_data(ocean)
