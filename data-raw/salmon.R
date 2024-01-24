# Salmon

library(dplyr)
library(readr)
library(ggplot2)

#===============================================================================
# 2013

df <- read_csv("/Users/ekezell/Downloads/bangarang data/B13package/YUM13.csv")
head(df)
df %>% names

df13 <- 
  df %>% 
  mutate(side = NA) %>% 
  select(date, time, lat, lon=long, effort, echo, 
         circuit, bft, precip, 
         knots = vess.spd, hdg = vess.hdg, 
         jumps, zone = in.strip, line=close, side, comment)

df13$zone %>% table
df13$zone[df13$zone == 'True'] <- '12'
df13$zone[df13$zone == 'TRUE'] <- '12'
df13$zone[df13$zone == 'once each'] <- '12'
df13$zone[df13$zone != '12'] <- NA

df13$line %>% table
df13$line[df13$line == TRUE] <- 'MIDL'
df13$line[df13$line == FALSE] <- NA

df13$date %>% unique
df13$time %>% unique
df13$time <- stringr::str_pad(df13$time, width = 6, side='left',pad='0')

(yyyy <- substr(df13$date, 1,4))
(mo <- substr(df13$date, 5,6))
(dd <- substr(df13$date, 7,8))
(hh <- substr(df13$time, 1,2))
(mm <- substr(df13$time, 3,4))
(ss <- substr(df13$time, 5,6))
df13$date <- lubridate::as_datetime(paste0(yyyy,'-',mo,'-',dd,' ',hh,':',mm,':',ss))
df13$time <- NULL


#===============================================================================
# 2014

df <- read_csv("/Users/ekezell/Downloads/bangarang data/B14package/2014_salmon.csv")
head(df)
df %>% names

df14 <- 
  df %>% 
  select(date, lat, lon=long, effort = eff, echo, 
         circuit = circ, bft, precip, 
         knots = spd, hdg, 
         jumps, zone, line, side, comment)

#===============================================================================
# 2015

df <- read_csv("/Users/ekezell/Downloads/bangarang data/B15package/2015_salmon.csv")
head(df)
df %>% names

df15 <- 
  df %>% 
  select(date, lat, lon=long, effort = eff, echo, 
         circuit = circ, bft, precip, 
         knots = spd, hdg, 
         jumps, zone, line, side, comment)

#===============================================================================
# Join and polish

df <- rbind(df13, df14, df15)
df %>% head
df %>% tail
df <- 
  df %>% 
  mutate(hdg = as.numeric(hdg),
         circuit = as.numeric(circuit),
         knots = as.numeric(knots),
         jumps = as.numeric(jumps),
         zone = as.numeric(zone))

df$jumps %>% plot
df$jumps[df$jumps > 10] <- NA

df$date %>% plot
df$bft %>% plot
df$lat %>% plot
df$lat[df$lat > 53.8] <- NA
df$lat[df$lat < 52.5] <- NA
df$lon %>% plot
df$lon[df$lon < -129.8] <- NA
df$lon[df$lon > -128.8] <- NA
df$precip %>% table

df$precip %>% table
df$precip[df$precip == 'CLR'] <- 'clear'
df$precip[df$precip == 'CLR 210 245'] <- 'clear'
df$precip[df$precip == 'DRZ'] <- 'drizzle'
df$precip[df$precip == 'FOG'] <- 'fog'
df$precip[df$precip == 'HAZ'] <- 'haze'
df$precip[df$precip == 'POR'] <- 'pouring'
df$precip %>% table

df$effort %>% table
df$effort[df$effort == '0'] <- 'off'
df$effort[df$effort == '1'] <- 'transect'
df$effort[df$effort == '2'] <- 'station'
df$effort[df$effort == '3'] <- 'station'
df$effort[df$effort == '4'] <- 'with whale'
df$effort[df$effort == '5'] <- 'casual'

df$comment <- NULL

#===============================================================================
# Save it

df %>% head

salmon <- df
save(salmon, file='data-raw/salmon.rds')
use_data(salmon, overwrite=TRUE)

document()
data(seabirds)
seabirds %>% head

