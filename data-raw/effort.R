# Effort datasets

library(dplyr)
library(ggplot2)

load('data-raw/lta/effort13.RData')
load('data-raw/lta/effort14.RData')

names(effort13)
head(effort13)

#===============================================================================

# Draw from locally stored 2013 data
subdir <- "/Users/ekezell/Downloads/bangarang data/B13package/effort13/"
(lf <- list.files(subdir) %>% rev)

i=1
mr <- data.frame()
for(i in 1:length(lf)){
  (lfi <- paste0(subdir,lf[i]))
  message(lfi)
  mri <- read.csv(lfi)
  head(mri)
  mri <- mri[mri$ev == 'POS',]
  if(i==1){nami <- names(mri)}
  nami
  
  if(identical(names(mri), nami)){
  }else{
    message('Columns do not match: ', lfi)
    nami
    names(mri)
    mri <- data.frame(mri[, 1:9],
               mode = 'LITE',
               bft = NA,
               swell = NA,
               per.cld = NA,
               vis = NA, 
               precip = NA,
               glare.L = NA,
               glare.R = NA,
               curr.str = NA,
               curr.dir = NA,
               spd = mri$spd,
               hdg = mri$hdg, 
               mag = 19, 
               rpm = 1800, 
               wx.valid = FALSE,
               app.dir = NA,
               app.spd = NA,
               true.dir = NA, 
               true.spd = NA,
               baro.merc = NA,
               baro.bars = NA,
               temp = NA,
               tsg.valid = FALSE,
               tsg.temp = NA, 
               tsg.con = NA, 
               tsg.sal = NA, 
               obs1 = NA, 
               pos1 = NA, 
               obs2 = NA, 
               pos2 = NA, 
               obs3 = NA, 
               pos3 = NA, 
               XX = NA, 
               X1 = NA, 
               X2 = NA, 
               X3 = NA, 
               X4 = NA, 
               X5 = NA,
               X6 = NA, 
               X7 = NA, 
               X8 = NA, 
               X9 = NA, 
               X10 = NA, 
               X11 = NA, 
               X12 = NA, 
               X13 = NA, 
               X14 = NA, 
               X15 = NA, 
               X16 = NA, 
               X17  = NA, 
               X18 = NA, 
               X19 = NA, 
               X20 = NA, 
               X21 = NA, 
               X22 = NA, 
               X23 = NA, 
               X24 = NA, 
               index = NA)
  }
  
  mr <- rbind(mr, mri)
}

effort13 <- 
  mr %>% 
  arrange(date) %>% 
  rename(event = ev,
         lon = long,
         circuit = circ,
         effort = eff, 
         perc_cloud = per.cld,
         current_strength = curr.str,
         current_dir = curr.dir,
         knots = spd,
         mag_offset = mag,
         glare_L = glare.L,
         glare_R = glare.R,
         wx_valid = wx.valid,
         wind_hdg_raw = app.dir,
         wind_kph_raw = app.spd,
         air_temp = temp,
         tsg_valid = tsg.valid,
         ss_temp = tsg.temp,
         ss_sal = tsg.sal) %>% 
  select(event:lon,
         knots:mag_offset,
         circuit:mode, 
         effort:echo,
         wx_valid, tsg_valid,
         obs1:pos3,
         bft:current_dir,
         wind_hdg_raw, wind_kph_raw,
         air_temp, ss_temp, ss_sal)

head(effort13)
tail(effort13)
nrow(effort13)

# Decimate to one position update per minute
eff13 <- 
  effort13 %>% 
  mutate(date = lubridate::as_datetime(date)) %>% 
  mutate(minute = floor(as.numeric(date)/60)) 
  
(mins <- eff13$minute %>% unique) %>% length
new_eff <- data.frame()
i=1
for(i in 1:length(mins)){
  (mini <- mins[i])
  (effi <- eff13 %>% filter(minute == mini)) %>% nrow
  mini_keep <- effi[1,]
  (this_effort <- effi$effort[1])
  j=1
  for(j in 1:nrow(effi)){
    (effj <- effi[j,])
    if(effj$effort != this_effort){
      this_effort <- effj$effort
      mini_keep <- rbind(mini_keep, effj)
    }
  }
  new_eff <- rbind(new_eff, mini_keep)
  message(i)
}

nrow(new_eff)
new_eff %>% select(date, effort, minute) %>% head(50)
new_eff$minute %>% table %>% table
effort13 <- new_eff %>% select(-minute)

#===============================================================================

load('data-raw/lta/effort14.RData')
names(effort14)
effort14 <- 
  effort14 %>% 
  rename(event = ev,
         lon = long,
         circuit = circ,
         effort = eff, 
         perc_cloud = per.cld,
         current_strength = curr.str,
         current_dir = curr.dir,
         knots = spd,
         mag_offset = mag,
         glare_L = glare.L,
         glare_R = glare.R,
         wx_valid = wx.valid,
         wind_hdg_raw = app.dir,
         wind_kph_raw = app.spd,
         air_temp = temp,
         tsg_valid = tsg.valid,
         ss_temp = tsg.temp,
         ss_sal = tsg.sal) %>% 
  select(event:lon,
         knots:mag_offset,
         circuit:mode, 
         effort:echo,
         wx_valid, tsg_valid,
         obs1:pos3,
         bft:current_dir,
         wind_hdg_raw, wind_kph_raw,
         air_temp, ss_temp, ss_sal)

head(effort14)

#===============================================================================

load('data-raw/lta/effort15.RData')
names(effort15)
effort15 <- 
  effort15 %>% 
  rename(event = ev,
         lon = long,
         effort = eff, 
         circuit = circ,
         perc_cloud = per.cld,
         current_strength = curr.str,
         current_dir = curr.dir,
         knots = spd,
         mag_offset = mag,
         glare_L = glare.L,
         glare_R = glare.R,
         wx_valid = wx.valid,
         wind_hdg_raw = app.dir,
         wind_kph_raw = app.spd,
         air_temp = temp,
         tsg_valid = tsg.valid,
         ss_temp = tsg.temp,
         ss_sal = tsg.sal) %>% 
  select(event:lon,
         knots:mag_offset,
         circuit:mode, 
         effort:echo,
         wx_valid, tsg_valid,
         obs1:pos3,
         bft:current_dir,
         wind_hdg_raw, wind_kph_raw,
         air_temp, ss_temp, ss_sal)

head(effort15)

#===============================================================================
# Combine

effort <- rbind(effort13, effort14, effort15)
head(effort)
tail(effort)
names(effort)

#===============================================================================
# Clean up

effort$mode <- NULL

effort <- 
  effort %>% 
  mutate(event = as.character(event),
         date = lubridate::as_datetime(date),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         knots = as.numeric(knots),
         hdg = as.numeric(hdg),
         mag_offset = as.numeric(mag_offset),
         circuit = as.numeric(circuit),
         block = as.character(block),
         effort = as.numeric(effort),
         pam = as.numeric(pam),
         echo = as.numeric(echo),
         bft = as.numeric(bft),
         perc_cloud = as.numeric(perc_cloud),
         glare_L = as.numeric(glare_L),
         glare_R = as.numeric(glare_R),
         wind_hdg_raw = as.numeric(wind_hdg_raw),
         wind_kph_raw = as.numeric(wind_kph_raw),
         air_temp = as.numeric(air_temp),
         ss_temp = as.numeric(ss_temp),
         ss_sal = as.numeric(ss_sal))

effort$mag_offset %>% table
effort$lat %>% range(na.rm=TRUE)
effort$lon %>% range(na.rm=TRUE)
effort$circuit %>% table
effort$bft %>% table

effort$bft[effort$bft == 10] <- 1
effort$bft[effort$bft == 25] <- 2
effort$bft[effort$bft == 40] <- 4
effort$bft[effort$bft == 5] <- 4
effort$bft[effort$bft == 201] <- 2

effort$block <- NULL
# effort$block <- gsub(' ','',effort$block)
# effort$block %>% table
# effort$block[effort$block == ''] <- NA
# effort$block[effort$block == 0] <- NA
# effort$block[effort$block == 1] <- NA
# effort$block[effort$block == 2] <- NA
# effort$block[effort$block == 5] <- NA
# effort$block[effort$block == '3\\'] <- NA

effort$wx_valid <- NULL
effort$tsg_valid <- NULL
# effort$wx_valid <- gsub(' ','',effort$wx_valid)
# effort$wx_valid %>% table
# effort$wx_valid[effort$wx_valid == ''] <- FALSE
# effort$wx_valid[effort$wx_valid == 'Bad'] <- FALSE
# effort$wx_valid[effort$wx_valid == 'Valid'] <- TRUE

effort$ss_temp[effort$ss_temp > 20] <- NA
effort$ss_temp[effort$ss_temp < 5] <- NA
effort$ss_temp %>% plot

effort$ss_sal[effort$ss_sal < 5] <- NA
effort$ss_sal %>% plot

effort$wind_kph_raw[effort$wind_kph_raw > 50] <- NA
effort$wind_kph_raw %>% plot
effort$wind_hdg_raw %>% plot

effort$hdg %>% plot
effort$knots %>% plot
effort$glare_L %>% plot
effort$glare_R %>% plot

effort$air_temp[effort$air_temp > 100] <- NA
effort$air_temp[effort$air_temp < 5] <- NA
effort$air_temp %>% plot

effort$pam <- NULL
effort %>% names

effort$echo %>% table
effort$echo[effort$echo == 0] <- FALSE
effort$echo[effort$echo == 1] <- TRUE

effort$effort %>% table
effort$effort <- as.character(effort$effort)
effort$effort[effort$effort == '0'] <- 'off'
effort$effort[effort$effort == '1'] <- 'transect'
effort$effort[effort$effort == '2'] <- 'station'
effort$effort[effort$effort == '3'] <- 'station'
effort$effort[effort$effort == '4'] <- 'with whale'
effort$effort[effort$effort == '5'] <- 'casual'

effort$event %>% table
effort$event <- NULL

effort$perc_cloud %>% sort %>% table
effort$perc_cloud[effort$perc_cloud > 100] <- 100

effort$swell <- NULL
effort$mag_offset <- NULL

effort$current_strength <- NULL
effort$current_dir <- NULL

effort$precip <- gsub(' ','',effort$precip)
effort$precip %>% table
effort$precip[effort$precip == 'CLR'] <- 'clear'
effort$precip[effort$precip == 'DRZ'] <- 'drizzle'
effort$precip[effort$precip == 'FOG'] <- 'fog'
effort$precip[effort$precip == 'HAZ'] <- 'haze'
effort$precip[effort$precip == 'POR'] <- 'pouring'
effort$precip[effort$precip == ''] <- NA
effort$precip[!is.na(as.numeric(effort$precip))] <- NA

effort$vis <- gsub(' ','',effort$vis)
effort$vis %>% table
effort$vis[effort$vis == ''] <- NA
effort$vis[effort$vis == '<1km'] <- '.5'
effort$vis[effort$vis == '<5km'] <- '2.5'
effort$vis[effort$vis == '<Inf'] <- '30'
effort$vis[effort$vis == 'CLR'] <- NA
effort$vis[effort$vis == '100'] <- NA
effort$vis[effort$vis > 30] <- 30

head(effort)
tail(effort)

#===============================================================================
# Add lag info

effi <- 
  effort %>% 
  mutate(this_date = lubridate::date(date),
         next_date = lead(this_date),
         next_dt = lead(date),
         next_effort = lead(effort),
         next_circuit = lead(circuit),
         next_lat = lead(lat),
         next_lon = lead(lon)) %>% 
  mutate(lead_sec = as.numeric(next_dt) - as.numeric(date)) %>% 
  mutate(lead_test = ifelse(this_date == next_date &
                              effort == next_effort &
                              circuit == next_circuit & 
                              lead_sec < 3*60 & 
                              lead_sec != 0 , 
                            TRUE, FALSE)) %>% 
  mutate(seconds = ifelse(lead_test, lead_sec, 2*60))

effi$lead_sec %>% hist
effi$lead_sec %>% table
effi$lead_test %>% table

kms <- c()
for(i in 1:nrow(effi)){
  (lat1 = effi$lat[i])
  (lat2 = effi$next_lat[i])
  (lon1 = effi$lon[i])
  (lon2 = effi$next_lon[i])
  if(all(!is.na(c(lat1, lat2, lon1, lon2)))){
    kmi <- swfscMisc::distance(lat1, lon1, lat2, lon2, units='km')
  }else{
    kmi <- NA
  }
  kms <- c(kms, kmi)
}

length(kms)
nrow(effi)
effi$km <- kms

effi <- 
  effi %>% 
  mutate(km = ifelse(lead_test, km, knots*((seconds)/(60*60))))

effi$km %>% hist

effort <- effi %>% select(- this_date, -next_date, -next_dt, -next_effort, -next_circuit, 
                          - next_lat, -next_lon, -lead_sec, -lead_test)

effort %>% head
effort %>% tail

effort$seconds %>% hist
effort$km %>% hist

# Take away final row
effort <- effort[1:(nrow(effort)-1), ]
tail(effort)

#===============================================================================
# Save

getwd()
save(effort, file='data-raw/effort.rds')

library(usethis)
use_data(effort)
