# Seabirds

library(dplyr)
library(readr)
library(ggplot2)

if(FALSE){
  #===============================================================================
  # 2013
  
  sib <- read_csv("/Users/ekezell/Downloads/bangarang data/B13package/SIB13.csv")
  head(sib)
  
  sib %>% names
  sib <- 
    sib %>% 
    mutate(wind_kph_raw = NA,
           wind_hdg_raw = NA,
           min = grp, 
           max = grp, 
           feed = NA,
           dir = NA,
           height = NA,
           plum1 = juv,
           plum2 = NA,
           plum3 = NA
    ) %>% 
    select(date:lat,
           lon = long,
           effort = eff,
           bft, 
           precip,
           knots = vess.spd,
           hdg = vess.hdg,
           wind_kph_raw,
           wind_hdg_raw,
           zone = in.strip,
           line = on.line,
           side = pos,
           best = grp,
           min, max,
           feed,
           motion = bhvr,
           dir,
           height,
           sp1 = spp1, per1 = spp1.per, plum1,
           sp2 = spp2, per2 = spp2.per, plum2,
           sp3 = spp3, per3 = spp3.per, plum3)
  
  sib %>% names
  
  
  sib$date %>% unique
  sib$time %>% unique
  sib$time <- stringr::str_pad(sib$time, width = 6, side='left',pad='0')
  
  (yyyy <- substr(sib$date, 1,4))
  (mo <- substr(sib$date, 5,6))
  (dd <- substr(sib$date, 7,8))
  (hh <- substr(sib$time, 1,2))
  (mm <- substr(sib$time, 3,4))
  (ss <- substr(sib$time, 5,6))
  sib$date <- lubridate::as_datetime(paste0(yyyy,'-',mo,'-',dd,' ',hh,':',mm,':',ss))
  sib$time <- NULL
  
  sib %>% head
  sib$zone %>% table
  sib$zone <- NA
  sib$line %>% table
  sib$line <- NA
  
  sib$sp1 %>% table
  sib %>% names
  
  (raw <- sib$motion)
  raw %>% table
  sib$motion[raw == 'FEED'] <- 'SIT'
  sib$motion[raw == '1F'] <- NA
  sib$motion[raw == 'FLOC'] <- 'FLY'
  sib$motion[raw == 'STAT'] <- 'SIT'
  sib$motion[raw == 'FLUS'] <- 'FLUSH'
  sib$motion[raw == 'FLYN'] <- 'FLY'
  sib$motion[raw == 'MIL'] <- 'SIT'
  sib$motion %>% table
  
  raw %>% table
  sib$feed <- 'N'
  sib$feed[raw == 'FEED'] <- 'Y'
  sib$feed[raw == 'FLOC'] <- 'M'
  sib$feed %>% table
  
  sib$dir
  sib$height
  
  sib13 <- sib
  
  #===============================================================================
  # 2014
  
  sib <- read_csv("/Users/ekezell/Downloads/bangarang data/B14package/2014_seabirds.csv")
  head(sib)
  
  sib %>% names
  
  sib <- 
    sib %>% 
    mutate(height = NA) %>% 
    select(date, lat, lon = long,
           effort = eff,
           bft,
           precip,
           knots = spd,
           hdg,
           wind_kph_raw = app.spd,
           wind_hdg_raw = app.dir,
           zone:line, side, 
           best:max,
           feed:dir,
           height,
           sp1:plum3)
  
  sib %>% head
  sib14 <- sib
  
  #===============================================================================
  # 2015
  
  sib <- read_csv("/Users/ekezell/Downloads/bangarang data/B15package/2015_seabirds.csv")
  head(sib)
  
  sib %>% names
  
  sib$sp3 %>% table
  
  sib <- 
    sib %>% 
    select(date, lat, lon = long,
           effort = eff,
           bft,
           precip,
           knots = spd,
           hdg,
           wind_kph_raw = app.spd,
           wind_hdg_raw = app.dir,
           zone:line, side, 
           best:max,
           feed:height,
           sp1:plum3)
  
  sib %>% head
  sib %>% names
  sib15 <- sib
  
  #===============================================================================
  
  cbind(names(sib13), names(sib14), names(sib15))
  sib <- rbind(sib13, sib14, sib15)
  
  sib %>% nrow
  sib %>% head
  sib %>% tail
  
  sib$hdg %>% sort %>% table
  
  sib <- 
    sib %>% 
    mutate(hdg = as.numeric(hdg),
           wind_kph_raw = as.numeric(wind_kph_raw), 
           wind_hdg_raw = as.numeric(wind_hdg_raw),
           best = as.numeric(best),
           min = as.numeric(min),
           max = as.numeric(max),
           per1 = as.numeric(per1),
           per2 = as.numeric(per2),
           per3 = as.numeric(per3))
  
  sib %>% head
  
  effort$precip %>% table
  sib$precip %>% table
  sib$precip[sib$precip == 'CLR'] <- 'clear'
  sib$precip[sib$precip == 'CLR 210 245'] <- 'clear'
  sib$precip[sib$precip == 'DRZ'] <- 'drizzle'
  sib$precip[sib$precip == 'FOG'] <- 'fog'
  sib$precip[sib$precip == 'HAZ'] <- 'haze'
  sib$precip[sib$precip == 'POR'] <- 'pouring'
  sib$precip %>% table
  
  sib$knots %>% plot
  sib$lat %>% plot
  sib$lat[sib$lat > 53.8] <- NA
  sib$lon %>% plot
  sib$lon[sib$lon < -129.8] <- NA
  sib$lon[sib$lon > -123] <- NA
  
  sib$hdg %>% plot
  sib$wind_kph_raw %>% plot
  sib$wind_hdg_raw %>% plot
  
  sib$zone %>% table
  sib$zone[sib$zone == '4'] <- 'OUT'
  
  sib$line %>% table
  
  sib$bft %>% table
  
  sib$side %>% table
  sib$side[sib$side == '6'] <- NA
  sib$side[sib$side == '8'] <- NA
  sib$side[sib$side == 'WBW'] <- NA
  sib$side[sib$side == 'LB STAR'] <- 'STAR'
  sib$side[sib$side == 'BW HELM'] <- 'HELM'
  sib$side[sib$side == 'MIDL'] <- 'HELM'
  
  sib$best %>% plot
  sib$min %>% plot
  sib$max %>% plot
  sib$per1 %>% plot
  sib$per2 %>% plot
  sib$per3 %>% plot
  
  sib <- 
    sib %>% 
    mutate(per1 = ifelse(is.na(per1), 0, per1)) %>% 
    mutate(per3 = ifelse(is.na(per3), 0, per3)) %>% 
    mutate(per2 = ifelse(per3 == 0, (100 - per1), per2))
  
  sib$date %>% plot
  
  sib$plum1 %>% table
  sib$plum1[sib$plum1 == '100'] <- NA
  sib$plum1[sib$plum1 == 'FALSE'] <- NA
  sib$plum1[sib$plum1 == 'TRUE'] <- NA
  
  sib$plum2 %>% table
  sib$plum2[sib$plum2 == '000'] <- NA
  
  sib$plum3 %>% table
  sib$plum3[sib$plum3 == '000'] <- NA
  
  sib$sp1 %>% unique %>% sort
  sib$sp1[sib$sp1 == 'Alcidspp.'] <- 'ALCD'
  sib$sp1[sib$sp1 == 'Arctictern'] <- 'ARTE'
  sib$sp1[sib$sp1 == 'Bald'] <- 'BAEA'
  sib$sp1[sib$sp1 == 'Baldeagle'] <- 'BAEA'
  sib$sp1[sib$sp1 == 'Baldeagle100'] <- 'BAEA'
  sib$sp1[sib$sp1 == 'Black-leggedkittiwake'] <- 'BLKI'
  sib$sp1[sib$sp1 == 'Blackscoter'] <- 'BLSC'
  sib$sp1[sib$sp1 == "Bonaparte'sgull"] <- 'BOGU'
  sib$sp1[sib$sp1 == "Bonaparte'sgullHer"] <- 'BOGU'
  sib$sp1[sib$sp1 == 'Californiagull'] <- 'CAGU'
  sib$sp1[sib$sp1 == "Cassin'sauklet"] <- 'CAAU'
  sib$sp1[sib$sp1 == 'Comm/Pac'] <- 'LOON'
  sib$sp1[sib$sp1 == 'CommonLoon'] <- 'COLO'
  sib$sp1[sib$sp1 == 'Commonmurre'] <- 'COMU'
  sib$sp1[sib$sp1 == 'Commontern'] <- 'COTE'
  sib$sp1[sib$sp1 == 'Fork-tailedstormpetrel'] <- 'FTSP'
  sib$sp1[sib$sp1 == 'Glaucous-wingedgull'] <- 'GWGU'
  sib$sp1[sib$sp1 == 'GlaucousWingedGull'] <- 'GWGU'
  sib$sp1[sib$sp1 == 'H/C/Ggull'] <- 'HTCG'
  sib$sp1[sib$sp1 == 'GULLS'] <- 'GULL'
  sib$sp1[sib$sp1 == 'H/Cgull'] <- 'HTCG'
  sib$sp1[sib$sp1 == 'Herringgull'] <- 'HEGU'
  sib$sp1[sib$sp1 == 'HERG'] <- 'HEGU'
  sib$sp1[sib$sp1 == 'hummingbird'] <- 'HUMM'
  sib$sp1[sib$sp1 == 'Hummingbird'] <- 'HUMM'
  sib$sp1[sib$sp1 == 'HUMR'] <- 'HUMM'
  sib$sp1[sib$sp1 == 'Jaegersp.'] <- 'PAJA'
  sib$sp1[sib$sp1 == 'Kittiwakesp.'] <- 'BLKW'
  sib$sp1[sib$sp1 == 'Largegull'] <- 'LAGU'
  sib$sp1[sib$sp1 == 'M/Bgull'] <- 'MBGU'
  sib$sp1[grep('marb',tolower(sib$sp1))] <- 'MAMU'
  sib$sp1[sib$sp1 == 'Mewgull'] <- 'MEGU'
  sib$sp1[sib$sp1 == 'Parasiticjaeger'] <- 'PAJA'
  sib$sp1[sib$sp1 == 'Pacificloon'] <- 'PALO'
  sib$sp1[sib$sp1 == 'PacificLoon'] <- 'PALO'
  sib$sp1[sib$sp1 == 'Pelagiccormorant'] <- 'PECO'
  sib$sp1[sib$sp1 == 'Phal spp.'] <- 'RNPH'
  sib$sp1[sib$sp1 == 'Pigeonguillemot'] <- 'PIGU'
  sib$sp1[sib$sp1 == 'PORT'] <- NA
  sib$sp1[sib$sp1 == 'Red-neckedphalarope'] <- 'RNPH'
  sib$sp1[sib$sp1 == 'Red-throatedLoon'] <- 'RTLO'
  sib$sp1[sib$sp1 == 'Rhinocerosauklet'] <- 'RHAU'
  sib$sp1[sib$sp1 == 'Smallgull'] <- 'SMGU'
  sib$sp1[sib$sp1 == 'Storm-petrel'] <- 'FTSP'
  sib$sp1[sib$sp1 == 'STPE'] <- 'FTSP'
  sib$sp1[sib$sp1 == 'Surfbird'] <- 'SURF'
  sib$sp1[sib$sp1 == 'Surfscoter'] <- 'SUSC'
  sib$sp1[sib$sp1 == 'Surfscoter100'] <- 'SUSC'
  sib$sp1[sib$sp1 == 'W/H/C'] <- 'WHCG'
  sib$sp1[sib$sp1 == 'W/H/Cgull'] <- 'WHCG'
  sib$sp1[sib$sp1 == 'Westerngull'] <- 'WEGU'
  sib$sp1[sib$sp1 == "Wilson'sphalarope"] <- 'RNPH'
  
  sib$sp2 %>% unique %>% sort
  sib$sp2[sib$sp2 == "100"] <- NA
  sib$sp2[sib$sp2 == "ABE"] <- 'BAEA'
  sib$sp2[sib$sp2 == "EMK"] <- NA
  sib$sp2[sib$sp2 == "WBW"] <- NA
  sib$sp2[sib$sp2 == "KLB"] <- NA
  sib$sp2[sib$sp2 == "S"] <- NA
  sib$sp2[sib$sp2 == "SW"] <- NA
  sib$sp2[sib$sp2 == "Kitti"] <- 'BLKW'
  sib$sp2[sib$sp2 == "Mew gull"] <- 'MEGU'
  sib$sp2[sib$sp2 == "W/H/C gull"] <- 'WHCG'
  
  sib$sp3 %>% table
  sib$sp3[sib$sp3 == "W/H/C gull"] <- 'WHCG'
  
  sib$effort %>% table
  sib$effort[sib$effort == '0'] <- 'off'
  sib$effort[sib$effort == '1'] <- 'transect'
  sib$effort[sib$effort == '2'] <- 'station'
  sib$effort[sib$effort == '4'] <- 'with whale'
  sib$effort[sib$effort == '5'] <- 'casual'
  
  #===============================================================================
  # Save it
  
  sib %>% head
  
  seabirds <- sib
  save(seabirds, file='data-raw/seabirds.rds')
  use_data(seabirds, overwrite=TRUE)
  
  document()
  data(seabirds)
  seabirds %>% head
  
}

#===============================================================================
#===============================================================================
#===============================================================================

if(FALSE){
  data(seabirds)
  seabirds <-
    seabirds %>%
    rename(datetime = date) %>%
    mutate(date = lubridate::date(datetime)) %>%
    mutate(datetime = lubridate::force_tz(datetime, 'Canada/Pacific')) 
  seabirds %>% as.data.frame %>% head
  usethis::use_data(seabirds, overwrite = TRUE)
}
