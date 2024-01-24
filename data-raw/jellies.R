# Jellies

#===============================================================================
# 2015

df <- read_csv("/Users/ekezell/Downloads/bangarang data/B15package/2015_drifters.csv")
head(df)

df %>% names

df %>% 
  select(date, 
         lat, lon = long, effort = eff, 
         circuit = circ, bft, perc_cloud = per.cld, 
         knots = spd, 
         hdg, 
         type:X24)
