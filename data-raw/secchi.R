# Secchi

s13 <- read.csv('data-raw/secchi13.csv')
s14 <- read.csv('data-raw/secchi14.csv')
s15 <- read.csv('data-raw/secchi15.csv')

library(dplyr)

s13 %>% names
s14 %>% names
s15 %>% names
cbind(names(s13), names(s14), names(s15))
secchi <- rbind(s13 %>% rename(date=d), s14, s15)
secchi

save(secchi, file='data-raw/secchi.rds')
use_data(secchi, overwrite=TRUE)
