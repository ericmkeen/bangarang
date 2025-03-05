# KFS grid

grid <- readr::read_csv('https://raw.githubusercontent.com/ericmkeen/shipstrike/refs/heads/main/data-raw/grid-kfs-1km.csv')
grid %>% head

library(usethis)
use_data(grid)

