# flight groups

url <- 'https://docs.google.com/spreadsheets/d/1ffxlGkh1QWAPvG2EpHvSFFxnutriWBWc8iZHVZsuWEM/edit?usp=sharing'
flight_groups <- gsheet::gsheet2tbl(url)
flight_groups

library(usethis)
use_data(flight_groups)
