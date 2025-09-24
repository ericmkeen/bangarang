#' Calculate flight flux correction factor
#'
#' @param sightings Seabird sightings. 
#' @param flight_groups Table of wind-contingent speed data for seabird groups (see `data(flight_groups)`).  
#'
#' @return Input `data.frame` with new columns.
#' @export
#' @import dplyr
#'
calculate_flight_flux <- function(sightings,
                                  flight_groups){
  
  mamu <- sightings
  mamu <- 
    mamu %>%
    # change wind name
    mutate(wind_ms_raw = wind_kph_raw) %>%
    #rename(wind_ms_raw = wind_kph_raw) %>%
    rowwise %>%
    
    # add flight speed group =====================================================
  left_join(flight_groups, by='spp') %>%
    #as.data.frame %>% tail
    
    # compute true wind speed & direction ========================================
  # convert vessel knots to meters per second
  mutate(ms = as.numeric(knots) * 0.5144444) %>%
    # compute the Y term in the wind conversion equation
    mutate(Ywind = ifelse(wind_hdg_raw >= 0 & wind_hdg_raw <= 180,
                          (pi/2) - ((pi/180)*wind_hdg_raw),
                          ifelse(wind_hdg_raw > 180 & wind_hdg_raw <= 360,
                                 ((pi/180)*wind_hdg_raw) - (3*pi/2),
                                 NA))) %>%
    # use Y term to calculate true wind speed @ anenometer height
    mutate(wind_ms = sqrt((wind_ms_raw*cos(Ywind)) + (((wind_ms_raw*sin(Ywind)) - ms)^2))) %>%
    # calculate wind direction
    mutate(wind_hdg_rad = ifelse(wind_hdg_raw >= 0 & wind_hdg_raw <= 180,
                                 (pi/2) - atan(((wind_ms_raw*sin(Ywind)) - ms) / (wind_ms_raw*cos(Ywind))) + ((pi/180)*hdg),
                                 ifelse(wind_hdg_raw > 180 & wind_hdg_raw <= 360,
                                        (3*pi/2) - atan(((wind_ms_raw*sin(Ywind)) - ms) / (wind_ms_raw*cos(Ywind))) + ((pi/180)*hdg),
                                        NA))) %>%
    # convert wind back to degrees
    mutate(wind_hdg = (wind_hdg_rad * (180/pi)) %% 360) %>%
    
    # handle bird orientation ==================================================
  # absolute hdg
  mutate(dir_deg = as.numeric(case_when(
    dir == 'N' ~ 1,
    dir == 'NE' ~ 45,
    dir == 'E' ~ 90,
    dir == 'SE' ~ 135,
    dir == 'S' ~ 179,
    dir == 'SW' ~ 225,
    dir == 'W' ~ 270,
    dir == 'NW' ~ 315,
    TRUE ~ NA))) %>%
    # hdg with respect to wind direction
    mutate(dir_relative = round(wind_hdg - dir_deg)) %>%
    # classify headwind, tailwind, etc.
    mutate(orientation = case_when(
      abs(dir_relative) <= 45  ~  'tailwind',
      abs(dir_relative) > 45 & abs(dir_relative) <= 135 ~ 'crosswind',
      abs(dir_relative) > 135 & abs(dir_relative) <= 225 ~ 'headwind',
      abs(dir_relative) > 225 & abs(dir_relative) <= 360 ~ 'crosswind',
      TRUE ~ NA
    )) %>%
    
    # scale wind speed to bird height ============================================
  # note that anenometer was mounted 5m above sea level
  # first quantify h0 adjustment based on Bft
  mutate(h0 = case_when(
    round(bft) < 3 ~ (10^-4),
    round(bft) == 3 ~ (10^-3),
    round(bft) == 4 ~ (10^-3),
    round(bft) >= 5 ~ (10^-2),
    TRUE ~ NA
  )) %>%
    # now estimate wind speed at height of the bird
    mutate(wind_ms_bird = wind_ms * ( log(height / h0) / log(5 / h0) ) * abs(cos((pi/180)*abs(dir_relative)))) %>%
    
    # use regression data to estimate flight speed ===============================
  mutate(ms_bird = case_when(
    orientation == 'tailwind' ~ tail_b + (tail_a * wind_ms_bird),
    orientation == 'crosswind' ~ cross_b + (cross_a * wind_ms_bird),
    orientation == 'headwind' ~ head_b + (head_a * wind_ms_bird),
    TRUE ~ NA)) %>%
    
    # replace missing flight speeds with means from the literature ===============
  mutate(ms_bird_filled = ifelse(is.na(ms_bird),
                                 case_when(
                                   orientation == 'tailwind' ~ rnorm(1, mean=tail_mean, sd=tail_sd),
                                   orientation == 'crosswind' ~ rnorm(1, mean=cross_mean, sd=cross_sd),
                                   orientation == 'headwind' ~ rnorm(1, mean=head_mean, sd=head_sd),
                                   is.na(orientation) ~ mean(c(tail_mean, head_mean, cross_mean)),
                                   TRUE ~ NA),
                                 ms_bird)) %>%
    
    # Calculate the flux term k ==================================================
  mutate(Cij = sqrt((ms^2) + (ms_bird_filled^2) - (2 * ms * ms_bird_filled * cos((pi/180)*dir_relative)) )) %>%
    # Spear Vur & Ainley 1992 define k as C / V
    mutate(k = Cij / ms) %>%
    #pull(k) %>% hist()
    # apparent density will be multiplied by the inverse of k
    mutate(correction_factor = 1/k) %>%
    
    # Replace missing flux terms =================================================
  ungroup %>%
    mutate(k_mean = mean(k, na.rm=TRUE)) %>%
    rowwise %>%
    mutate(k_filled = ifelse(is.na(k), k_mean, k)) %>%
    mutate(correction_factor = ifelse(is.na(correction_factor), 1/k_filled, correction_factor)) %>%
    
    # QA/QC ======================================================================
  #pull(wind_kph) %>% sort
  #pull(wind_kph) %>% hist
  #pull(wind_hdg) %>% sort
  #pull(wind_hdg) %>% hist
  #pull(dir_deg)
  #pull(dir_relative)
  #pull(orientation)
  #pull(wind_kph_bird) %>% sort %>% hist
  #pull(ms_bird)
  #pull(ms_bird_filled)
  #pull(Cij)
  #pull(k)
  #pull(correction_factor) %>% hist
  #tail %>%
  as.data.frame
  
  return(mamu)
}