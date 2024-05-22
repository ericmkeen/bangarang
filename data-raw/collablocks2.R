
library(dplyr)
library(ggplot2)
library(sf)
library(bangarang)
library(sfheaders)
library(bangarang)
library(dssd)

# Prep bounding boxes ==========================================================

bblox <- data.frame(stratum = c(rep(1, times=10),
                                rep(2, times=7),
                                rep(3, times=8),
                                rep(4, times=4),
                                rep(5, times=6),
                                rep(6, times=6),
                                rep(7, times=7),
                                rep(8, times=11),
                                rep(9, times=6),
                                rep(10, times=5),
                                rep(11, times=5),
                                rep(11, times=8),
                                rep(11, times=5)),
                    name = c(rep('Wright', times=10),
                             rep('Verney', times=7),
                             rep('Ursula', times=8),
                             rep('Bishop Bay', times=4),
                             rep('McKay', times=6),
                             rep('Whale', times=6),
                             rep('Campania', times=7),
                             rep('Squally', times=11),
                             rep('Lewis', times=6),
                             rep('Otter - Nepean', times=5),
                             rep('Estevan', times=5),
                             rep('Caamano', times=8),
                             rep('Douglas', times=5)),
                    x = c(
                      # stratum 1 - Wright
                      -129.342, # farrant
                      -129.36, # grenville
                      -129.26, # douglas w
                      -129.16, # douglas e
                      -129.16, # money pt
                      -129.14, # girbbel nw
                      -129.12, # gribbel sw
                      -129.08, # pr nw
                      -129.2, # gil ne
                      -129.27, # gil nw
                      
                      # stratum 2 - verney 
                      -129.16, # money pt
                      -129.14, # girbbel nw
                      -129.05, # gribbel jenkinson
                      -129.03, # gribbel ne
                      -128.97, # main ne
                      -129.05, # hawks ne
                      -129.16, # douglas e
                      
                      # stratum 3 - ursula
                      -129.03, # gribbel ne
                      -128.97, # main ne
                      -128.925, # bishop north
                      -128.885, # bishop south
                      -128.88, # other inlet
                      -128.88, # main se
                      -128.96, # gribbell se
                      -128.96, # gribbel ctr e
                      
                      # stratum 4 - bishop bay
                      -128.925, # bishop north
                      -128.885, # bishop south
                      -128.82, # bishop se
                      -128.90, # bishop ne
                      
                      # stratum 5 - mckay
                      -129.12, # gribbel sw
                      -129.08, # pr nw
                      -128.9, # pr ne
                      -128.86, # main se
                      -128.96, # gribbell se
                      -129.05, # gribbel s ctr
                      
                      # stratum 4 - Whale
                      -129.08, # pr nw
                      -129.2, # gil ne
                      -129.175, # york pt
                      -129.12, # cameron cove
                      -129.05, # pr se
                      -129.05, # cornwall
                      
                      # stratum 5 - Campania
                      -129.175, # york pt
                      -129.12, # cameron cove
                      -129.18, # emily carr
                      -129.33, # alexander
                      -129.37, # camp 1
                      -129.25, # fawcett
                      -129.178, # york pt
                      
                      # stratum 6 - Squally 
                      -129.37, # camp 1
                      -129.25, # fawcett
                      -129.33, # mcdonald bay
                      -129.365, # firs
                      -129.34, # cridge
                      -129.35, # farrant
                      -129.48, # union
                      -129.55, # twartz
                      -129.51, # pitt se
                      -129.49, # cmp ne
                      -129.47, # cmp mid
                      
                      # stratum 8 - Lewis
                      -129.35, # farrant
                      -129.27, # gil nw
                      -129.25, # lewis
                      -129.33, # mcdonald bay
                      -129.365, # firs
                      -129.34, # cridge
                      
                      # stratum 9 - Otter
                      -129.51, # pitt se
                      -129.49, # cmp ne
                      -129.72, # otter group sw
                      -129.8, # otter group nw
                      -129.6, # pitt nw
                      
                      # stratum 10 - Estevan
                      -129.49, # cmp ne
                      -129.72, # otter group sw
                      -129.45, # sw estevan
                      -129.33, # alexander
                      -129.47, # cmp mid
                      
                      # stratum 11 - Caamano
                      -129.33, # alexander
                      -129.45, # sw estevan
                      -129.35, # wall
                      -129.22, # ulric
                      -129.08, # pr sw
                      -129.11, # surf s
                      -129.12, # surf n
                      -129.18, # emily carr
                      
                      # stratum 12 - douglas
                      -129.26, # douglas w
                      -129.16, # douglas e
                      -129.05, # hawks se
                      -129.15, # hawks ne
                      -129.27 # main nw
                    ),
                    y = c(
                      # stratum 1 - wright
                      53.32, # farrant
                      53.374, # grenville
                      53.43, # douglas w
                      53.43, # douglas e
                      53.385, # money pt
                      53.365, # gribbell nw
                      53.325, # gribbell sw
                      53.285, # pr nw
                      53.28, # gil ne
                      53.32, # gil nw
                      
                      # stratum 2 - verney 
                      53.385, # money pt
                      53.365, # gribbell nw
                      53.43, # gribbel jenkinson
                      53.52, # gribbel ne
                      53.54, # main ne
                      53.57, # hawks ne
                      53.44, # douglas e
                      
                      # stratum 3 - ursula
                      53.52, # gribbel ne
                      53.54, # main ne
                      53.46, # bishop north
                      53.42, # bishop south
                      53.37, # other inlet
                      53.31, # main se
                      53.34, # gribbel se
                      53.42, # gribbel ctr e
                      
                      # stratum 4 - bishop bay
                      53.46, # bishop north
                      53.42, # bishop south
                      53.47, # bishop se
                      53.51, # bishop ne
                      
                      # stratum 5 - mckay
                      53.325, # gribbell sw
                      53.285, # pr nw
                      53.29, # pr ne
                      53.31, # main se
                      53.34, # gribbel se
                      53.35 , # gribbel s ctr
                      
                      # stratum 6 - whale
                      53.285, # pr nw
                      53.28, # gil ne
                      53.1, # york pt
                      53.05, # cameron cove
                      53.05, # pr se
                      53.23, # cornwall
                      
                      # stratum 7 - campania
                      53.1, # york pt
                      53.05, # cameron cove
                      52.94, # emily carr
                      52.96, # alexander
                      53.05, # camp 1
                      53.13, # fawcett
                      53.12, # n of york pt
                      
                      # stratum 8 - squally 
                      53.05, # camp 1
                      53.13, # fawcett
                      53.19, # mcdonald bay
                      53.24, # firs
                      53.28, # cridge
                      53.31, # farrant
                      53.4, # union
                      53.3, # twarz
                      53.225, # pitt se
                      53.15, # cmp ne
                      53.13, # cmp mid
                      
                      # stratum 9 - lewis
                      53.32, # farrant
                      53.32, # gil nw
                      53.27, # lewis
                      53.19, # mcdonald bay
                      53.24, # firs
                      53.28, # cridge
                      
                      # stratum 10 - otter
                      53.225, # pitt se
                      53.15, # cmp ne
                      53.10, # otter group sw
                      53.225, # otter group nw
                      53.27, # pitt nw
                      
                      # stratum 11 - estevan
                      53.15, # cmp ne
                      53.10, # otter group sw
                      52.9394, # dupont
                      52.96, # alexander
                      53.13, # cmp mid
                      
                      # stratum 12 - caamano
                      52.96, # alexander
                      52.9394, # dupont
                      52.84, # wall
                      52.79, # ulric
                      52.83, # pr sw
                      52.87, # surf s
                      52.91, # surf n
                      52.94, # emily carr
                      
                      # stratum 13 - douglas
                      53.43, # douglas w
                      53.43, # douglas e
                      53.57, # hawks se
                      53.615, # hawks ne
                      53.615 # main nw
                    ))

print(
  gg_kfs(lat_range = c(52.8, 53.67),
         lon_range = c(-129.8, -128.85)) + 
    geom_path(data=bblox, aes(x=x, y=y, color=name))
)

bblox %>% head(12)

# convert to multipolgyon ======================================================

blox_sf <- sfheaders::sf_polygon(obj = bblox, 
                                 x = "x", y = "y", 
                                 polygon_id = "name")
blox_sf
st_crs(blox_sf) <- 4326
sf_use_s2(FALSE)
blox_sf2 <- st_make_valid(blox_sf)
blox_sf2
#ggplot(blox_sf2) + geom_sf()

# Bring in shoreline ===========================================================

data(nepac_shore_sf) # from bangarang package
nepac_shore_sf
sf_use_s2(FALSE)
shore2 <- st_make_valid(nepac_shore_sf)
st_crs(shore2)

# Get polygons of water in each stratum ========================================

strata <- st_difference(blox_sf2,
                        shore2)

#strata %>% plot

gg_kfs() + 
  geom_sf(data=strata, aes(fill=name), alpha=.7) + 
  coord_sf(ylim = c(52.8, 53.67),
           xlim = c(-129.8, -128.85))

# Get area
strata$area <- round(st_area(strata) / (10^6), 1)
strata

# Convert coordinates to KM
st_crs(strata)
strata_utm <- st_transform(strata, crs=7131)
st_crs(strata_utm)
strata_utm

# Make sure it is a multipolgyon
strata_mp <- st_cast(strata_utm, 'MULTIPOLYGON')
st_geometry(strata_mp)

################################################################################
# Now work with DSSD functions
# (just following instructions in link at top of R script)

# Setup region / survey area ===================================================

region <-
  make.region(region.name = "gg",
              strata.name = strata_mp$name %>% unique,
              #units = 'm',
              units = 'km',
              shape = strata_mp)

region %>% plot

# Make a coverage grid =========================================================

cover <- make.coverage(region,
                       n.grid.points = 1000)
cover

# Determine realistic line length in a single day within a stratum
speed <- 6 # knots
hours_per_day <- 6 # per day
(km <- hours_per_day * speed * 1.82) # max reasonable in day

# Set line-length goals based on equal coverage rates
target_length <- c( 50, # wright
                    25, # verney
                    28, # ursula
                    8, # bishop
                    18, # mckay
                    40, # whale
                    45, # campania
                    65, # squ 
                    15, # lewis
                    55, # otter
                    45, # estevan
                    70, # caamano
                    30) # douglas

# Lewis + Wright - 65
# Squally - 65
# Otter - 55
# Gribbel - 60 + 18 = 72

target_length_m <- target_length * 1000

# prop_coverage <- .8 # this is the goal
# truncation_distance <- 3 # km
# (area_m2 <- as.numeric(strata_mp$area)*(10^6))
# (target_area <- area_m2*prop_coverage) 
# (target_length_m <- target_area / (truncation_distance*1000))
# target_length_m[2] <- max(c(target_length_m[2], 28000)) # Force Verney to be longer 
# target_length_m[3] <- max(c(target_length_m[3], 28000)) # Force Ursula-McKay to be longer
# target_length_m[6] <- max(c(target_length_m[2], 28000)) # Force LNG to be longer 
# (target_length_km <- target_length_m / 1000) # See what it is in km


# Set angles
design_angles <- c( 120, # wright
                    35, # verney
                    175, # ursula
                    45, # bishop
                    100, #mckay
                    0, # whale
                    0, # campania
                    0, # squ 
                    32.5, # lewis
                    90, # otter
                    150, # estevan
                    125, # caamano
                    10) # douglas

# Stipulate choices for survey design  =========================================

design <- make.design(region = region,
                      transect.type = 'line',
                      
                      # The two designs below seem most promising
                      # (uncomment one or the other to try)
                      #design = "systematic",
                      design = 'eszigzag',
                      
                      # The next input defines how many KM we want to do in each channel
                      # I just made up numbers for this test
                      line.length = target_length_m, 
                      edge.protocol = "minus",
                      design.angle = design_angles,
                      truncation = 3,
                      coverage.grid = cover)

design

# Generate transects ===========================================================
# (this function randomly draws transects based on the details in `design`)

transects <- generate.transects(design)
transects

# Plot transects ===============================================================

p <- plot(region, transects, covered.area=TRUE)
p

# Save dataset  ================================================================

study_design <- list(design = design, 
                     strata = strata_mp, 
                     transects_ex = p,
                     region = region, 
                     coverage = cover)

save(study_design, file='data-raw/study_design.rds')
usethis::use_data(study_design, overwrite=TRUE)

devtools::document()

