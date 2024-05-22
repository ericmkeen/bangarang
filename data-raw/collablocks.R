# Collaborative monitoring blocks

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
                           rep(3, times=5),
                           rep(4, times=6),
                           rep(5, times=7),
                           rep(6, times=5),
                           rep(7, times=6),
                           rep(8, times=8),
                           rep(9, times=5),
                           rep(10, times=5),
                           rep(11, times=8),
                           rep(12, times=5)),
                    name = c(rep('Wright', times=10),
                             rep('Verney-N. Ursula', times=7),
                             rep('Bishop-S. Ursula-McKay', times=5),
                             rep('Whale', times=6),
                             rep('Campania', times=7),
                             rep('Squally (South)', times=5),
                             rep('Squally (North)', times=6),
                             rep('Squally (Central) & Lewis', times=8),
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
                      -129.15, # girbbel nw
                      -129.12, # gribbel sw
                      -129.08, # pr nw
                      -129.2, # gil ne
                      -129.27, # gil nw

                      # stratum 2 - verney + n ursula
                      -129.16, # money pt
                      -129.15, # girbbel nw
                      -129.12, # gribbel sw
                      -128.83, # main ne
                      -128.9, # main lil lower
                      -129.05, # hawks ne
                      -129.16, # douglas e
                      
                      # stratum 3 - s ursula + mckay
                      -129.12, # gribbel sw
                      -129.08, # pr nw
                      -128.9, # pr ne
                      -128.83, # main se
                      -128.83, # main ne
                      
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
                      -129.34, # camp 1
                      -129.25, # fawcett
                      -129.178, # york pt

                      # stratum 6 - Squally S
                      -129.34, # camp 1
                      -129.25, # fawcett
                      -129.33, # mcdonald bay
                      -129.49, # cmp ne
                      -129.47, # cmp mid
                      
                      # stratum 7 - Squally N
                      -129.365, # firs
                      -129.51, # pitt se
                      -129.55, # twartz
                      -129.48, # union
                      -129.342, # farrant
                      -129.33, # cridge
                      
                      # stratum 8 - lng
                      -129.342, # farrant
                      -129.27, # gil nw
                      -129.25, # lewis
                      -129.33, # mcdonald bay
                      -129.51, # cmp ne
                      -129.53, # pitt se
                      -129.365, # firs
                      -129.33, # cridge
                      
                      # stratum 9 - Otter
                      -129.53, # pitt se
                      -129.51, # cmp ne
                      -129.72, # otter group sw
                      -129.8, # otter group nw
                      -129.6, # pitt nw
                      
                      # stratum 10 - Estevan
                      -129.51, # cmp ne
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
                      53.37, # gribbell nw
                      53.325, # gribbell sw
                      53.285, # pr nw
                      53.28, # gil ne
                      53.32, # gil nw
                      
                      # stratum 2 - verney - n ursula
                      53.385, # money pt
                      53.37, # gribbell nw
                      53.325, # gribbell sw
                      53.53, # main ne
                      53.52, # main lil lower
                      53.57, # hawks ne
                      53.44, # douglas e
                      
                      # stratum 3- s ursula mckay
                      53.325, # gribbell sw
                      53.285, # pr nw
                      53.29, # pr ne
                      53.34, # main se
                      53.53, # main ne

                      # stratum 4 - whale
                      53.285, # pr nw
                      53.28, # gil ne
                      53.1, # york pt
                      53.05, # cameron cove
                      53.05, # pr se
                      53.23, # cornwall

                      # stratum 5 - campania
                      53.1, # york pt
                      53.05, # cameron cove
                      52.94, # emily carr
                      52.96, # alexander
                      53.03, # camp 1
                      53.12, # fawcett
                      53.12, # n of york pt

                      # stratum 6 - squally s
                      53.03, # camp 1
                      53.12, # fawcett
                      53.19, # mcdonald bay
                      53.17, # cmp ne
                      53.13, # cmp mid

                      # stratum 7 - squally n
                      53.24, # firs
                      53.225, # pitt se
                      53.3, # twarz
                      53.4, # union
                      53.31, # farrant
                      53.28, # cridge
                      
                      # stratum 8 - lng
                      53.32, # farrant
                      53.32, # gil nw
                      53.25, # lewis
                      53.19, # mcdonald bay
                      53.17, # cmp ne
                      53.225, # pitt se
                      53.24, # firs
                      53.28, # cridge
                      
                      # stratum 9 - otter
                      53.225, # pitt se
                      53.17, # cmp ne
                      53.125, # otter group sw
                      53.225, # otter group nw
                      53.27, # pitt nw

                      # stratum 10 - estevan
                      53.17, # cmp ne
                      53.125, # otter group sw
                      52.9394, # dupont
                      52.96, # alexander
                      53.13, # cmp mid

                      # stratum 11 - caamano
                      52.96, # alexander
                      52.9394, # dupont
                      52.84, # wall
                      52.79, # ulric
                      52.83, # pr sw
                      52.87, # surf s
                      52.91, # surf n
                      52.94, # emily carr

                      # stratum 12 - douglas
                      53.43, # douglas w
                      53.43, # douglas e
                      53.57, # hawks se
                      53.615, # hawks ne
                      53.615 # main nw
                    ))

gg_kfs(lat_range = c(52.8, 53.67),
       lon_range = c(-129.8, -128.85)) + 
  geom_path(data=bblox, aes(x=x, y=y, color=name))

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
prop_coverage <- .8 # this is the goal
truncation_distance <- 3 # km
(area_m2 <- as.numeric(strata_mp$area)*(10^6))
(target_area <- area_m2*prop_coverage) 
(target_length_m <- target_area / (truncation_distance*1000))
target_length_m[2] <- max(c(target_length_m[2], 28000)) # Force Verney to be longer 
target_length_m[3] <- max(c(target_length_m[3], 28000)) # Force Ursula-McKay to be longer
target_length_m[6] <- max(c(target_length_m[2], 28000)) # Force LNG to be longer 
(target_length_km <- target_length_m / 1000) # See what it is in km


# Set angles
design_angles <- c( 90, # wright
                    125, #15, # verney
                    165, # ursula-mckay
                    0, # whale
                    0, # campania
                    160, # squ s
                    170, # lng
                    0, # squ n
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

plot(region, transects, covered.area=TRUE)


