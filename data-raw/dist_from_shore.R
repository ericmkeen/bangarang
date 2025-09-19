library(bangarang)
library(dplyr)
library(ggplot2)
library(sf)
library(swfscMisc)

# Bring in datasets ============================================================

data(grid)
grid %>% head
gg_kfs() + 
  geom_point(data=grid, mapping=aes(x=x, y=y), size=.2) 


data("nepac_shore_sf")
nepac_shore_sf %>% summary
shore <- st_coordinates(nepac_shore_sf) %>% as.data.frame
shore %>% names
shore %>% head
shore <- 
  shore %>% 
  filter(X < -128.8,
         X > -129.8,
         Y > 52.8,
         Y < 53.7) %>% 
  select(x=X, y=Y)
shore %>% head

# Get minimum distance to shore for each grid point  ===========================

i=1
min_dists <- c()
for(i in 1:nrow(grid)){
  message('Grid point ',i,' of ', nrow(grid))
  gridi <- grid[i,]
  (gridi_coords <- c(gridi$x, gridi$y))
  dists <- 
    apply(shore, 1,
        function(x){
          swfscMisc::distance(lat1=x[2], lon1=x[1],
                              lat2=gridi_coords[2], lon2=gridi_coords[1],
                              units='km')
        })
  (mini <- which.min(dists))
  (min_disti <- dists[mini])
  (min_dists <- c(min_dists, min_disti))
}

grid$km_to_shore <- min_dists


# Color code map by distance to shore ==========================================

gg_kfs() + 
  geom_point(data=grid, 
             mapping=aes(x=x, y=y,
                         color = km_to_shore), 
             pch=15,
             size=3.4) + 
  ggplot2::scale_color_viridis_c() + 
  labs(color = 'KM from shore') + 
  xlab(NULL) + 
  ylab(NULL)

ggsave('../kfs_distance from shore.png', width=7, height=8)

