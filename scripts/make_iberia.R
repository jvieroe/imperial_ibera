pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tmap,
               raster,
               fasterize,
               tictoc,
               future,
               furrr,
               haven,
               stargazer,
               fixest,
               zoo,
               knitr,
               magick,
               stringi,
               broom,
               hrbrthemes,
               readr)

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia")
getwd()

# ---------------------------------------------------------
# Make Iberia maps
# ---------------------------------------------------------
# ----- Load data
iberia <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/14 data/NE/ne_10m_admin_0_countries/ne_10m_admin_0_countries",
              layer = "ne_10m_admin_0_countries",
              crs = 4326)

iberia <- iberia %>% 
  dplyr::select(c(SOVEREIGNT, ADM0_A3))

iberia <- iberia %>% 
  filter(SOVEREIGNT == "Spain" | SOVEREIGNT == "Portugal")

# ----- Crop map
iberia <- st_crop(iberia,
                  st_bbox(c(xmin = -11, xmax = 4.5, ymin = 36, ymax = 45),
                          crs = st_crs(iberia)))

# ----- Unionize map
iberia_union <- st_union(iberia)

tmap_mode("view")
tm_shape(iberia) +
  tm_polygons(col = "red", alpha = 0.3) 

tmap_mode("view")
tm_shape(iberia_union) +
  tm_polygons(col = "red", alpha = 0.3) 

getwd()

st_write(iberia,
         dsn = "data/iberia",
         layer = "iberia",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

st_write(iberia_union,
         dsn = "data/iberia union",
         layer = "iberia_union",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(iberia, iberia_union)



