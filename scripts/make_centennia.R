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

# ----- Iberia data
iberia_union <- st_read(dsn = "data/iberia union",
                        layer = "iberia_union",
                        crs = 4326)

iberia <- st_read(dsn = "data/iberia",
                  layer = "iberia",
                  crs = 4326)


# ----- Centennia data
centennia <- st_read(dsn = "/Users/jeppeviero/Dropbox/Under Empire/Data and code/5_centennia/centennia_clean",
                     layer = "centennia_clean",
                     crs = 4326) %>% 
  mutate(area_km2 = unclass(st_area(.))/1000000)

centennia <- st_intersection(centennia,
                             iberia_union)  

tmap_mode("view")
tm_shape(centennia[centennia$year == 1400,]) +
  tm_polygons(col = "red", alpha = 0.3) 

st_write(centennia,
         dsn = "data/centennia_iberia",
         layer = "centennia_iberia",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(centennia)

