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

# ---------------------------------------------------------
# Roman Roads (McCormick et al.)
# ---------------------------------------------------------
# ----- Roman roads data
roman_roads <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/14 data/McCormick et al 2013a/dataverse_files",
                       layer = "roman_roads_v2008")
roman_roads <- st_transform(roman_roads, crs = 4326)
roman_roads <- st_intersection(roman_roads,
                               iberia_union)

ggplot() +
  geom_sf(data = iberia, fill = "white", size = 0.5) +
  geom_sf(data = roman_roads, col = "firebrick", size = .2) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

st_write(roman_roads,
         dsn = "data/roman_roads",
         layer = "roman_roads",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(roman_roads)

# ---------------------------------------------------------
# Grids
# https://stackoverflow.com/questions/63246583/behaviour-of-st-make-grid-from-r-sf-package
# https://gis.stackexchange.com/questions/334495/r-spatstat-sp-sf-grid-a-single-polygon-divide-alphanumeric-labels-based-o
# ---------------------------------------------------------
st_is_longlat(iberia_union)

grid_sf <- st_make_grid(iberia_union,
                        n = c(100, 100),
                        square = TRUE) %>%
  st_as_sf()

temp <- grid_sf %>%  
  dplyr::mutate(area_km2 = unclass(st_area(.))/1000000) %>% 
  dplyr::mutate(length = sqrt(area_km2)) %>% 
  dplyr::mutate(gridID = row_number())

mean(temp$area_km2)
hist(temp$area_km2)
hist(temp$length)

rm(temp)

st_crs(iberia_union)
st_crs(grid_sf)

ggplot() +
  geom_sf(data = iberia_union, fill = "white", size = 0.25) +
  geom_sf(data = grid_sf, fill = NA, size = 0.1) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

st_write(grid_sf,
         dsn = "data/iberia_grid",
         layer = "iberia_grid",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(grid_sf)

# ---------------------------------------------------------
# Rivers
# ---------------------------------------------------------
# ----- Ocean data
coast <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/14 data/NE/ne_10m_geography_marine_polys",
                 layer = "ne_10m_geography_marine_polys",
                 crs = 4326)

coast <- st_crop(coast,
                 iberia_union)

# ----- Atlantic
atlantic_coast <- coast %>%
  filter(name %in% c("North Atlantic Ocean",
                     "South Atlantic Ocean",
                     "English Channel",
                     "Irish Sea",
                     "Inner Seas",
                     "Bay of Biscay",
                     "North Sea",
                     "Bristol Channel",
                     "Norwegian Sea",
                     "Vestfjorden",
                     "Greenland Sea",
                     "Denmark Strait",
                     "Skagerrak",
                     "Kattegat",
                     "Øresund")) %>% 
  st_union()

ggplot() +
  geom_sf(data = iberia_union, fill = "grey60", col = "black", size = 0.25) +
  geom_sf(data = atlantic_coast, fill = "grey90", col = "black", size = 0.25) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


# ----- Mediterranean
medi_coast <- coast %>%
  filter(name %in% c("Alboran Sea",
                     "Mediterranean Sea",
                     "Balearic Sea",
                     "Golfe du Lion",
                     "Ligurian Sea",
                     "Tyrrhenian Sea",
                     "Adriatic Sea",
                     "Ionian Sea",
                     "Gulf of Gabès",
                     "Gulf of Sidra",
                     "Sea of Crete",
                     "Aegean Sea")) %>% 
  st_union()

ggplot() +
  geom_sf(data = iberia_union, fill = "grey60", col = "black", size = 0.25) +
  geom_sf(data = medi_coast, fill = "grey90", col = "black", size = 0.25) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

rm(coast)

st_write(atlantic_coast,
         dsn = "data/atlantic_coast",
         layer = "atlantic_coast",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(atlantic_coast)

st_write(medi_coast,
         dsn = "data/medi_coast",
         layer = "medi_coast",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(medi_coast)


# ----- River/lake data
river_lake <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/14 data/NE/ne_10m_rivers_lake_centerlines",
                      layer = "ne_10m_rivers_lake_centerlines",
                      crs = 4326)

# table(river_lake$featurecla)
# river_lake <- river_lake %>% 
#   filter(featurecla == "River")

river_lake <- st_intersection(river_lake,
                              iberia_union)

ggplot() +
  geom_sf(data = iberia_union, fill = "white", col = "black", size = 0.35) +
  geom_sf(data = river_lake, col = "grey50", size = 0.25) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

st_write(river_lake,
         dsn = "data/rivers",
         layer = "rivers",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(river_lake)


# ---------------------------------------------------------
# Ruggesness data (Shaver et al.)
# ---------------------------------------------------------
library(raster)
library(rgdal)

shaver <- raster("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/Shaver et al 2019/ruggedness_longlat/ruggedness_longlat.tif")
crs(shaver)
extent(shaver)
temp_crs <- crs(iberia_union, asText = TRUE)
temp_crs

shaver <- crop(shaver, extent(iberia_union))
shaver <- mask(shaver, iberia_union)
plot(shaver)

writeRaster(shaver, "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/ruggedness/ruggedness.tif",
            overwrite = T, 
            prj = T)

rm(shaver)

# ---------------------------------------------------------
# Agricultural Suitaility (Zabel)
# ---------------------------------------------------------
# ----- Iberia data
st_area(iberia_union)

# ----- Zabel data
library(raster)
library(rgdal)

suit_rainfed <- raster("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/Zabel (2014)/rainfed/overall_suitability_1/overall_suitability_1.bil")
suit_irrigated <- raster("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/Zabel (2014)/irrigated/overall_suitability_1/overall_suitability_1.bil")
suit_both <- raster("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/Zabel (2014)/rainfed_irrigated/overall_cropsuit_i_1961-1990/overall_cropsuit_i_1961-1990.bil")

zabel_list <- list()
zabel_list[[1]] <- suit_rainfed
zabel_list[[2]] <- suit_irrigated
zabel_list[[3]] <- suit_both

# ----- Define function
fun_fixZabel <- function(zabel, zabel_fixed) { 
  
  zabel_fixed <- crop(zabel, extent(iberia_union))
  zabel_fixed <- mask(zabel_fixed, iberia_union)
  crs(zabel_fixed) <- CRS('+init=EPSG:4326')
  
  return(zabel_fixed)
  
}

# ----- Apply function
no_cores <- 3
plan(multiprocess, workers = no_cores)
tic()
zlist <- furrr::future_map(zabel_list,
                           fun_fixZabel,
                           .progress = T,
                           .options = furrr_options(seed = TRUE))
toc()
future::plan(sequential)

suit_rainfed <- zlist[[1]]
suit_irrigated <- zlist[[2]]
suit_both <- zlist[[3]]

library(RColorBrewer)
col_scale <- brewer.pal(9, "Greens")

tm_shape(suit_rainfed) +
  tm_raster(palette = col_scale) +
  tm_legend(position = c("right", "bottom"),
            title = "",
            legend.show = T,
            legend.outside = F,
            legend.title.size = .01,
            legend.title.color = "white")

# ----- Export
writeRaster(suit_rainfed, "data/agri_zabel/asc_files/zabel_rainfed.asc", format = "ascii", overwrite = T, prj = T)
writeRaster(suit_irrigated, "data/agri_zabel/asc_files/zabel_irrigated.asc", format = "ascii", overwrite = T, prj = T)
writeRaster(suit_both, "data/agri_zabel/asc_files/zabel_both.asc", format = "ascii", overwrite = T, prj = T)

writeRaster(suit_rainfed, "data/agri_zabel/tif_files/zabel_rainfed.tif", overwrite = T, prj = T)
writeRaster(suit_irrigated, "data/agri_zabel/tif_files/zabel_irrigated.tif", overwrite = T, prj = T)
writeRaster(suit_both, "data/agri_zabel/tif_files/zabel_both.tif", overwrite = T, prj = T)

rm(zabel_list, suit_both, suit_irrigated, suit_rainfed, zlist)


