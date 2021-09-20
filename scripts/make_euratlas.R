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

year_sec <- seq(0, 1800, 100)
eura_list <- list()

for (i in seq_along(year_sec)) {
  
  dsn_path <- paste0("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/Euratlas Nussli/data/",
                     year_sec[i],
                     "/utf8")
  
  print(dsn_path)
  
  df <- st_read(dsn = dsn_path,
                layer = "sovereign_dependent_states") %>% 
    st_transform(crs = 4326)
  
  df <- st_intersection(df,
                        iberia_union)

  eura_list[[i]] <- df
  
}

euratlas <- bind_rows(eura_list)
rm(dsn_path, df)

euratlas <- euratlas %>% 
  dplyr::select(-c(FID))

tmap_mode("view")
tm_shape(euratlas[euratlas$year == 500,]) +
  tm_polygons(col = "red", alpha = 0.3) 

st_write(euratlas,
         dsn = "data/euratlas_iberia",
         layer = "euratlas_iberia",
         driver = "ESRI Shapefile",
         delete_layer = T,
         layer_options = "ENCODING=UTF-8")

rm(euratlas, eura_list)

