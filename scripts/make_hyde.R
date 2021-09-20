pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tictoc,
               future,
               furrr,
               haven,
               stargazer,
               lfe,
               stringr,
               raster,
               zoo)

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia")
getwd()

# ---------------------------------------------------------
# Interpolate rural population data (HYDE)
# ---------------------------------------------------------
rm(list=ls())
#memory.limit(size=16000)

# ----- Iberia data
iberia_union <- st_read(dsn = "data/iberia union",
                        layer = "iberia_union",
                        crs = 4326)

year_sec <- seq(800, 1700, 100)

raster_list <- list()

for (i in seq_along(year_sec)) {
  
  raster_path <- paste("/Users/jeppeviero/Dropbox/Under Empire/Data and code/hyde 3.2/hyde_pop_data/",
                       year_sec[[i]],
                       "AD_pop/rurc_",
                       year_sec[[i]],
                       "AD.asc",
                       sep = "")
  print(raster_path)
  
  r <- raster(raster_path)
  proj4string(r) <- CRS("+init=epsg:4326")
  r <- crop(r, extent(iberia_union))
  r <- mask(r, iberia_union)
  
  df <- r@data@values %>% 
    as.data.frame() %>% 
    mutate(year = year_sec[[i]],
           row_id = row_number())
  
  colnames(df) <- c("hyde_pop", "year", "row_id")
  
  raster_list[[i]] <- df
  
}

rm(df, r, iberia_union)

df <- bind_rows(raster_list)
nrow(df)/1000000
nrow(df)/1000

rm(raster_list)

df_expanded <- df %>%
  dplyr::select(-hyde_pop) %>%
  group_by(row_id) %>%
  summarise_all(~ list(seq(800, 1700, 10))) %>%
  unnest() %>% 
  as.data.frame()

nrow(df_expanded)/1000000
nrow(df_expanded)/1000

df_expanded <- df_expanded %>% 
  left_join(.,
            df,
            by = c("row_id", "year")) %>% 
  arrange(row_id, year) %>% 
  as.data.frame()

nrow(df_expanded)/1000000
nrow(df_expanded)/1000

rm(df)

#memory.limit(size=16000)

df_expanded <- df_expanded %>%
  mutate(ln_hyde_pop = log(hyde_pop + 1)) %>% 
  arrange(row_id, year) %>% 
  group_by(row_id) %>% 
  mutate(ln_hyde_pop_int = zoo::na.approx(ln_hyde_pop, year, na.rm = F)) %>% 
  mutate(hyde_pop_int = exp(ln_hyde_pop_int) - 1) %>% 
  ungroup()

df_expanded <- df_expanded %>% 
  dplyr::select(-c(row_id,
                   hyde_pop,
                   ln_hyde_pop,
                   ln_hyde_pop_int)) %>% 
  rename(hyde_rural_int = hyde_pop_int)

# ----- Iberia data
iberia_union <- st_read(dsn = "data/iberia union",
                        layer = "iberia_union",
                        crs = 4326)


raster_path <- paste("/Users/jeppeviero/Dropbox/Under Empire/Data and code/hyde 3.2/hyde_pop_data/1000AD_pop/popc_1000AD.asc")
base_r <- raster(raster_path)
proj4string(base_r) <- CRS("+init=epsg:4326")
base_r <- crop(base_r, extent(iberia_union))
base_r <- mask(base_r, iberia_union)

rm(iberia_union)

year_sec <- seq(800, 1700, 10)

#memory.limit(size=16000)

for (i in seq_along(year_sec)) {
  
  r <- base_r
  
  df <- df_expanded %>% filter(year == year_sec[[i]]) %>% 
    dplyr::select(-year)
  
  values(r) <- df$hyde_rural_int
  
  raster_path_uniq <- paste("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/hyde_iberia/",
                            "AD",
                            year_sec[[i]],
                            ".asc",
                            sep = "")
  print(raster_path_uniq)
  
  writeRaster(r, raster_path_uniq, format = "ascii", overwrite = T)
  
}


rm(df_expanded, base_r, df, r)

# ----- Check exports
raster_path <- paste("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/hyde_iberia/AD1630.asc")
base_r <- raster(raster_path)
proj4string(base_r) <- CRS("+init=epsg:4326")

plot(base_r)


# ---------------------------------------------------------
# Crop non-interpolated rural population data (HYDE)
# ---------------------------------------------------------
rm(list=ls())

# ----- Iberia data
iberia_union <- st_read(dsn = "data/iberia union",
                        layer = "iberia_union",
                        crs = 4326)

year_sec <- seq(1710, 1800, 10)

for (i in seq_along(year_sec)) {
  
  raster_path <- paste("/Users/jeppeviero/Dropbox/Under Empire/Data and code/hyde 3.2/hyde_pop_data/",
                       year_sec[[i]],
                       "AD_pop/rurc_",
                       year_sec[[i]],
                       "AD.asc",
                       sep = "")
  print(raster_path)
  
  r <- raster(raster_path)
  proj4string(r) <- CRS("+init=epsg:4326")
  r <- crop(r, extent(iberia_union))
  r <- mask(r, iberia_union)
  
  raster_path_uniq <- paste("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/hyde_iberia/",
                            "AD",
                            year_sec[[i]],
                            ".asc",
                            sep = "")
  print(raster_path_uniq)
  
  writeRaster(r, raster_path_uniq, format = "ascii", overwrite = T)

}

