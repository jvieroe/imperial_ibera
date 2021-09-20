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
# Load polity data
# ---------------------------------------------------------
euratlas <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/euratlas_iberia",
                    layer = "euratlas_iberia")
centennia <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/0 Papers/9 iberia/data/centennia_iberia",
                    layer = "centennia_iberia")

# ---------------------------------------------------------
# Load city data
# ---------------------------------------------------------
bosker <- rio::import("data/cities/bosker_panel_iberia.Rdata")
stasavage <- rio::import("data/cities/stasavage_panel_iberia.Rdata")

bosker <- bosker %>% 
  dplyr::select(c(city_id, year, latitude, longitude)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

stasavage <- stasavage %>% 
  dplyr::select(c(city_id, year, latitude, longitude)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

# ---------------------------------------------------------
# Link Bosker to Centennia
# ---------------------------------------------------------
# ----- Define function
func_centennia_coding <- function(cent, cities, geo_data) {
  
  cent <- cent %>% 
    dplyr::select(-year)
  
  geo_data <- st_join(cities, cent, join = st_nearest_feature) %>% 
    as.data.frame() %>% 
    dplyr::select(c(city_id, year, state))
  
  return(geo_data)
  
}

year_sec <- seq(1000, 1800, 100)

temp <- centennia %>% 
  filter(year %in% year_sec)
cent_list <- split(temp,
                   f = temp$year)
rm(temp)

temp <- bosker %>% 
  filter(year %in% year_sec)
cit_list <- split(temp,
                     f = temp$year)
rm(temp)

coding_list <- purrr::map2(cent_list, cit_list,
                           func_centennia_coding)

coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id) %>% 
  dplyr::mutate(cent = as.character(state)) %>% 
  dplyr::select(c(city_id, year, cent))

save(coding_data,
     file = "data/state_coding/bosker_centennia.Rdata")

rm(coding_data, coding_list, cit_list, cent_list)

# ---------------------------------------------------------
# Link Stasavage to Centennia
# ---------------------------------------------------------
year_sec <- seq(1000, 1800, 10)

temp <- centennia %>% 
  filter(year %in% year_sec)
cent_list <- split(temp,
                   f = temp$year)
rm(temp)

temp <- stasavage %>% 
  filter(year %in% year_sec)
cit_list <- split(temp,
                  f = temp$year)
rm(temp)

coding_list <- purrr::map2(cent_list, cit_list,
                           func_centennia_coding)

coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id) %>% 
  dplyr::mutate(cent = as.character(state)) %>% 
  dplyr::select(c(city_id, year, cent))

save(coding_data,
     file = "data/state_coding/stasavage_centennia.Rdata")

rm(coding_data, coding_list, cit_list, cent_list)

rm(centennia)

# ---------------------------------------------------------
# Link Bosker to Euratlas
# ---------------------------------------------------------
# ----- Define function
func_euratlas_coding <- function(cent, cities, geo_data) {
  
  cent <- cent %>% 
    dplyr::select(-year)
  
  geo_data <- st_join(cities, cent, join = st_nearest_feature) %>% 
    as.data.frame() %>% 
    dplyr::select(c(city_id, year, short_name, sname_o, lname_o, variants_o, sname_h, lname_h,
                    variants_h))
  
  return(geo_data)
  
}

year_sec <- seq(800, 1800, 100)

temp <- euratlas %>% 
  filter(year %in% year_sec)
eura_list <- split(temp,
                   f = temp$year)
rm(temp)

temp <- bosker %>% 
  filter(year %in% year_sec)
cit_list <- split(temp,
                  f = temp$year)
rm(temp)

coding_list <- purrr::map2(eura_list, cit_list,
                           func_euratlas_coding)

coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id)

save(coding_data,
     file = "data/state_coding/bosker_euratlas.Rdata")

rm(coding_data, coding_list, cit_list, eura_list)

# ---------------------------------------------------------
# Link Stasavage to Euratlas
# ---------------------------------------------------------
year_sec <- seq(1000, 1800, 100)

temp <- euratlas %>% 
  filter(year %in% year_sec)
eura_list <- split(temp,
                   f = temp$year)
rm(temp)

temp <- stasavage %>% 
  filter(year %in% year_sec)
cit_list <- split(temp,
                  f = temp$year)
rm(temp)

coding_list <- purrr::map2(eura_list, cit_list,
                           func_euratlas_coding)

coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id)

save(coding_data,
     file = "data/state_coding/stasavage_euratlas.Rdata")

rm(coding_data, coding_list, cit_list)
rm(bosker, eura_list, euratlas, stasavage)


