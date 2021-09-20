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
# Prepare Stasavage data
# ---------------------------------------------------------
stasavage <- rio::import("/Users/jeppeviero/Dropbox/02 PhD/14 data/Stasavage 2014/stasavage_panel.Rdata")

iberia <- stasavage %>% 
  filter(country == "Spain" | country == "Portugal")

save(iberia,
     file = "data/cities/stasavage_panel_iberia.Rdata")

haven::write_dta(data = iberia,
                 path = "data/cities/stasavage_panel_iberia.dta",
                 version = 14)


# ---------------------------------------------------------
# Prepare Bosker data
# ---------------------------------------------------------
bosker <- rio::import("data/cities/bagdad - london - final restat.dta") %>% 
  arrange(country, city, year)

bosker <- bosker %>% 
  mutate(city_id = indicator)

# ----- Link to geodata
ne <- st_read(dsn = "/Users/jeppeviero/Dropbox/02 PhD/14 data/NE/ne_10m_admin_0_countries/ne_10m_admin_0_countries",
              layer = "ne_10m_admin_0_countries",
              crs = 4326)

continent_vector <- as.character(c("Antarctica"))

ne <- ne %>% 
  filter(CONTINENT %!in% continent_vector) %>% 
  dplyr::select(c(SOVEREIGNT,
                  GEOUNIT,
                  ADM0_A3,
                  ISO_A3)) %>% 
  dplyr::mutate(poly_id = row_number())

bosker_sf <- bosker %>% 
  distinct(., city_id, .keep_all = T) %>% 
  dplyr::select(c(city_id, latitude, longitude)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

tic()
geo_data <- st_join(bosker_sf, ne, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(c(city_id, SOVEREIGNT, ADM0_A3)) %>% 
  dplyr::rename(country = SOVEREIGNT,
                iso3 = ADM0_A3)
toc()

bosker <- bosker %>% 
  rename(country_orig = country) %>% 
  left_join(.,
            geo_data,
            by = "city_id")

iberia <- bosker %>% 
  filter(country == "Spain" | country == "Portugal")

save(iberia,
     file = "data/cities/bosker_panel_iberia.Rdata")

haven::write_dta(data = iberia,
                 path = "data/cities/bosker_panel_iberia.dta",
                 version = 14)

rm(ne, geo_data, bosker, bosker_sf, stasavage, iberia)
