# this script draws an African map with a specific country highlighted

library(sf)
library(tidyverse)

africa <- rnaturalearth::ne_countries(
  scale = "small",
  type = "sovereignty",
  continent = "africa",
  returnclass = "sf"
)

iso3_codes <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", 
                 "COM", "COG", "COD", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB", 
                 "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", 
                 "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", 
                 "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO", 
                 "UGA", "ZMB", "ZWE")


generate_maps <- function(iso_code) {
  
  plt <- ggplot(data = africa) +
    geom_sf(fill = "gray80", color = "white") +
    geom_sf(data = africa %>% filter(iso_a3 == iso_code), fill = "#5ea76a", color = "black") +
    theme_minimal() +
    coord_sf() +
    theme_void()
  
  ggsave(filename = here::here(paste0("www/images/maps/", iso_code, ".png")), plot = plt, width = 11, height = 8)
  
}

walk(iso3_codes, generate_maps)

