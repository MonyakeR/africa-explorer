# this script draws an African map with a specific country highlighted

library(sf)
library(tidyverse)

africa <- rnaturalearth::ne_countries(
  scale = "small",
  continent = "africa",
  returnclass = "sf"
)

country_name = "Nigeria"
ggplot(data = africa) +
  geom_sf(fill = "gray80", color = "white") +
  geom_sf(data = africa %>% filter(name == country_name), fill = "#5ea76a", color = "black") +
  theme_minimal() +
  labs(title = paste("Map of Africa Highlighting", country_name),
       caption = "Data from Natural Earth")
