# #30DayMapChallenge Day 03: Polygons
# All the buildings of Brno...

library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)
library(showtext)


# data --------------------------------------------------------------------

bc <- st_sfc(st_point(c(16.606836, 49.195061)), crs = 4326)
bbuff <- bc %>% st_buffer(10e3)

# osm data
q_b <- getbb("Brno") %>%
  opq() %>%
  add_osm_feature("building")

b <- osmdata_sf(q_b)

b <- b$osm_polygons %>%
  st_geometry()

bcrop <- b %>%
  st_as_sf() %>%
  st_filter(bbuff)

# plots -------------------------------------------------------------------

bcrop %>%
  ggplot() +
  geom_sf(fill = "gray20", color = "gray20") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

ggsave(here::here("2022/03_polygons.png"),
       dpi = 600, width = 300, height = 300, units = "mm")
