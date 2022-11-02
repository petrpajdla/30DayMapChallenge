# I will try to download OSM features that have anything to do with archaeology
# in 'Central Europe', this might be quite a large data set, never really worked with OSM data before...

library(tidyverse)
library(osmdata)
library(sf)


theme_set(theme_void())

# query osm api -----------------------------------------------------------
ce <- c(10, 46, 20, 53)

q_ce <- opq(bbox = ce, timeout = 60*6) %>%
  add_osm_feature("historic", "archaeological_site")

r_ce <- osmdata_sf(q_ce)


# explore -----------------------------------------------------------------

r_ce

# ggplot() +
#   geom_sf(data = r_ce$osm_points, alpha = 0.1)
#
# ggplot() +
#   geom_sf(data = r_ce$osm_lines)
#
# ggplot() +
#   geom_sf(data = r_ce$osm_polygons)
#
# ggplot() +
#   geom_sf(data = r_ce$osm_multilines)
#
# ggplot() +
#   geom_sf(data = r_ce$osm_multipolygons)


# simplify polygons/lines to centroids ------------------------------------

multi_to_centroid <- function(x) {
  x_nm <- x %>% names()
  x_cols <- x_nm[stringr::str_detect(x_nm, "lines|poly")]

  x[x_cols] %>% purrr::map(st_centroid)
}

ce_osm_arch <- r_ce %>% multi_to_centroid() %>%
  bind_rows() %>%
  bind_rows(r_ce[["osm_points"]]) %>%
  # some points out of original bbox
  st_crop(setNames(ce, c("xmin", "ymin", "xmax", "ymax"))) %>%
  st_transform(32633)


# create hexagons ---------------------------------------------------------

hex <- st_make_grid(ce_osm_arch, square = FALSE, cellsize = 2e4)

hex <- hex %>%
  st_as_sf() %>%
  filter(n > 0) %>%
  mutate(n = lengths(st_intersects(hex, ce_osm_arch)),
         n_log = log10(n),
         n_bins = cut_interval(n_log, 9))


# plot --------------------------------------------------------------------



# bg map?
ce_states <- rnaturalearth::ne_countries(scale = "medium") %>%
  st_as_sf() %>%
  filter(sov_a3 %in% c("AUT", "CZE", "DEU", "POL", "HUN", "SVK", "SRB",
                       "ITA", "CHE", "SVN", "HRV")) %>%
  st_transform(32633)

# ggplot() +
#   geom_sf(data = hex, aes(fill = n),
#           alpha = 0.8, color = NA) +
#   scale_fill_viridis_c(option = "E") +
#   geom_sf(data = ce_states,
#           color = "black", fill = NA, alpha = 0.2) +
#   coord_sf(xlim = c(9.5, 20.5),  ylim = c(45.5, 53.5), crs = 4326)

ggplot() +
  geom_sf(data = hex, aes(fill = n_bins),
          color = NA, show.legend = FALSE) +
  geom_sf(data = ce_states,
          color = "black", fill = NA, alpha = 0.2) +
  scale_fill_brewer(palette = "GnBu") +
  coord_sf(xlim = c(9.8, 20.2),  ylim = c(46, 53.1), crs = 4326) +
  labs(caption = "Data: © OpenStreetMap contributors and Natural Earth") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(here::here("05_osmdata.png"), width = 12, height = 14)
