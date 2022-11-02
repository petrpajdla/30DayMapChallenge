# #30DayMapChallenge Day 02: Lines
# Compare the infrastructure for cycling with real traffic of cyclists in Brno "city"
# recorded in the "Cycling to work" ("Do práce na kole") project
# https://www.dopracenakole.cz/

library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)


# data --------------------------------------------------------------------

# osm cycling lanes etc.
q_cw <- getbb("Brno") %>%
  opq() %>%
  add_osm_feature("highway", "cycleway")

q_cl <- getbb("Brno") %>%
  opq() %>%
  add_osm_feature("cycleway", "lane")

q_ct <- getbb("Brno") %>%
  opq() %>%
  add_osm_feature("cycleway", "track")

q_sl <- getbb("Brno") %>%
  opq() %>%
  add_osm_feature("cycleway", "shared_lane")

cw <- osmdata_sf(q_cw)
cl <- osmdata_sf(q_cl)
# ct <- osmdata_sf(q_ct) there are no tracks
sl <- osmdata_sf(q_sl)

# data from do prace na kole project
# downloaded from data.brno.cz
# https://data.brno.cz/datasets/mestobrno::dopravn%C3%AD-intenzity-cyklist%C5%AF-do-pr%C3%A1ce-na-kole-traffic-intensity-of-cyclists-of-bike-to-work-event/about
dpnk <- st_read(here::here("2022/data/dpnk_from_databrno.geojson"))


# dataprep ----------------------------------------------------------------

dpnk_sum <- dpnk %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(sum = sum(c_across(c(starts_with("data"), "dpnk_22")))) %>%
  select(OBJECTID, sum)

dpnk_summed <- dpnk %>%
  bind_cols(sum = dpnk_sum$sum) %>%
  filter(sum > 0) %>%
  mutate(log = log10(sum),
         s = case_when(
           log >= 3 ~ 0.8,
           log >= 2 & log < 3 ~ 0.6,
           log >= 1 & log < 2 ~ 0.4,
           log < 1 ~ 0.2,
         ))

# dpnk_summed %>% pull(sum) %>% hist()

# plots -------------------------------------------------------------------

# library(showtext)
# font_add_google("Cabin Sketch", "cabin")
showtext_auto()

ggplot() +
  geom_sf(data = sl$osm_lines, color = "green") +
  geom_sf(data = cl$osm_lines) +
  geom_sf(data = cw$osm_lines, color = "blue")

# ggplot() +
#   geom_sf(data = dpnk_summed, aes(color = sum, size = s)) +
#   scale_color_viridis_c(option = "viridis", values = c(0, 0.02, 0.08, 0.1, 0.4, 1)) +
#   scale_size_identity() +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "gray20"),
#         legend.direction = "horizontal",
#         legend.position = c(0.8, 0.1),
#         legend.text = element_text(color = "gray80"),
#         legend.title = element_text(color = "gray80")) +
#   labs(color = "Cyclists\n(thousands)")

ggplot() +
  geom_sf(data = dpnk_summed, aes(color = log, size = s)) +
  scale_color_viridis_c(option = "viridis", labels = c(0, 3, 6, 9, "12k")) +
  scale_size_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray20"),
        legend.direction = "horizontal",
        legend.position = c(0.8, 0.1),
        legend.text = element_text(family = "sans", color = "gray80", size = 10),
        plot.caption = element_text(family = "sans", color = "gray80", size = 10),
        text = element_text(family = "cabin", color = "gray80", size = 16)) +
  labs(color = "",
       title = "Number of cycling trips to/from work recorded in\nBrno in the 'Bike to Work' challenge",
       subtitle = "May 2018 to 2022",
       caption = "Data from https://data.brno.cz/.")

ggsave(here::here("2022/02_lines.svg"), scale = 2)


