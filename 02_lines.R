# I will try to plot all line features accessible in AIS CR database
# The data can be queried from an api /api.aiscr.cz/ but I am using
# exported data I prepared earlier for different reasons...

library(tidyverse)
library(sf)


# data --------------------------------------------------------------------

# bg data - CzechRep
rep <- RCzechia::republika()

# amcr data
input <- read_delim(here::here("data/export_2021-11-02_pian.csv"), delim = "#")

sf_data <- st_as_sfc(input$geom_wkt) %>%
  st_as_sf() %>%
  bind_cols(select(input, -starts_with("geom"), -starts_with("centroid"))) %>%
  st_set_crs(5514) %>%
  filter(st_is_valid(sf_data))

# filter line features only
sf_lines <- sf_data %>%
  filter(str_detect(st_geometry_type(sf_data), "LINE"))


# plot --------------------------------------------------------------------
# color by length

sf_lines %>%
  mutate(len = sqrt(as.numeric(st_length(sf_lines)))) %>%
  ggplot() +
  geom_sf(data = rep, fill = "black", color = "gray40") +
  geom_sf(aes(color = len), show.legend = FALSE) +
  scale_color_viridis_c(option = "C") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(color = "gray80")) +
  labs(caption = "Data: Archaeological information system of the Czech Republic
       and RCzechia package #30DayMapChallenge ")

ggsave(here::here("02_lines.png"), width = 12, height = 8)
