# Map of water mills in the Czech Republic
# Data scraped from https://www.vodnimlyny.cz/
# by Petr Pajdla

library(dplyr)
library(ggplot2)
library(sf)


# functions ---------------------------------------------------------------

#' Scrape Water Mills Database
#'
#' Serves to scrape database of water mills at \url{https://www.vodnimlyny.cz/}.
#'
#' @param url Site URL.
#' @param path Local path to save database.
#'
#' @return A \code{df}.
#' @export
#'
#' @examples
scrape_voda <- function(url, path = NA) {

  pg <- 1

  # init html
  p <- rvest::read_html(paste0(url, pg))

  # sum
  n <- p |>
    rvest::html_element("#snippet--totalCount") |>
    rvest::html_children() |>
    rvest::html_text() |>
    as.numeric()

  if (file.exists(path)) {
    len <- readr::read_rds(path) |>
      nrow()
  } else {
    len <- NA
  }

  if (!file.exists(path) | len != n) {
    # relative part of url
    link <- p |>
      rvest::html_elements(css = ".td-name") |>
      rvest::html_elements(xpath = "a/@href") |>
      rvest::html_text()

    # init res
    res <- p |>
      rvest::html_element(xpath = "//table") |>
      rvest::html_table(header = TRUE) |>
      janitor::clean_names() |>
      dplyr::bind_cols(link = link)

    while (nrow(res) < n) {
      pg <- pg + 1

      pi <- rvest::read_html(paste0(url, pg))

      linki <- pi |>
        rvest::html_elements(css = ".td-name") |>
        rvest::html_elements(xpath = "a/@href") |>
        rvest::html_text()

      res <- pi |>
        rvest::html_element(xpath = "//table") |>
        rvest::html_table(header = TRUE) |>
        janitor::clean_names() |>
        dplyr::bind_cols(link = linki) |>
        dplyr::bind_rows(res)

      Sys.sleep(0.8)
      print(paste0(nrow(res), "/", n))
    }

    res |> readr::write_rds(path)

  } else {

    print("Database up to date.")

  }

}


# data --------------------------------------------------------------------

url_voda <- "https://www.vodnimlyny.cz/mlyny/objekty/?paginator-page="

scrape_voda(url_voda, path = here::here("2022/data/vodni.rds"))

rep <- RCzechia::republika() %>%
  st_simplify(dTolerance = 4e2)

# dataprep ----------------------------------------------------------------

mlyny <- readr::read_rds(here::here("2022/data/vodni.rds")) %>%
  select(-obrazek) %>%
  mutate(
    nazev = stringr::str_remove(nazev, "»"),
    nazev = stringr::str_trim(nazev),
    gps_souradnice = stringr::str_replace(gps_souradnice, "\\n\\t\\t\\t\\t\\t", "; "),
    link = paste0("https://www.vodnimlyny.cz", link)
  ) %>%
  tidyr::separate(gps_souradnice, into = c("lat", "lon"), sep = ";") %>%
  mutate(
    across(c(lat, lon), stringr::str_trim),
    lat = parzer::parse_lat(lat),
    lon = parzer::parse_lon(lon)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# plot --------------------------------------------------------------------

# bblue <- "#0096FF"
#
# mlyny %>%
#   ggplot() +
#   geom_sf(data = rep, fill = NA, color = "gray60") +
#   geom_sf(color = bblue, alpha = 0.2, shape = 20) +
#   theme_void() +
#   theme(plot.background = element_rect(fill = "gray30", color = "gray30"),
#         text = element_text(color = "gray60")) +
#   labs(title = "Water Mills\nin the Czech Republic",
#        subtitle = "\u002330DayMapChallenge",
#        caption = "Data scraped from https://www.vodnimlyny.cz/.")

mlyny %>%
  ggplot() +
  geom_sf(color = "black", alpha = 0.2, shape = 16, size = 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    text = element_text(color = "gray60", family = "baloo")
  ) +
  labs(
    title = "Watermills in the Czech Republic",
    subtitle = "\u002330DayMapChallenge",
    caption = "Data scraped from https://www.vodnimlyny.cz/."
  )

ggsave(here::here("2022/01_points.svg"),
       width = 300, height = 200, units = "mm")

# labels edited manually in Inkscape
