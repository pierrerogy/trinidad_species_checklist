# Make map of sites

# Libraries
library(tidyverse)
library(here)
library(ggmap)
library(ggrepel)

# Load tibble with site coordinates
sites <- 
  readr::read_csv(here::here("data_raw",
                             "sites.csv")) %>% 
  ## Add a minus where there is none and give better names
  dplyr::mutate(lon = as.numeric(ifelse(stringr::str_detect(pattern = "-",
                                                            string = W),
                                        W,
                                        paste0("-", W))),
                lat = N) %>%
  dplyr::select(-N, -W) %>% 
  ## Rename that location with several fallen bromeliads
  dplyr::mutate(name = ifelse(name == "F3",
                              "F3-F5",
                              name)) %>% 
  ## Make new column with nice, capitalised names for plotting
  dplyr::mutate(name_clean = stringr::str_to_title(stringr::str_replace(pattern = "_",
                                                                        replacement = " ",
                                                                        string = name))) %>% 
  ## Add category column for sites on the road
  dplyr::mutate(type = ifelse(stringr::str_detect(pattern = "F",
                                                  string = name),
                              "road",
                              "site")) %>% 
  ## Rename site column
  dplyr::rename(site = name)

# Note that you will have to register, to run ggmap, but can do so on free trial
## Secret API code
api_secret <- 
## Register your machine 
ggmap::register_google(key = api_secret)




# Get country map
satellite <- 
  get_map(c(left = -62.5, 
            right = -60, 
            bottom = 10, 
            top = 11.5), 
          maptype = "satellite", 
          source = "google", 
          api_key = api_secret)
## Plot map
map_country <- 
  ggmap::ggmap(satellite) +
  ## Add points
  geom_point(data = sites,
             aes(x = lon, 
                 y = lat),
             size = 3,
             colour = "ivory")  +
  ## Custom axes
  scale_x_continuous(breaks = seq(from = -62,
                                  to = -60.5,
                                  by = 0.5),
                     limits = c(-62, -60.5),
                     name = "") +
  scale_y_continuous(breaks = seq(from = 10,
                                  to = 11.5,
                                  by = 0.5),
                     name = "",
                     limits = c(10, 11.5)) +
  ## Draw a little rectangle where the zoom will be
  geom_rect(aes(xmin = -61.35, 
                xmax = -61.22, 
                ymin = 10.65, 
                ymax = 10.8), 
            fill = NA, 
            col = "ivory")


# Get close-up map
satellite <- 
  get_map(c(left = -61.35, 
            right = -61.22, 
            bottom = 10.65, 
            top = 10.8), 
          maptype = "satellite", 
          source = "google", 
          api_key = api_secret)
## Plot map
map_sites <- 
  ggmap::ggmap(satellite) +
  ## Add points
  geom_label_repel(data = sites,
                   aes(x = lon, 
                       y = lat,
                       label = name_clean),
                   size = 3)  +
  ylab("") +
  xlab("")


# Save maps
ggplot2::ggsave(map_country,
                file = here::here("plots",
                           "map_full.pdf"),
                height = 6,
                width = 12)

ggplot2::ggsave(map_sites,
                file = here::here("plots",
                           "map_sites.pdf"),
                height = 6,
                width = 7)

# Save cleaned site data
readr::write_csv(sites,
                 here::here("data_clean",
                            "sites.csv"))
