## Set working directory

setwd("yourworkingdirectory")

## Load libraries

suppressMessages(library(tidyverse))
suppressMessages(library(scales))
suppressMessages(library(spotifyr))
suppressMessages(library(sf))
suppressMessages(library(rmapshaper))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))

## Set up credentials for Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = 'yourclientid')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'yourclientsecret')

access_token <- get_spotify_authorization_code()

## Set theme

theme_set(theme_bw())
theme_update(text = element_text(family = "Cambria", size = 12),
             legend.spacing.y = unit(0.5, "cm"))

## Get playlists

raw <- map_dfr(
  .x = seq(0,1500,50),
  .f = ~search_spotify(q = "Top 50", type = "playlist", market = "DE", limit = 50, offset = .x) %>% 
    select(id, name, owner = owner.display_name)
)

## Read in and prepare shapefile 

europe_shp <- sf::read_sf("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  filter(CONTINENT == "Europe") %>% 
  select(country = NAME_EN, geometry) %>% 
  st_crop(., xmin = -25, xmax = 45, ymin = 30, ymax = 73) %>% 
  ms_simplify()

## Create dataset

playlist_data <- raw %>% 
  filter(owner == "spotifycharts" 
         & grepl(" Top 50", name)) %>% 
  mutate(across(name,~str_to_title(.))) %>% 
  filter(!grepl("Top 50 De", name)) %>% 
  mutate(country = gsub(" Top 50", "", name)) %>%
  select(id, country) %>% 
  filter(country %in% europe_shp$country) %>%
  arrange(country) %>% 
  mutate(track_info = map(id,~get_playlist_audio_features("spotifycharts", .) %>% 
                            select(where(is.double), explicit  = track.explicit, 
                                   popularity = track.popularity, -loudness) %>% 
                            summarise(across(everything(),mean)))) %>% 
  unnest(track_info) %>% 
  mutate(across(popularity,~rescale(., to = c(0,1), from = c(0,100)))) %>% 
  left_join(europe_shp, ., "country")

## Choose features & generate plot

features <- c("popularity", "energy", "danceability", "valence",
              "speechiness", "acousticness", "instrumentalness", "explicit")

for (i in seq_along(features)) {
  
  p <- playlist_data %>% 
    select(country, geometry, feature = features[i]) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = feature), color = "black", size = 0.3) +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, name = NULL) +
    theme(strip.background = element_blank(),
          strip.text.x.top = element_text(size = 12),
          strip.text.y.left = element_text(angle = 0, size = 12),
          panel.grid.major = element_line(colour = "transparent"),
          # panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = features[i])
  
  assign(paste0("plot_",features[i]), p)
  
}

plot_features <- grid.arrange(plot_popularity, plot_energy, plot_danceability, plot_valence,
                              plot_speechiness, plot_acousticness, plot_instrumentalness, plot_explicit, 
                              nrow = 2, 
                              top = textGrob("\n Features of the Spotify Top 50 playlists across Europe",
                                             gp = gpar(fontsize = 20, fontfamily = "Cambria")))

## Save plot

ggsave("plot_features.png", plot_features, width = 25, height = 12, dpi = "print", limitsize = F)
