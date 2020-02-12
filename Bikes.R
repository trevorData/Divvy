setwd('~/Projects/Divvy')

library(RSocrata)
library(dplyr)
library(ggmap)

# Load data from Chicago Data API
url <- paste('https://data.cityofchicago.org/resource/fg6s-gzvg.json?',
             '$where=date_extract_y(start_time)=2019 AND ',
             'date_extract_dow(start_time) between 1 and 4 AND ',
             'date_extract_m(start_time)=7&',
             '$limit=500000', 
             sep = '')

raw.df <- read.socrata(url)

# Format Columns
raw.df$start_h <- raw.df$start_time %>% format("%H") %>% as.numeric
raw.df$stop_h  <- raw.df$stop_time %>% format("%H") %>% as.numeric

raw.df$start_m <- raw.df$start_time %>% format("%M") %>% as.numeric
raw.df$stop_m  <- raw.df$stop_time %>% format("%M") %>% as.numeric

raw.df$start_mins <- raw.df$start_h * 60 + raw.df$start_m
raw.df$stop_mins  <- raw.df$stop_h * 60 + raw.df$stop_m

raw.df$from_latitude <- raw.df$from_latitude %>% as.numeric
raw.df$from_longitude <- raw.df$from_longitude %>% as.numeric

df <- raw.df %>% select(c("trip_id", "from_latitude", "from_longitude", "to_latitude", "to_longitude", "start_mins", "stop_mins"))

# Load Map Background from Google
register_google(Sys.getenv('gmaps_key'))

mapImage <- get_map(location = c(lon = -87.68, lat = 41.9), #c(-88.0, 41.63, -87.4, 42.1),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 10)

# Set visualization styles
coord_lims <- coord_map(xlim = c(-87.95, -87.4),
                        ylim = c(41.68, 42.1))

# Plot Map
ggmap(mapImage) +
  geom_point(data = df[df$start_mins == 1000,], 
             aes(x=from_longitude, y=from_latitude), 
             color='blue') +
  coord_lims


