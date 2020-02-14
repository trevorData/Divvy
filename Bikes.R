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

raw.df$from_latitude <- raw.df$from_latitude %>% as.numeric
raw.df$from_longitude <- raw.df$from_longitude %>% as.numeric

raw.df$to_latitude <- raw.df$to_latitude %>% as.numeric
raw.df$to_longitude <- raw.df$to_longitude %>% as.numeric

# Calculate columns describing time since midnight
raw.df$start_mins <- raw.df$start_h * 60 + raw.df$start_m
raw.df$stop_mins  <- raw.df$stop_h * 60 + raw.df$stop_m

raw.df$start_q <- (raw.df$start_mins/15) %>% floor
raw.df$stop_q <- (raw.df$stop_mins/15) %>% floor

df <- raw.df %>% select(c("trip_id", "from_station_id", "from_latitude", "from_longitude", "to_latitude", "to_longitude", "start_mins", "stop_mins", "start_q", "stop_q"))

# Load Map Background from Google
register_google(Sys.getenv('gmaps_key'))

mapImage <- get_map(location = c(lon = -87.68, lat = 41.9), #c(-88.0, 41.63, -87.4, 42.1),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 11)

# Set visualization styles
coord_lims <- coord_map(xlim = c(-87.95, -87.4),
                        ylim = c(41.68, 42.1))

#######################################################################################

station.counts <- df %>% count(from_station_id, from_latitude, from_longitude, start_q)
  

# Plot Map
ggmap(mapImage) +
  geom_bin2d(data = df[df$start_q == 17*4,], 
           aes(x=from_longitude, y=from_latitude), 
           bins=60, alpha=.8) +
  scale_fill_gradient2(limits=c(0, 200), 
                       low = 'white',
                       mid = 'red4',
                       high = 'red4',
                       midpoint = 70,
                       na.value = 'red4')

ggmap(mapImage) +
  geom_bin2d(data = df[df$start_q == 0*4,], 
             aes(x=from_longitude, y=from_latitude), 
             bins=60, alpha=.8) +
  scale_fill_gradientn(colours = c('white', 'white','slateblue4', 'darkcyan', 'yellow2'), 
                       values = c(0, .01, .02, .5, 1),
                       limits= c(0, 300), 
                       na.value = 'yellow2')









  scale_fill_gradientn(colours = c('blue', 'seagreen', 'yellow'),
                       values = c(0, .25, 1), 
                       limits=c(2, 1700),
                       na.value = NA)



