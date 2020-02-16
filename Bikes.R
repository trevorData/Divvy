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
register_google(Sys.getenv('google_maps_api'))

mapImage <- get_map(location = c(lon = -87.58, lat = 41.9), #c(-88.0, 41.63, -87.4, 42.1),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 11)

# Set plot themes
themes <- theme(legend.position = c(0.93, 0.16),
                legend.background = element_blank(),
                legend.text = element_text(colour = 'white', size=6),
                legend.title = element_text(color = 'white', size=6.5),
                axis.title = element_blank(), 
                axis.text = element_blank()
                )

label <- labs(fill = "# Rides\nper 15 min")

ann_x <- -87.5
ann_y <- 42.035
ann_size <- 9

#######################################################################################


# Plot Map
ggmap(mapImage) +
  geom_bin2d(data = df[df$start_q == 16*4,], 
             aes(x=from_longitude, y=from_latitude), 
             bins=60, alpha=.8) +
  scale_fill_gradientn(colours = c('white','purple4', 'deeppink3', 'yellow'), 
                       values = c(0, .03, .5, 1),
                       limits= c(0, 300), 
                       na.value = 'yellow2') +
  themes + label +
  annotate('text', label = 'Chicago Bike Rentals',  
           family = 'serif', fontface='bold', size = 9.5, color = 'white', 
           x = -87.51, y = 42.04) +
  annotate('text', label = 'Frequency of Divvy bikes checked out\n on a typical summer weekday',  
           family = 'serif', size = 4, color = 'white', 
           x = -87.507, y = 42.015) +
  annotate('text', label = 'Each tile represents a\n12 km wide area at:',  
         family = 'serif', size = 3, color = 'white', 
         x = -87.495, y = 41.96) +
  annotate('text', label = '5:00 PM',  
           family = 'serif', size = 8, color = 'white', fontface='bold',
           x = -87.494, y = 41.94)
