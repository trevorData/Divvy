setwd("~/Projects/Divvy")

library(RSocrata)
library(dplyr)
library(ggmap)

# Load data from Chicago Data  Portal API
url <- paste('https://data.cityofchicago.org/resource/fg6s-gzvg.json?',
             '$where=date_extract_y(start_time)=2019 AND ',
             'date_extract_dow(start_time) between 1 and 4 AND ',
             'date_extract_m(start_time)=7&',
             '$limit=500000', 
             sep = '')

raw.df <- read.socrata(url)

# Format Columns
raw.df$start_h <- raw.df$start_time %>% format("%H") %>% as.numeric
raw.df$start_m <- raw.df$start_time %>% format("%M") %>% as.numeric

raw.df$from_latitude <- raw.df$from_latitude %>% as.numeric
raw.df$from_longitude <- raw.df$from_longitude %>% as.numeric

# Calculate minutes since midnight, aggregate into 15 min quarters
raw.df$start_mins <- raw.df$start_h * 60 + raw.df$start_m
raw.df$start_q <- (raw.df$start_mins/15) %>% floor

df <- raw.df %>% select(c("trip_id", "from_station_id", "from_latitude", "from_longitude", "start_q"))

# Load Map Background from Google
register_google(Sys.getenv('google_maps_api'))

mapImage <- get_map(location = c(lon = -87.58, lat = 41.9), #c(-88.0, 41.63, -87.4, 42.1),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 11)

# Set plot styles
themes <- 
  theme(legend.position = c(0.95, 0.11),
        legend.background = element_blank(),
        legend.text = element_text(colour = 'white', size=6.5),
        legend.title = element_text(color = 'white', size=10),
        axis.title = element_blank(), 
        axis.text = element_blank()
        )

gradient <-
  scale_fill_gradientn(colours = c('white','purple4', 'deeppink3', 'yellow'), 
                       values = c(0, .03, .5, 1), 
                       limits= c(0, 300), 
                       na.value = 'yellow2', 
                       labels = c(0, 5, 10, 15))

label <- labs(fill = "# rides\nper 15 min")

ann_x   <- -87.511
title_y <- 42.04
sub_y   <- 42.015
desc_y  <- 41.96
time_y  <- 41.94

title <- 
  annotate('text', label = 'Chicago Bike Rentals',  
         family = 'serif', fontface='bold', size = 15, color = 'white', 
         x = ann_x, y = title_y)

subtitle <-
  annotate('text', label = 'Frequency of Divvy bike check-outs\non a typical summer weekday',  
         family = 'serif', size = 6, color = 'white', 
         x = ann_x, y = sub_y)

desc <-
  annotate('text', label = 'Each tile represents a\n1/2 mile wide area at:',  
         family = 'serif', size = 4, color = 'white', 
         x = ann_x, y = desc_y)

# Plot Maps
for (i in 0:95){
  
  # Calculate values for time annotation
  period <- if_else(i < 48, ' AM', ' PM')
  
  hour <- floor(i/4)
  hour <- if_else(hour > 12, hour - 12, hour)
  hour <- if_else(hour == 0, 12, hour)
  hour <- sprintf('%02d', hour)
  
  min <- case_when(
    i%%4 == 0 ~ "00",
    i%%4 == 1 ~ "15",
    i%%4 == 2 ~ "30",
    i%%4 == 3 ~ "45"
  )
  
  time <- paste(hour, ":", min, period, sep = "")
  
  # Plot map using set styles and values
  ggmap(mapImage) +
    geom_bin2d(data = df[df$start_q == i,], 
               aes(x = from_longitude, y = from_latitude), 
               binwidth = c(.007, .007), alpha = .8) +
    annotate('text', label = time,  
             family = 'serif', size = 13, color = 'white', fontface='bold',
             x = ann_x, y = time_y) +
    title + subtitle + desc + themes + label + gradient
  
  # Save map
  paste('divvy_plot_', i, '.png', sep = '') %>% ggsave(device = 'png', height = 9, width = 9, dpi = 'screen')
}
