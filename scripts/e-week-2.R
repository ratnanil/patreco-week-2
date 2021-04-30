# Libraries ####

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(purrr)        # to apply functions
library(tmap)

# Import data ####

wildschwein_BE <- read_delim("data/wildschwein_BE_2056.csv",",") 

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_BE

# Task 1: Overview ####

# group by Animal ID
wildschwein_BE <- group_by(wildschwein_BE,TierID)

# define timelag
wildschwein_BE <- wildschwein_BE %>% 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC), units = "secs"))

# overview
summary(wildschwein_BE)
unique(wildschwein_BE$TierID) # 3 individuals are tracked
unique(wildschwein_BE$TierName) # names of individuals

# plot data points
ggplot(wildschwein_BE, aes (color = TierID)) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056)

# duration of tracking for each individual:
wildschwein_BE %>% 
  st_drop_geometry() %>%
    split(.$TierID) %>% map(summary) #looking at summary for each animal

wildschwein_BE %>% # same with dplyr
  st_drop_geometry() %>%
  summarise(mean = mean(timelag, na.rm = T), min = min(timelag, na.rm = T), max = max(timelag, na.rm = T)) # same with dplyr

# optical representation of tracking duration
ggplot(wildschwein_BE, aes(x = DatetimeUTC, y = TierName)) +
  geom_point() +
  scale_x_datetime(breaks = "1 month") +
  theme_grey()

wildschwein_BE$timelag_rounded <- round(wildschwein_BE$timelag, -2)

ggplot(wildschwein_BE, aes(timelag_rounded))+
  geom_histogram(binwidth = 50) +
  xlim(c(0, 2000))


# Taks 2: Deriving movement parameters I: Speed ####

