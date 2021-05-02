# Libraries ####

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(purrr)        # to apply functions

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

# counting time lags

# round lags on -2 digits
wildschwein_BE$timelag_rounded <- round(wildschwein_BE$timelag, -2)

# build categories with rounded lags
wildschwein_BE_lags <- wildschwein_BE %>%
  st_drop_geometry() %>% 
  group_by(timelag_rounded) %>%
  summarise(count = n())

wildschwein_BE_lags

# visualise filtered timelag data
ggplot(wildschwein_BE, aes(timelag))+
  geom_histogram() + 
  scale_y_log10() +
  ylab("count log10")
  
# visualise orignal data with lags < 15000 (as in task graphics)
wildschwein_BE %>% 
  filter(timelag < 15000) %>% 
  ggplot(., aes(timelag))+
  geom_histogram(binwidth = 100) + 
  scale_y_log10() +
  ylab("count log10")

# viualise change in lag over time
wildschwein_BE %>% 
  filter(timelag < 30000) %>% 
  ggplot(., aes(DatetimeUTC, timelag, col = TierID )) +
  geom_point() +
  geom_line(size = 0.1)

# consistent lags for 002A with a change in oct 14 and march 15,
# also consistent for 018A until may 15, quite inconsistent after that

# Taks 2: Deriving movement parameters I: Speed ####

# caluculating steplength 

 wildschwein_BE$steplength <- wildschwein_BE %>% 
   {(.$E - lead(.$E))^2 + (.$N - lead(.$N))^2} %>% 
   sqrt()

# or without piping (more repetition but no {}, see ?magrittr::`%>%`)
# wildschwein_BE$steplength <- sqrt(
#   (wildschwein_BE$E - lead(wildschwein_BE$E))^2 + (wildschwein_BE$N - lead(wildschwein_BE$N))^2
# )

# calculating speed based on timelag (t) in secs and steplength (s) in meter

wildschwein_BE$speed <- wildschwein_BE %>% {.$steplength/.$timelag} 

# Task 3: Cross-scale movement analysis ####
