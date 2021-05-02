# LIBRARIES ####

library(readr) # to import tabular data (e.g. csv)
library(dplyr) # to manipulate (tabular) data
library(ggplot2) # to visualize data
library(sf) # to handle spatial vector data
library(terra) # to handle raster data
library(lubridate) # to handle dates and times
library(purrr) # to apply functions
library(zoo)  # moving window function

# DATA IMPORT ####

wildschwein_BE <- read_delim("data/wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_BE

# TASK 1: OVERVIEW ####

# group by Animal ID ####
wildschwein_BE <- group_by(wildschwein_BE, TierID)

# define timelag ####
wildschwein_BE <- wildschwein_BE %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))

# overview ####
summary(wildschwein_BE)
unique(wildschwein_BE$TierID) # 3 individuals are tracked
unique(wildschwein_BE$TierName) # names of individuals

# plot data points ####
ggplot(wildschwein_BE, aes(color = TierID)) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056)

# duration of tracking for each individual ####
wildschwein_BE %>%
  st_drop_geometry() %>%
  split(.$TierID) %>%
  map(summary) # looking at summary for each animal

# wildschwein_BE %>% # same with dplyr
#   st_drop_geometry() %>%
#   summarise(mean = mean(timelag, na.rm = T), min = min(timelag, na.rm = T), max = max(timelag, na.rm = T)) # same with dplyr

# optical representation of tracking duration ####
ggplot(wildschwein_BE, aes(x = DatetimeUTC, y = TierName)) +
  geom_point() +
  scale_x_datetime(breaks = "1 month") +
  theme_grey()

# counting time lags ####

# round lags on -2 digits
wildschwein_BE$timelag_rounded <- round(wildschwein_BE$timelag, -2)

# build categories with rounded lags
wildschwein_BE_lags <- wildschwein_BE %>%
  st_drop_geometry() %>%
  group_by(timelag_rounded) %>%
  summarise(count = n())

wildschwein_BE_lags

# visualise filtered timelag data ####
ggplot(wildschwein_BE, aes(timelag)) +
  geom_histogram() +
  scale_y_log10() +
  ylab("count log10")

# visualise orignal data with lags < 15000 (as in task graphics) ####
wildschwein_BE %>%
  filter(timelag < 15000) %>%
  ggplot(., aes(timelag)) +
  geom_histogram(binwidth = 100) +
  scale_y_log10() +
  ylab("count log10")

# viualise change in lag over time ####
wildschwein_BE %>%
  filter(timelag < 30000) %>%
  ggplot(., aes(DatetimeUTC, timelag, col = TierID)) +
  geom_point() +
  geom_line(size = 0.1)

# consistent lags for 002A with a change in oct 14 and march 15,
# also consistent for 018A until may 15, quite inconsistent after that

# Taks 2: Deriving movement parameters I: Speed ####

# caluculating steplength ####

wildschwein_BE$steplength <- wildschwein_BE %>%
  {
    (.$E - lead(.$E))^2 + (.$N - lead(.$N))^2
  } %>%
  sqrt()

# or without piping (more repetition but no {}, see ?magrittr::`%>%`)
# wildschwein_BE$steplength <- sqrt(
#   (wildschwein_BE$E - lead(wildschwein_BE$E))^2 + (wildschwein_BE$N - lead(wildschwein_BE$N))^2
# )

# calculating speed based on timelag (t) in secs and steplength (s) in meter ####

wildschwein_BE$speed <- wildschwein_BE %>% {
  .$steplength / .$timelag
}

# TASK 3: CROSS-SCALE MOVEMENT ANALYSIS ####

# import new dataset ####

caro <- read_delim("data/caro60.csv", ",")

attr(caro$DatetimeUTC, "tzone") # checking timezone

# renaming cols beacause N and E were flipped in source data
caro %>%  rename(N = E, E = N)

caro <- st_as_sf(caro, coords = c("N", "E"), crs = 2056, remove = F)

# reduce granularity by sequentialising dataset by 3, 6 and 9 ####

seq3 <- seq(from = 1, to = 200, by = 3)
seq6 <- seq(from = 1, to = 200, by = 6)
seq9 <- seq(from = 1, to = 200, by = 9)

caro_3 <- caro %>% slice(seq3)
caro_6 <- caro %>% slice(seq6)
caro_9 <- caro %>% slice(seq9)

# calculate timelag, steplength and speed for new data sets ####

l_caro <- list(caro, caro_3, caro_6, caro_9) # save list of all three tibbles
n_caro <- c("caro", "caro_3", "caro_6", "caro_9") # define names of list entries (aka tibbles)
names(l_caro) <- n_caro # apply names to list

# perform mutation on list using purr::map ####
l_caro <- l_caro %>% 
  map(~ mutate(., timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))) %>% 
  map(~ mutate(., steplength = sqrt((.$E - lead(.$E))^2 + (.$N - lead(.$N))^2 ))) %>% 
  map(~ mutate(., speed = steplength / timelag))

# return tibbles individually to the environemnt #### 

list2env(l_caro, envir = .GlobalEnv) # (if necessary)

# plot trajectories ####

# 1 minute vs 3 minutes

l_caro_rbind <- l_caro %>% bind_rows(.id = "df")

l_caro_rbind %>% 
  filter(df == "caro" | df == "caro_3") %>% 
  ggplot(aes(E, N, colour = df)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
    theme_bw() +
  theme(panel.border = element_blank()) +
  scale_color_discrete(name = "Trajectories", labels = c("1 Minute", "3 Minutes"))

# 1 minute vs 6 minutes 
l_caro_rbind %>% 
  filter(df == "caro" | df == "caro_6") %>% 
  ggplot(aes(E, N, colour = df)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  scale_color_discrete(name = "Trajectories", labels = c("1 Minute", "6 Minutes"))

# 1 minute vs 9 minutes
l_caro_rbind %>% 
  filter(df == "caro" | df == "caro_9") %>% 
  ggplot(aes(E, N, colour = df)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  scale_colour_discrete(name = "Trajectories", labels = c("1 Minute", "9 Minutes"))

# plot speed depending on trajectories

labs = c("1 Minute", "3 Minutes", "6 Minutes", "9 Minutes")

l_caro_rbind %>% 
  ggplot(., aes(DatetimeUTC, speed, colour = df)) +
  geom_line() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  scale_colour_discrete(name = "Intervals", labels = labs) +
  xlab("Time") +
  ylab("Speed (m/s)")

# TASK 4: ROLLING WINDOW FUNCTIONS ####

# example with dummy data ####
example <- rnorm(10)

rollmean(example, k = 3, fill = NA, align = "left")
rollmean(example, k = 4, fill = NA, align = "left")

# testing different rolling windows
caro$smooth03 <- rollmean(caro$speed, k = 3, fill = NA, allign = "left")
caro$smooth04 <- rollmean(caro$speed, k = 4, fill = NA, allign = "left")
caro$smooth05 <- rollmean(caro$speed, k = 5, fill = NA, allign = "left")
caro$smooth10 <- rollmean(caro$speed, k = 10, fill = NA, allign = "left")
caro$smooth15 <- rollmean(caro$speed, k = 15, fill = NA, allign = "left")
caro$smooth20 <- rollmean(caro$speed, k = 20, fill = NA, allign = "left")

# plotting rolling windows ####

# pivot longer
caro_longer <- caro %>% pivot_longer(., cols = starts_with("smooth"), names_to = "window", values_to = "speed_smooth") 

caro_longer %>% 
  ggplot(., aes(DatetimeUTC, speed_smooth, colour = window)) +
  geom_line() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  scale_colour_discrete(name = "Windows") +
  xlab("Time") +
  ylab("Speed (m/s)")
