# Process wind data for Sandheads station (ID 6831)
# P. Pata
# Created February 15, 2023
#
# Data downloaded via Cygwin and https://climate.weather.gc.ca/climate_data
# Data are saved as monthly files of hourly climate station variables. 
#   These are merged here into a single data frame. The wind stress is
#   calculated following Perry et al. 2021 PLOS ONE.
#
# See https://climate.weather.gc.ca/glossary_e.html#windDir for data dictionary.
#
# In the data, missing values are flagged as "M", and estimated values as "E"

library(tidyverse)

# List files
fol <- "C:/Owncloud_EOAS/Data/Sandheads_station_data/"
files <- list.files(fol, pattern = ".csv")

# Loop: Open files and add to a single dataframe
sandheads <- data.frame()
for (i in files){
  sandheads <- bind_rows(sandheads,
                         read.csv(paste0(fol,i)))
}

# Select and revise column names
sandheads <- sandheads[,c(3,4,6,7,8,10,16,18)] %>% 
  rename(Temperature.C = `Temp...C.`, 
         Wind.Direction.10s.deg = `Wind.Dir..10s.deg.`,
         Wind.Speed.km.h = `Wind.Spd..km.h.`) %>% 
  # Arrange by date
  arrange(Day, Month, Year) %>% 
  # Convert wind speed to m/s
  mutate(Wind.Speed.m.s = Wind.Speed.km.h / 3.6) %>% 
  # Calculate wind stress (kg m-2 s-1)
  # windstress = density of air (1.22 kg m-3) * wind-drag coefficient (0.0013) * wind sped (m s-1)
  mutate(Wind.Stress = 1.22 * 0.0013 * Wind.Speed.m.s) %>% 
  # Calculate daily means, then monthly means
  group_by(Station.Name, Climate.ID, Year, Month, Day) %>% 
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  group_by(Station.Name, Climate.ID, Year, Month) %>% 
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

# Export
write.csv(sandheads, file = "data/Sandheads_wind_20230215.csv", row.names = FALSE)
