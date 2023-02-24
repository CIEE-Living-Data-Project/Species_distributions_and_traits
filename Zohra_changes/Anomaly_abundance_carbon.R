# Calculate abundanuce anomally

# Load required packages
library(dplyr)
library(zoo)

# Read in the data (replace file_path with the path to your data file)
zooplankton_data <- zoop.SOG

# Convert the date column to a date object
zooplankton_data$date <- as.Date(zooplankton_data$date, format = "%Y-%m-%d")

# Calculate the annual mean abundance for each year
annual_mean_abundance <- zooplankton_data %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(mean_abundance = mean(`Abundance(indiv/m3)`))

# Calculate the anomaly of abundance for each year
anomaly_abundance <- annual_mean_abundance %>%
  mutate(anomaly = as.numeric(scale(mean_abundance))) %>%
  # set anomaly color scale
  mutate(sign = if_else(anomaly <0, "neg","pos")) %>% 
  mutate(sign = factor(sign, levels = c("pos","neg")))

# Print the results
anomaly_abundance


ggplot(anomaly_abundance, aes(x = year, y = anomaly, fill = sign)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1996,2020,2)) +
  scale_color_manual(values = c("cyan4","firebrick")) +
  xlab("Year") +
  theme(legend.position = "none", strip.text.x = element_text(size = 16))



# Calculate traits anomally

# Load required packages
library(dplyr)
library(zoo)

# Read in the data (replace file_path with the path to your data file)
zooplankton_data_traits <- zoop.SOG.traits

# NULLs to NAs

# Replace NULL with NA
zoop.SOG.traits_carbon[is.null(zoop.SOG.traits_carbon)] <- NA


# Calculate the annual mean abundance for each year
annual_mean_carbon <- zoop.SOG.traits_carbon %>%
  group_by(Year) %>%
  summarize(mean_carbon = mean(traitValue, na.rm = TRUE))


# Calculate the anomaly of abundance for each year
anomaly_carbon <- annual_mean_carbon %>%
  mutate(anomaly = as.numeric(scale(mean_carbon))) %>%
  # set anomaly color scale
  mutate(sign = if_else(anomaly <0, "neg","pos")) %>% 
  mutate(sign = factor(sign, levels = c("pos","neg")))

# Print the results
anomaly_carbon


ggplot(anomaly_carbon, aes(x = Year, y = anomaly, fill = sign)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1996,2020,2)) +
  scale_color_manual(values = c("cyan4","firebrick")) +
  xlab("Year") +
  theme(legend.position = "none", strip.text.x = element_text(size = 16))


library(gridExtra)
grid.arrange(ano)
