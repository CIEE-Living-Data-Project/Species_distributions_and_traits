load("data/SOG_physics_data_2023_01_10.RData")
library(tidyverse)

physics.merge <- left_join(physics.bottle, physics.meta, by = "NetKey")


# PolySet data frames for mapping the Strait of Georgia from the PBSmapping package
data(nepacLL)
data(nepacLLhigh) #higher resolution maps
colnames(nepacLLhigh) <- c("group", "POS", "lon", "lat") # rename columns
colnames(nepacLL) <- c("group", "POS", "lon", "lat")

# This is the high resolution base map for the SoG. Please change data = "nepacLL" if a lower resolution without some small islands is preferred.
SOGmap <- ggplot() + 
  geom_polygon(data=nepacLLhigh, aes(lon, lat, group=group), 
               fill="grey85", size=0.2, color="black") +
  coord_map(projection='mercator', 
            xlim = c(-126, -122), ylim = c(48, 51)) + 
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  theme_classic()


# Spatial distribution of samples. Note that there are multiple overlapping points and there are a few stations that are regularly sampled.
SOGmap +
  geom_point(data = filter(physics.merge, (!is.na(CTDKey.x))), aes(x = Longitude, y = Latitude, color = "Nitrl50"),
             alpha = 0.5) +
  ggtitle("Distribution of Nitrogen")
