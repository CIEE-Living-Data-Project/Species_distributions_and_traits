load("data/SOG_physics_data_2023_01_10.RData")
library(tidyverse)

physics.merge <- left_join(physics.bottle, physics.meta, by = "NetKey")

physics.merge <- physics.merge %>% 
mutate(Season = "Winter") %>% 
  mutate(Season = if_else(Month >= 3 & Month <=5, "Spring", Season),
         Season = if_else(Month >= 6 & Month <=8, "Summer", Season),
         Season = if_else(Month >= 9 & Month <=11, "Fall", Season))

physics.summarizeN <- physics.merge %>% 
  group_by(Station, Season) %>% 
  summarize(mean.N = mean(NitrI50),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude))
  
physics.summarizeN <- left_join(physics.summarizeN, physics.merge, by = "Station")

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
library(viridis)
SOGmap +
  geom_point(data = physics.summarizeN, 
             aes(x = Longitude, y = Latitude, color = mean.N)) +
  scale_color_viridis()+
  facet_wrap(~Season)+
  ggtitle("Distribution of Nitrogen")


ggplot(data = physics.merge, 
       aes(x = NetTowDepth, y = NitrI50))+
  geom_point()+
  facet_wrap(~Season)


