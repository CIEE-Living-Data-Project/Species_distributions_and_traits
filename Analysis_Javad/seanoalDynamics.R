#Javad

# data prep ----------------------
# looking for different values of a trait
table (traits.lvl2 %>% filter (traitName == "trophicGroup") %>% select (traitValue))

#Sites that have high resolution:
ggplot(data = zoop.SOG.meta, aes(x = dayofyear, y = Year)) +
  geom_point(aes(color = Twilight)) +
  # Roughly mark seasons
  geom_vline(xintercept = c(32,120,212,304), linetype="dotted", color = "blue")+
  facet_wrap(~ Station)

HighResZoopSites <- c ("11", "22", "24", "28", "38", "40", "41", "CPF1", "CPF2", "GEO1", "QU39")

# Location of sites with High Zoop resolution
SOGmap +
  geom_point(data = zoop.SOG.meta %>% filter (Station %in% HighResZoopSites ), aes(x = Longitude, y = Latitude,
                                       color = `Bottom_depth(m)`)) +
  ggtitle("Distribution of zooplankton net samples")

# Different values of a trait
traits.lvl2 %>% filter (traitName == "verticalDistribution") %>% select (traitValue)%>% unique()

# Biomass over time (date after bloom) of different size categories

# Temporal distribution of samples
ggplot(data = ctd.SOG, aes(x = dayofyear, y = Year)) +
  geom_point(aes(color = Twilight), alpha = 0.5) +
  # Roughly mark seasons
  geom_vline(xintercept = c(32,120,212,304), linetype="dotted", color = "blue") +
  facet_wrap(~Station)

HighResCTDSites <- c ("11", "14", "22", "28", "38", "40", "41", "CPF1", "CPF2", "GEO1")

# seasonal biomass dynamics across different size classes ------------------------------

# Histogram of different average biomass

ggplot (data = zoop.SOG, aes (x =log( `Average_biomass_per_individual(mg)`) ))+
  geom_histogram(bins = 15)

# Summing data accross size classes 

zoop.SOG.sizeclass <- zoop.SOG %>% 
  mutate (logBiomassPerIndiv = log (`Average_biomass_per_individual(mg)`)) %>% 
  mutate (sizeClass = case_when(logBiomassPerIndiv < -7 ~ 1,
                                logBiomassPerIndiv < -4 & logBiomassPerIndiv >= -7 ~ 2,
                                logBiomassPerIndiv < -1 & logBiomassPerIndiv >= -4 ~ 3, 
                                logBiomassPerIndiv < 2 & logBiomassPerIndiv >= -1 ~ 4,
                                logBiomassPerIndiv >= 2 ~ 5)) %>% 
  group_by(Index, sizeClass) %>% 
  summarise(biomassSum = sum(`Biomass(mg/m3)`, na.rm= TRUE)) %>% 
  left_join(zoop.SOG %>% select (Index, Station, Year, Month, dayofyear) %>% unique())

# plotting biomass of different size classes

ggplot (data = zoop.SOG.sizeclass %>% filter (Station %in% HighResZoopSites), aes(x = dayofyear, y = log (biomassSum), color = factor(sizeClass), group = factor(sizeClass) ))+
  geom_point()+
  geom_line()+
  facet_wrap (~Station)

# doing the same thingbut focusing on month and averaging across locations
zoop.SOG.sizeclass.mean <- zoop.SOG %>% 
  mutate (logBiomassPerIndiv = log (`Average_biomass_per_individual(mg)`)) %>% 
  mutate (sizeClass = case_when(logBiomassPerIndiv < -7 ~ 1,
                                logBiomassPerIndiv < -4 & logBiomassPerIndiv >= -7 ~ 2,
                                logBiomassPerIndiv < -1 & logBiomassPerIndiv >= -4 ~ 3, 
                                logBiomassPerIndiv < 2 & logBiomassPerIndiv >= -1 ~ 4,
                                logBiomassPerIndiv >= 2 ~ 5)) %>% 
  group_by(Index, sizeClass) %>% 
  summarise(biomassSum = sum(`Biomass(mg/m3)`, na.rm= TRUE)) %>% 
  left_join(zoop.SOG %>% select (Index, Station, Year, Month, dayofyear) %>% unique()) %>% 
  group_by(Month, sizeClass) %>% 
  summarise(biomassSum = mean (biomassSum, na.rm = TRUE))


zoop.SOG.sizeclass2 <- zoop.SOG %>% 
  mutate (logBiomassPerIndiv = log (`Average_biomass_per_individual(mg)`)) %>% 
  mutate (sizeClass = case_when(logBiomassPerIndiv < -5 ~ 1,
                                logBiomassPerIndiv <= 0 & logBiomassPerIndiv >= -5 ~ 2,
                                logBiomassPerIndiv > 0 ~ 3)) %>% 
  group_by(Index, sizeClass) %>% 
  summarise(biomassSum = sum(`Biomass(mg/m3)`, na.rm= TRUE)) %>% 
  left_join(zoop.SOG %>% select (Index, Station, Year, Month, dayofyear) %>% unique())

# plotting biomass of different size classes

ggplot (data = zoop.SOG.sizeclass , aes(x = factor(Month), y = log (biomassSum), color = factor(sizeClass)))+
  geom_boxplot()+
  facet_wrap(~sizeClass)

ggplot (data = zoop.SOG.sizeclass , aes(x = factor(Month), y = log (biomassSum)))+
  geom_boxplot()

ggplot (data = zoop.SOG.sizeclass , aes(x = factor(Month), y = log (biomassSum)))+
  geom_boxplot()+
  facet_wrap(~Year)


ggplot (data = zoop.SOG.sizeclass2 %>%  filter (Year > 2014), aes(x = factor(Month), y = log (biomassSum), color = factor(sizeClass)))+
  geom_boxplot()+
  facet_grid(cols = vars(sizeClass) ,rows= vars( Year))

# conclusion: larger things peak later and start rising with a delay

###




# 
# seasonal biomass dynamics across different feeding modes


# making a zoop abundance file that has traits

zoop.SOG.traits <- zoop.SOG %>% left_join(
  traits.lvl2 %>% select (traitName, traitValue, scientificName) %>%  pivot_wider( names_from = traitName, values_from = traitValue) %>% unique()
)
