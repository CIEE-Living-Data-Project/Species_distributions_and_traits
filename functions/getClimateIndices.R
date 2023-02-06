# getClimateIndices() function
# Created by: P. Pata
# Last updated: January 26, 2023
#
# This function returns the monthly climate index for the specified period. 
# Most of the indices were downloaded from https://psl.noaa.gov. Note that
# this site could point to external sources which originally generated the
# index. Cite the original source when necessary. NPGO was downloaded from 
# http://www.o3d.org/npgo/. 
# To run this function the file climateIndices_20230126.RData should be present
# in the function directory.
#
# *Arguments*
# index : acronym/s of the index as listed below
# year.s : start year to extract
# year.e : end year to extract
#
# *Value*
# This returns a long data frame with [year, month, and index values...]. Blanks
#   are marked as -9999.
#
# List of indices:
#  1. NPGO (North Pacific Gyre Oscillation): This is the 2nd dominant mode of
#       sea surface height varability in the Northeast Pacific. Positively 
#       correlates with fluctuations in salinity, nutrients, and chlorophyll-a 
#       in the California current system and the Gulf of Alaska. 
#  2. SOI (Southern Oscillation Index): This is the normalized pressure between
#       Tahiti and Darwin.
#  3. PDO (Pacific Decadal Oscillation): The first mode of sea surface 
#       temperature anomaly (and sea surface height anomaly) of the Northern 
#       Pacific Ocean. 
#  4. Nino34 (Nino 3.4 SST Index): Calculated from HadISST1 and is the area 
#       averaged SST from 5S-5N and 170-120W.
#  5. Nino34_anomaly: The Nino 3.4 index with the 1981-2010 mean removed.
#  6. Nino12 (Nino 1+2 SST Index): Calculated from HadISST1 and is the area 
#       averaged SST from 0-10S and 90-80W.
#  7. Nino12_anomaly: The Nino 1+2 index with the 1981-2010 mean removed.
#  8. Nino3 (Nino 3 SST Index): Calculated from HadISST1 and is the area 
#       averaged SST from 5S-5N and 150-90W.
#  9. Nino3_anomaly: The Nino 3 index with the 1981-2010 mean removed.
#  10. Nino4 (Nino 4 SST Index): Calculated from HadISST1 and is the area 
#       averaged SST from 5S-5N and 160E-150W.
#  11. Nino4_anomaly: The Nino 4 index with the 1981-2010 mean removed.
#  12. GMSST (Global Mean SST): This is the global mean land/ocean temperature
#       index based on SST and is an anomaly from NASA/GISS. 
#  13. GLBTS (Global Land-Ocean Temperature Index): This is the global mean 
#       temperature anomalies from meteorological stations only from NASA.
#  14. GLBTSSST (Global Land-Ocean Temperature Index): This is the global mean 
#       temperature anomalies from meteorological stations and SST from NASA.
#  15. MEI (Multivariate ENSO Index V2): This is bimonthly data and centered
#       between two months (e.g., Jan represents Dec-Jan).
#  16: ONI (Ocean Nino Index): The 3-month running mean of NOAA ERSST.V5 SST
#       anomalies in the Nino 3.4region with chaning base period centered on 
#       30-year base periods. Centered between three months (e.g., Jan 
#       represents Dec-Jan-Feb)

require(dplyr)
require(tidyr)
getClimateIndices <- function(index, year.s = 1980, year.e = 2020) {
  load("../data/climateIndices_20230126.RData")
  
  output <- climate.indices %>% 
    filter(Index %in% tolower(index)) %>% 
    filter(Year >= year.s & Year <= year.e)
  
  return(output)
}
