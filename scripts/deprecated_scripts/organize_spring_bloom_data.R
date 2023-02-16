# Organish SOG Bloomcast results
# P. Pata
# February 16, 2023
#
# Data provided by S. Allen.
#
# Allen, S.E. and M.A. Wolfe. Hindcast of the timing of the spring phytoplankton bloom 
#   in the Strait of Georgia, 1968-2010. Progress in Oceanography, 115, 6-13 (2013).
# Allen S, Latornell D. (2022). Spring phytoplankton bloom timing in the Strait of 
#   Georgia. Boldt J, Joyce E, Tucker S, Gauthier S. State of the Physical, 
#   Biological and Selected Fishery Resources of Pacific Canadian Marine Ecosystems 
#   in 2021. : 165-167. Published, Canadian Technical Report of Fisheries and 
#   Aquatic Sciences.
#

fol <- "data/SOG_BloomCastResults/"

bloom <- read.table(paste0(fol,"bloomdates_16mar2023.dat"), skip = 4)
colnames(bloom) <- c("Year","YearDay")

write.csv(bloom, file = "data/SOG_phytoplankton_bloom_date.csv")