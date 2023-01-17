# Prepare subset of the trait dataset from internal files
# Created by: Patrick Pata
# Last modified: Jan 16, 2023

library(dplyr)
`%notin%` <- Negate(`%in%`)


# load zooplankton trait dataset from source
load("../UBC_zoop_traits_paper/data_raw/trait_dataset_level2-2022-12-15.RData")


zooplankton.traits <- traits.lvl2 %>%
  # exclude a few traits for now
  filter(traitName %notin% c("afdwPDW","ratioNP","ratioCN","ratioCP","hydrogenPDW",
                             "hydrogenTotal","chitinPDW","ashPDW","ratioProteinLipid",
                             "ashPWW","proteinPAFDW","lipidPAFDW",
                             "energyDensityVolume","energyDensityDW",
                             "energyDensityAFDW","growthRate_15C","growthRate_WSC_15C",
                             "eggDiameter","lifeSpan","developmentDuration",
                             "afdw","habitatAssociation",
                             "hibernation","restingEggs",
                             "DVM.weak","DVM.reverse","DVM.strong"))

# TODO: Clean respirationRate_WSC which actually don't have carbon weight records. 
#   were probably reported as weight-specific values but need to verify.

# export
save(zooplankton.traits, file = "data_inputs/zooplankton_traits_2023_01_16.RData")
