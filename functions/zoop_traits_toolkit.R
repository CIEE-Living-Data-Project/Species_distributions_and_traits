# Trait dataset paper tool kit
# Created by P. Pata
# Last modified February 15, 2023
#
# ***** Trait dataset tool kit *****
# 
# This file contains a set of functions that were utilized in developing and 
# curating the zooplankton trait dataset. A list of the functions with short
# descriptions are reported below. Many of the functions require a the trait
# dataset, trait directory, and taxonomy table.
# 
#
# ----- Function Descriptions -----
#
# ** Data wrangling and curation functions **
#
# convertRateT2WS(df, sizeAssoc, trtName1, trtName2, trtUnit):
#   Converts a by individual rate to a weight-specific rate.
# standardizeTable(file.list, s.format, taxonomy, lifestagelist, trait.directory):
#   Reorders the column and converts the names and formats of the traits.
# convertPW2Total(df, sizeAssoc, trtName1, trtName2, trtUnit = "mg"):
#   Converts percent composition value to total bulk composition value.
# convertTotal2PW(df, sizeAssoc, trtName1, trtName2, trtUnit = "percent"):
#   Converts total bulk composition to percent composition relative to a weight.
# assignMajorGroup(taxonomy): Assigns the zooplankton major group in a taxonomy
#   data frame.
# getMaxObsNum(tN, tI, df): Gets the maximum observation number for a trait and 
#   species.
# updateIDs(data, trait.direcctory): Updats the trait and taxon-trait IDs.
# mergeTraitDetails(df, trait.directory): Combine duplicated trait records of 
#   the same value but from different references.
# annotateLifeStage(data, lifestagelist): Modifies how the life stage was coded,
#   assigns the life stage ID, and records the original life stage.
# annotateTaxonomy(data, taxonomy): Assigns the taxonomic ID and the taxonomic 
#   ranks it belongs to. The recorded scientific name would be stored in the 
#   column verbatimScientificName.
# widen_traits(trait.table, trait.list): Subsets the trait dataset to only 
#    the traits specified in the list. Only retains taxon information.
# getMaxObsNum(traitName, taxonID, df): Gets the maximum observation number for 
#    a trait and species.
# getSpeciesMeanSD(traitValue, traitValueSD, traitValueN): Calculate the 
#   weighted means and SDs of traits when the dataset is a mix of individual
#   values and averaged records.
#
# ** Data imputation functions **
# 
# get_pairwise.N(x,y): Calculates the number of pair of two variables with data.
# correlate_traits(trait.table, trait.list): Calculates correlations between
#    variables in trait.list and returns a long data frame of the correlation,
#    N data pairs, and p-value.
# getGroupLevelValue(taxon, trait, gen.level, trait.df, taxonomy.df)): Calculates
#   the group-level mean, standard deviation, and number of species-level 
#   observations for generalization of a given trait for a taxon. 
# conv.allom(W,a,b,base): General equation for allometric conversions. 
#   The default base is the natural log.
# getpval(x) : Calculates a p value from a regression model F statistic object.
# getRegressionModel(df, grp, X, Y, model): Derives the regression model between  
#   two trait variables for a zooplankton group. 
# calculateFromModel(df, model, excludeWithLit, applyToGeneralized, 
#   excludeCalculated): Calculates a trait using a regression model.
# plotAllometric(df, grp, X, Y, base): Plots the distribution of values between two 
#   traits and a regression line. [!!! Should be sensitive to if OLS or RMA regressions.]
# plotRegModel(model): Plots a simple regression model result with 95% confidence
#   intervals. 
# calculate.WSRates(trait.df, traits.calculated, trait.X, trait.Y): Calculates 
#   the weight specific rate for traits with calculate rate values derived
#   from regression equations.

require(tidyverse)
require(lmodel2)


# ----- Data wrangling and curation functions -----

# For converting individual rates to weight specific
convertRateT2WS <- function(df, sizeAssoc, trtName1, trtName2, trtUnit) {
  D.1 <- df %>% 
    filter(traitName == trtName1)
  D.2 <- df %>% 
    filter(traitName == trtName2) %>% 
    filter(taxonID %notin% D.1$taxonID) %>% 
    # Derived columns are the original columns
    mutate(verbatimScientificName = scientificName,
           verbatimTraitName = traitName,
           verbatimTraitValue = as.character(traitValue),
           verbatimTraitUnit = traitUnit,
           verbatimLifeStage = lifeStage,
           verbatimNotes = notes,
           isMerged = FALSE,
           mergedTraitTaxonIDs = traitTaxonID) %>% 
    select(-c(sizeAssocName, sizeAssocValue, sizeAssocUnit, sizeAssocReference)) %>% 
    left_join(sizeAssoc, by = "taxonID") %>% 
    filter(!is.na(sizeAssocValue)) %>% 
    group_by(traitID, taxonID) %>% 
    mutate(traitName = trtName1,
           traitValue = traitValue/sizeAssocValue,
           traitUnit = trtUnit,
           traitValueSource = "derived",
           traitValueSD = NaN,
           traitValueN = 1,
           notes = paste0("Calculated from ",trtName1," and ",
                          trtName2,"; based on", traitTaxonID),
           maxObsNum = 0) %>% 
    ungroup() %>% 
    standardizeID(trait.directory)
}

# for converting between bulk and percent composition
convertPW2Total <- function(df, sizeAssoc, trtName1, trtName2, trtUnit = "mg"){
  D.1 <- df %>% 
    filter(traitName == trtName1)
  D.2 <- df %>% 
    filter(traitName  == trtName2) %>% 
    filter(taxonID %notin% D.1$taxonID) %>% 
    # Derived columns are the original columns
    mutate(verbatimScientificName = scientificName,
           verbatimTraitName = traitName,
           verbatimTraitValue = as.character(traitValue),
           verbatimTraitUnit = traitUnit,
           verbatimLifeStage = lifeStage,
           verbatimNotes = notes,
           isMerged = FALSE,
           mergedTraitTaxonIDs = traitTaxonID) %>% 
    select(-c(sizeAssocName, sizeAssocValue, sizeAssocUnit, sizeAssocReference)) %>% 
    left_join(sizeAssoc, by = "taxonID") %>% 
    filter(!is.na(sizeAssocValue)) %>% 
    group_by(traitID, taxonID) %>% 
    mutate(traitName = trtName1,
           traitValue = traitValue*sizeAssocValue / 100,
           traitUnit = trtUnit,
           traitValueSource = "derived",
           traitValueSD = NaN,
           traitValueN = 1,
           notes = paste0("Calculated from ",trtName1," and ",
                          trtName2,"; based on", traitTaxonID),
           maxObsNum = 0) %>% 
    ungroup() %>% 
    standardizeID(trait.directory)
  # updateIDs(trait.directory)
}  


convertTotal2PW <- function(df, sizeAssoc, trtName1, trtName2, trtUnit = "percent") {
  D.1 <- df %>% 
    filter(traitName == trtName1)
  D.2 <- df %>% 
    filter(traitName  == trtName2) %>% 
    filter(taxonID %notin% D.1$taxonID) %>% 
    # Derived columns are the original columns
    mutate(verbatimScientificName = scientificName,
           verbatimTraitName = traitName,
           verbatimTraitValue = as.character(traitValue),
           verbatimTraitUnit = traitUnit,
           verbatimLifeStage = lifeStage,
           verbatimNotes = notes,
           isMerged = FALSE,
           mergedTraitTaxonIDs = traitTaxonID) %>% 
    select(-c(sizeAssocName, sizeAssocValue, sizeAssocUnit, sizeAssocReference)) %>% 
    left_join(sizeAssoc, by = "taxonID") %>% 
    filter(!is.na(sizeAssocValue)) %>% 
    group_by(traitID, taxonID) %>% 
    mutate(traitName = trtName1,
           traitValue = traitValue/sizeAssocValue * 100,
           traitUnit = trtUnit,
           traitValueSource = "derived",
           traitValueSD = NaN,
           traitValueN = 1,
           notes = paste0("Calculated from ",trtName1," and ",
                          trtName2,"; based on", traitTaxonID),
           maxObsNum = 0) %>% 
    ungroup() %>% 
    # updateIDs(trait.directory) %>% 
    standardizeID(trait.directory) %>% 
    filter(traitValue <= 100)
}



# for merging duplicated trait values
mergeTraitDetails <- function(df, trait.directory) {
  df <- df %>% 
    mutate(dup = n())
  
  df.same <- df %>% 
    filter(dup == 1) %>% 
    mutate(isMerged = FALSE) %>% 
    ungroup()
  
  df.updated <- df %>% 
    filter(dup > 1) %>% 
    mutate(isMerged = TRUE)  %>% 
    mutate(primaryReference = paste(primaryReference, collapse = "; "),
           primaryReferenceDOI = paste(primaryReferenceDOI, collapse = "; "),
           secondaryReference = paste(secondaryReference, collapse = "; "),
           secondaryReferenceDOI = paste(secondaryReferenceDOI, collapse = "; "),
           lifeStage = paste(lifeStage, collapse = "; "),
           sizeType = paste(sizeType, collapse = "; "), 
           # sizeAssocName = paste(sizeAssocName, collapse = "; "), 
           # sizeAssocUnit = paste(sizeAssocUnit, collapse = "; "), 
           # sizeAssocValue = paste(sizeAssocValue, collapse = "; "), 
           # sizeAssocReference = paste(sizeAssocReference, collapse = "; "), 
           location = paste(location, collapse = "; "), 
           # longitude = paste(longitude, collapse = "; "), 
           # latitude = paste(latitude, collapse = "; "), 
           notes = paste(notes, collapse = "; "),
           traitValueSource = paste(traitValueSource, collapse = "; "), 
           traitValueMeasurementNotes = paste(traitValueMeasurementNotes, collapse = "; "), 
           verbatimScientificName = paste(verbatimScientificName, collapse = "; "), 
           verbatimTraitName = paste(verbatimTraitName, collapse = "; "), 
           verbatimTraitValue = paste(verbatimTraitValue), 
           verbatimTraitUnit = paste(verbatimTraitUnit, collapse = "; "), 
           verbatimLifeStage = paste(verbatimLifeStage, collapse = "; "), 
           verbatimNotes = paste(verbatimNotes, collapse = "; "),
           mergedTraitTaxonIDs = paste(traitTaxonID, collapse = "; ") ) %>% 
    # clean strings
    mutate(primaryReference = cleanStrings(primaryReference),
           primaryReferenceDOI = cleanStrings(primaryReferenceDOI),
           secondaryReference = cleanStrings(secondaryReference),
           secondaryReferenceDOI = cleanStrings(secondaryReferenceDOI),
           lifeStage = cleanStrings(lifeStage),
           sizeType = cleanStrings(sizeType), 
           # sizeAssocName = cleanStrings(sizeAssocName), 
           # sizeAssocUnit = cleanStrings(sizeAssocUnit), 
           # sizeAssocValue = cleanStrings(sizeAssocValue), 
           # sizeAssocReference = cleanStrings(sizeAssocReference), 
           location = cleanStrings(location), 
           # longitude = cleanStrings(longitude), 
           # latitude = cleanStrings(latitude), 
           notes = cleanStrings(notes),
           traitValueSource = cleanStrings(traitValueSource), 
           traitValueMeasurementNotes = cleanStrings(traitValueMeasurementNotes), 
           verbatimScientificName = cleanStrings(verbatimScientificName), 
           verbatimTraitName = cleanStrings(verbatimTraitName), 
           verbatimTraitValue = cleanStrings(verbatimTraitValue), 
           verbatimTraitUnit = cleanStrings(verbatimTraitUnit), 
           verbatimLifeStage = cleanStrings(verbatimLifeStage), 
           verbatimNotes = cleanStrings(verbatimNotes),
           mergedTraitTaxonIDs = cleanStrings(traitTaxonID) )%>% 
    distinct(mergedTraitTaxonIDs, .keep_all = TRUE) %>% 
    ungroup() %>% 
    # mutate(observationNumber = NA) %>% 
    updateIDs() %>% 
    mutate(notes = cleanStrings(paste("Trait value merged from multiple records; ", notes))) %>% 
    # update upload date
    mutate(uploadBy = "P. Pata", uploadDate = as.character(ymd(Sys.Date())))
  
  df <- bind_rows(df.same, df.updated) %>% 
    select(-dup)
}

# Function for selecting the average associated size for a trait value. If there
#   are multiple size types for a trait, the one with the highest sample size 
#   will be selected. References will also be summarized. Note that this may be 
#   a useful reference but it does not correspond to the species-average size 
#   value. For analyses requiring comparing the trait with size, it might be 
#   better to use the species-averaged size instead of the averaged sizeAssocValue.
summarise.sizeAssoc <- function(df) {
  A <- df %>% 
    select(traitID, taxonID,
           sizeAssocName, sizeAssocValue, sizeAssocUnit, sizeAssocReference) %>% 
    filter(!is.na(sizeAssocValue)) %>% 
    group_by(traitID, taxonID, sizeAssocName, sizeAssocUnit) %>% 
    # mutate(N = n(),
    #        Nmax = max(N)) %>% 
    # ungroup() %>% 
    # filter(N == Nmax) %>% 
    # group_by(traitID, taxonID, traitUnit) %>% 
    mutate(sizeAssocN = n(),
           sizeAssocSD = sd(sizeAssocValue, na.rm = TRUE),
           sizeAssocValue = mean(sizeAssocValue, na.rm = TRUE),
           sizeAssocReference = cleanStrings(paste(sizeAssocReference, 
                                                   collapse = "; "))) %>% 
    ungroup() %>% 
    distinct(traitID, taxonID, 
             sizeAssocName, sizeAssocValue, sizeAssocUnit, sizeAssocReference,
             sizeAssocN, sizeAssocSD) %>% 
    # arrange and get first instance
    group_by(traitID, taxonID) %>% 
    arrange(sizeAssocN) %>% 
    filter(row_number() == 1)
}

# Update the trait ID and unit when standardizing raw data
standardizeID <- function(data, trait.directory){
  col.order <- colnames(data)
  data <- data %>% 
    select(-c(traitID)) %>% 
    left_join(distinct(trait.directory, traitID, traitName), by = "traitName")  %>% 
    relocate(col.order)
}
standardizeUnit <- function(data, trait.directory){
  col.order <- colnames(data)
  data <- data %>% 
    select(-c(traitUnit)) %>% 
    left_join(distinct(trait.directory, traitName, traitUnit), by = "traitName")  %>% 
    relocate(col.order)
}

# For updating trait IDs and taxon-trait-IDs 
updateIDs <- function(data){
  col.order <- colnames(data)
  data <- data %>% 
    group_by(traitID,taxonID) %>% 
    mutate(maxObsNum = max(observationNumber)) %>% 
    mutate(maxObsNum = if_else(!is.na(maxObsNum), maxObsNum, 0)) %>% 
    # if there is no observation number, assign one
    mutate(observationNumber = if_else(!is.na(observationNumber),
                                       observationNumber, 
                                       maxObsNum + row_number())) %>% 
    mutate(maxObsNum = max(observationNumber)) %>% 
    # if there is no traitTaxonID, assign one
    mutate(traitTaxonID = if_else(is.na(traitTaxonID), 
                                  paste0(traitID,"-",taxonID,"-",observationNumber),
                                  traitTaxonID)) %>% 
    ungroup() %>% 
    relocate(col.order)
}

# Assign major group based on a row/s of taxonomy information
assignMajorGroup <- function(taxonomy){
  taxonomy <- taxonomy %>% 
    mutate(majorgroup = "") %>% 
    mutate(majorgroup = if_else(class %in% c("Polychaeta"),"Polychaete",majorgroup)) %>% 
    mutate(majorgroup = if_else(phylum %in% c("Chaetognatha"),"Chaetognath",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Branchiopoda"),"Cladoceran",majorgroup)) %>% 
    mutate(majorgroup = if_else(phylum %in% c("Ctenophora"),"Ctenophore",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Scyphozoa"),"Scyphomedusae",majorgroup)) %>%  
    mutate(majorgroup = if_else(order %in% c("Siphonophorae"),"Siphonophore",majorgroup)) %>%  
    mutate(majorgroup = if_else(order %in% c("Narcomedusae","Leptothecata",
                                             "Trachymedusae","Limnomedusae",
                                             "Anthoathecata"),
                                "Hydromedusae",majorgroup)) %>% 
    mutate(majorgroup = if_else(order %in% c("Pteropoda"),"Pteropod",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Thaliacea"), "Thaliacean",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Appendicularia"),
                                "Appendicularian",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Ostracoda"), "Ostracod",majorgroup)) %>% 
    mutate(majorgroup = if_else(class %in% c("Copepoda"),"Non-calanoid",majorgroup)) %>% 
    mutate(majorgroup = if_else(order %in% c("Calanoida"),"Calanoid",majorgroup)) %>% 
    mutate(majorgroup = if_else(order %in% c("Amphipoda"),"Amphipod",majorgroup)) %>% 
    mutate(majorgroup = if_else(order %in% c("Decapoda"),"Decapod",majorgroup)) %>% 
    mutate(majorgroup = if_else(order %in% c("Euphausiacea"),"Euphausiid",majorgroup)) %>%
    mutate(majorgroup = if_else(order %in% c("Mysida","Mysidacea"),
                                "Mysid",majorgroup)) 
  return(taxonomy$majorgroup)
}

# For updating the maximum observation number of a trait and taxon.
# This is important when modifying trait records.
getMaxObsNum <- function(tN, tI, df) {
  df <- df %>% 
    filter(traitName == tN & taxonID == tI)
  if (nrow(df) > 0) {
    i <- max(df$observationNumber)
  } else {
    i <- 0
  }
  i
}


# For standardizing individual trait files 
standardizeTable <- function(file.list, s.format, taxonomy, lifestagelist,
                             trait.directory, rev.by = "") {
  data <- s.format
  for(i in file.list) {
    d <- openxlsx::read.xlsx(paste0(infol,i)) %>% 
      mutate(secondaryReference = as.character(secondaryReference))
    if("traitUnit" %notin% colnames(d)){
      d <- d %>% 
        mutate(traitUnit = NA)
    }
    if("verbatimTraitUnit" %notin% colnames(d)){
      d <- d %>% 
        mutate(verbatimTraitUnit = NA)
    }
    d <- d %>%  
      mutate(traitValue = as.character(traitValue),
             verbatimTraitValue = as.character(verbatimTraitValue)) %>% 
      # set traitID, traitName based on trait directory
      select(-c(traitName, traitUnit)) %>%
      left_join(select(trait.directory, traitID, traitName, 
                       verbatimTraitName, verbatimTraitUnit),
                by = c("verbatimTraitName","verbatimTraitUnit"))
    
    if("traitValueN" %in% colnames(d)) {
      d$traitValueN <- as.numeric(str_trim(d$traitValueN))
    }
    if("traitValueTemperature" %in% colnames(d)) {
      d$traitValueTemperature <- as.character(d$traitValueTemperature)
    }
    if("lifeStage" %in% colnames(d)) {
      d$lifeStage <- as.character(d$lifeStage)
    }
    if("location" %in% colnames(d)) {
      d$location <- as.character(d$location)
    }
    if("notes" %in% colnames(d)) {
      d$notes <- as.character(d$notes)
    }
    data <- data %>% 
      bind_rows(d)
  }
  # Annotate taxonomy information
  data <- annotateTaxonomy(data, taxonomy) %>% 
    # Set stage ID
    annotateLifeStage(lifestagelist) %>% 
    # Record file generation time stamp
    mutate(uploadBy = rev.by, uploadDate = ymd(Sys.Date())) %>% 
    # Remove not in mesozooplankton major groups of interest
    filter(!is.na(majorgroup)) %>% 
    # assign traittaxonID
    group_by(traitID,taxonID) %>% 
    mutate(observationNumber = row_number()) %>% 
    mutate(traitTaxonID = paste0(traitID,"-",taxonID,"-",observationNumber)) %>% 
    mutate(maxObsNum = max(observationNumber)) %>% 
    ungroup() %>% 
    # trait unit = verbatim trait unit if empty
    mutate(traitUnit = if_else(is.na(traitUnit), verbatimTraitUnit, traitUnit)) %>% 
    # assign trait value source
    mutate(traitValueSource = "literature") %>% 
    relocate(colnames(s.format))
}


# for merging duplicated trait values
mergeTraitDetails <- function(df, trait.directory, rev.by = "") {
  df <- df %>% 
    mutate(dup = n())
  
  df.same <- df %>% 
    filter(dup == 1) %>% 
    mutate(isMerged = FALSE) %>% 
    ungroup()
  
  df.updated <- df %>% 
    filter(dup > 1) %>% 
    mutate(isMerged = TRUE)  %>% 
    mutate(primaryReference = paste(primaryReference, collapse = "; "),
           primaryReferenceDOI = paste(primaryReferenceDOI, collapse = "; "),
           secondaryReference = paste(secondaryReference, collapse = "; "),
           secondaryReferenceDOI = paste(secondaryReferenceDOI, collapse = "; "),
           lifeStage = paste(lifeStage, collapse = "; "),
           sizeType = paste(sizeType, collapse = "; "), 
           # sizeAssocName = paste(sizeAssocName, collapse = "; "), 
           # sizeAssocUnit = paste(sizeAssocUnit, collapse = "; "), 
           # sizeAssocValue = paste(sizeAssocValue, collapse = "; "), 
           # sizeAssocReference = paste(sizeAssocReference, collapse = "; "), 
           location = paste(location, collapse = "; "), 
           # longitude = paste(longitude, collapse = "; "), 
           # latitude = paste(latitude, collapse = "; "), 
           notes = paste(notes, collapse = "; "),
           traitValueSource = paste(traitValueSource, collapse = "; "), 
           traitValueMeasurementNotes = paste(traitValueMeasurementNotes, collapse = "; "), 
           verbatimScientificName = paste(verbatimScientificName, collapse = "; "), 
           verbatimTraitName = paste(verbatimTraitName, collapse = "; "), 
           verbatimTraitValue = paste(verbatimTraitValue), 
           verbatimTraitUnit = paste(verbatimTraitUnit, collapse = "; "), 
           verbatimLifeStage = paste(verbatimLifeStage, collapse = "; "), 
           verbatimNotes = paste(verbatimNotes, collapse = "; "),
           mergedTraitTaxonIDs = paste(traitTaxonID, collapse = "; ") ) %>% 
    # clean strings
    mutate(primaryReference = cleanStrings(primaryReference),
           primaryReferenceDOI = cleanStrings(primaryReferenceDOI),
           secondaryReference = cleanStrings(secondaryReference),
           secondaryReferenceDOI = cleanStrings(secondaryReferenceDOI),
           lifeStage = cleanStrings(lifeStage),
           sizeType = cleanStrings(sizeType), 
           # sizeAssocName = cleanStrings(sizeAssocName), 
           # sizeAssocUnit = cleanStrings(sizeAssocUnit), 
           # sizeAssocValue = cleanStrings(sizeAssocValue), 
           # sizeAssocReference = cleanStrings(sizeAssocReference), 
           location = cleanStrings(location), 
           # longitude = cleanStrings(longitude), 
           # latitude = cleanStrings(latitude), 
           notes = cleanStrings(notes),
           traitValueSource = cleanStrings(traitValueSource), 
           traitValueMeasurementNotes = cleanStrings(traitValueMeasurementNotes), 
           verbatimScientificName = cleanStrings(verbatimScientificName), 
           verbatimTraitName = cleanStrings(verbatimTraitName), 
           verbatimTraitValue = cleanStrings(verbatimTraitValue), 
           verbatimTraitUnit = cleanStrings(verbatimTraitUnit), 
           verbatimLifeStage = cleanStrings(verbatimLifeStage), 
           verbatimNotes = cleanStrings(verbatimNotes),
           mergedTraitTaxonIDs = cleanStrings(traitTaxonID) )%>% 
    distinct(mergedTraitTaxonIDs, .keep_all = TRUE) %>% 
    ungroup() %>% 
    updateIDs() %>% 
    mutate(notes = cleanStrings(paste("Trait value merged from multiple records; ", notes))) %>% 
    # update upload date
    mutate(uploadBy = rev.by, uploadDate = as.character(ymd(Sys.Date())))
  
  df <- bind_rows(df.same, df.updated) %>% 
    select(-dup)
}

annotateLifeStage <- function(data, lifestagelist) {
  data <- data %>% 
    mutate(verbatimLifeStage = lifeStage) %>% 
    select(-c(lifeStage, stageID)) %>% 
    left_join(lifestagelist, by = "verbatimLifeStage")
}

annotateTaxonomy <- function(data, taxonomy) {
  data <- data %>% 
    select(-c(taxonID, taxonRankID, scientificName, aphiaID, aphiaAuthority,
              rank, kingdom, phylum, class, order, family, genus, majorgroup)) %>% 
    mutate(vsn_ed = cleanScientificName(verbatimScientificName)) %>% 
    left_join(select(taxonomy, verbatimScientificName, scientificName, 
                     taxonID, taxonRankID, aphiaID, 
                     aphiaAuthority = valid_authority,
                     rank, kingdom, phylum, class, order, family, genus, 
                     majorgroup), 
              by = c("vsn_ed" = "verbatimScientificName")) %>%
    select(-vsn_ed)
}

# Widens a table for listed traits. Column names do not change and SD is excluded.
widen_traits <- function(trait.table, trait.list) {
  trait.table %>% 
    filter(traitName %in% trait.list) %>% 
    dplyr::select(taxonID, scientificName, majorgroup, aphiaID, 
                  traitName, traitValue) %>% 
    pivot_wider(names_from = traitName, values_from = traitValue) %>% 
    relocate(all_of(trait.list)) 
}

getMaxObsNum <- function(traitName, taxonID, df) {
  df <- df %>% 
    filter(traitName == traitName & taxonID == taxonID)
  if (nrow(df) > 0) {
    i <- max(df$observationNumber)
  } else {
    i <- 0
  }
  i
}


# Calculate the weighted means and SDs of traits when the dataset is a mix of 
#   individual values and averaged records.
getSpeciesMeanSD <- function(traitValue, traitValueSD, traitValueN){
  # extract indiv values and grouped values
  val.indiv <- traitValue[which(traitValueN == 1)]
  val.group.mean <- traitValue[which(traitValueN > 1)]
  val.group.N <- traitValueN[which(traitValueN > 1)]
  val.group.sd <- traitValueSD[which(traitValueN > 1)]
  # calculate mean of the individual values
  val.indiv.mean <- mean(val.indiv)
  val.indiv.N <- length(val.indiv)
  val.indiv.sd <- sd(val.indiv, na.rm = TRUE)
  
  # calculate N
  N <- sum(traitValueN)
  # calculate weighted mean
  mean <- weighted.mean(c(val.indiv.mean, val.group.mean),
                        c(val.indiv.N, val.group.N))
  # calculate pooled sd
  # catch if val.indiv.N = 1, calculate the pooled sd from the groups vars only, 
  #  note that the mean and N are of all though. This is obviously not accurate 
  #  but would be a better estimate compared to calculating the unweighted sd, 
  #  especially when the group.N sums to a large sample.
  if (val.indiv.N == 1) {
    sd <- pooled.sd(c(val.group.sd),
                    c(val.group.N))
  } else {
    sd <- pooled.sd(c(val.indiv.sd, val.group.sd),
                    c(val.indiv.N, val.group.N))
  }
  # calculate standard error
  se <- sd / sqrt(N)
  
  
  return(list("N" = N, "mean" = mean, "sd" = sd, "se" = se))
}



# ----- Data imputation functions -----

# This function calculates the mean, standard deviation, and number of observations 
#   for generalization of a given trait for a taxon. This will not generate an 
#   updated traitTaxonID and this should created if including the generalized trait 
#   observation in the overall trait table.
getGroupLevelValue <- function(taxon, trait, gen.level = "genus", trait.df, taxonomy.df){
  # check if trait is continuous or binary
  trait.type <- trait.df %>% 
    filter(traitName == trait) %>% 
    filter(row_number()==1) %>% 
    select(dataType)
  
  if (trait.type$dataType %notin% c("continuous","binary")) {
    stop("Error: Please select a trait that is either continuous or binary.")
  }
  
  # get taxonomy details of a taxon
  taxon.details <- taxonomy.df %>% 
    filter(scientificName == taxon)
  
  if (nrow(taxon.details) == 0) {
    stop("Error: Please selec a scientific name that is found in the taxonomy data frame.")
  }
  
  # get a list of species that are in the focus taxon's group
  if (gen.level == "genus"){
    taxon.group <- taxonomy.df %>% 
      filter(genus == taxon.details$genus)
  } else if (gen.level == "family") {
    taxon.group <- taxonomy.df %>% 
      filter(family == taxon.details$family)
  } else if (gen.level == "order") {
    taxon.group <- taxonomy.df %>% 
      filter(order == taxon.details$order)
  } else {
    stop("Error: Please select a generalization level that is either genus, family, or order.")
  }
  
  taxon.group <- taxon.group %>%  
    filter(taxonRankID > 180) %>% 
    distinct(taxonID, scientificName)
  
  # filter the trait dataframe to select the trait and the species of interest
  trait.sub <- trait.df %>% 
    filter(traitName == trait) %>% 
    filter(taxonID %in% taxon.group$taxonID)
  
  # If there are no other taxa found for this species, return with a blank and provide a warning that there was no generalization.
  if(nrow(trait.sub) == 0){
    warning("Warning: There are no records of the selected trait at the selected group level.")
    return(NA)
  }
  
  # Calculate the mean and standard deviation across all members in a group and return a row of generalized trait value following the standardized format. 
  generalized.trait <- trait.sub %>% 
    mutate(traitValue = as.numeric(traitValue)) %>% 
    mutate(traitValue = mean(traitValue), 
           traitValueSD = sd(traitValue, na.rm = TRUE), traitValueN = n(),
           traitValueSource = "generalized", 
           notes = paste0("Trait value generalized from the ",gen.level," level average.")) %>% 
    distinct(traitID, traitName, traitValue, traitUnit, dataType,
             traitValueTemperature, traitValueSD, traitValueN, 
             notes, traitValueSource) %>% 
    bind_cols(select(taxon.details, -c(verbatimScientificName)))
  
  return(generalized.trait)
}


get_pairwise.N <- function(vec_a, vec_b) {
  sum(!is.na(vec_a) & !is.na(vec_b))
}

correlate_traits <- function(trait.table, trait.list,
                             logtransX = FALSE, logtransY = FALSE) {
  A <- widen_traits(trait.table, trait.list) %>% 
    # filter(!is.na(get(trait.list[1])) & !is.na(get(trait.list[2]))) %>% 
    dplyr::select(all_of(trait.list)) 
  
  # If log10 transform the X or Y variable
  if (logtransX == TRUE) {
    A[,1] = log10(A[,1])
  }
  if (logtransY == TRUE) {
    A[,2] = log10(A[,2])
  }
  
  # with the corrr::correlate() function, multiple traits can be analyzed with missing points. This returns a correlation matrix which can be stretched into a long data frame.
  # Correlate, get p-value, and N
  B <- corrr::correlate(A, use = "pairwise.complete.obs", 
                        method = "pearson", quiet = TRUE) %>% 
    corrr::stretch(na.rm = TRUE, remove.dups = TRUE) 
  
  C <- corrr::colpair_map(A, get_pairwise.N) %>% 
    corrr::stretch(na.rm = TRUE, remove.dups = TRUE) %>% 
    rename(N = r)
  
  D <- corrr::colpair_map(A, calc_p_value) %>%  
    corrr::stretch(na.rm = TRUE, remove.dups = TRUE) %>% 
    rename(pval = r)
  
  E <- left_join(B,C, by = c("x","y")) %>% 
    left_join(D, by = c("x","y")) %>% 
    arrange(-r)
  
  return(E)
}

# general equation for allometric conversion, base defaults to natural log
conv.allom <- function(W,a,b,base = exp(1)) {
  base^(a + (b*log(W,base)))
}

# Calculate p value from an F statistic object
getpval <- function(x){ 
  p <- pf(x[1],x[2],x[3],lower.tail = F) 
  attributes(p) <- NULL
  return(p)
}

# Derives a specified regression model.
# df is a standardized trait dataframe
getRegressionModel <- function(df, grp = "All", X, Y, 
                               model = "OLS", base = "10"){
  trait.sub <- df %>% 
    filter(traitName %in% c(all_of(X), all_of(Y)))
  if (grp != "All") {
    trait.sub <- trait.sub %>% 
      filter(str_detect(group, grp)) 
  }
  trait.sub <- trait.sub %>% 
    filter(rank %in% c("Subspecies","Species")) %>% 
    select(taxonID, scientificName, traitName, traitValue, majorgroup) %>% 
    pivot_wider(names_from = traitName, values_from = traitValue) %>% 
    filter(!is.na(get(X)) & !is.na(get(Y))) %>% 
    relocate(X, Y)
  
  # This calculates both the OLS and RMA
  if(base == "10") {
    reg <- lmodel2(log10(get(Y)) ~ log10(get(X)), data = trait.sub)
  } else if (base == "e") {
    reg <- lmodel2(log(get(Y)) ~ log(get(X)), data = trait.sub)
  } else {
    stop("Error: Please select either base 10 or e.")
  }
  
  
  if (model == "OLS"){
    ii <- 1
  } else if (model =="RMA") {
    ii <- 3
  } else {
    stop("Error: Please select either OLS or RMA regression model.")
  }
  
  # return(reg)
  
  reg.results.size <- data.frame(grp = grp, X = X,  Y = Y, 
                                 a = reg$regression.results$Intercept[ii], 
                                 b = reg$regression.results$Slope[ii], 
                                 a.ci.2.5 = reg$confidence.intervals$`2.5%-Intercept`[ii],
                                 a.ci.97.5 = reg$confidence.intervals$`97.5%-Intercept`[ii],
                                 b.ci.2.5 = reg$confidence.intervals$`2.5%-Slope`[ii],
                                 b.ci.97.5 = reg$confidence.intervals$`97.5%-Slope`[ii],
                                 n = nrow(trait.sub), 
                                 R2 = reg$rsquare,
                                 pval = reg$P.param, 
                                 model = model, base = as.character(base),
                                 minX = min(trait.sub[,1]), maxX = max(trait.sub[,1]))
}

# calculate values based on an allometric conversion model
calculateFromModel <- function(df, model, trait.directory, excludeWithLit = TRUE,
                               applyToGeneralized = FALSE,
                               excludeCalculated = TRUE) {
  # make sure this is just one model
  stopifnot(nrow(model) == 1)
  
  # error if the base of the logarithm is not 10 or e (natural log).
  if (!(model$base == "10" | model$base == "e" | model$base == "ln")) {
    stop("Error: Please select model with either base 10 or e.")
  }
  
  df.withdata <- df %>% 
    filter(str_detect(traitValueSource, "literature|derived")) %>% 
    filter(traitName %in% model$Y)
  
  df.calc <- df %>% 
    # Assign the verbatim trait information as the calculated values
    mutate(verbatimTraitName = traitName, verbatimTraitUnit = traitUnit,
           verbatimTraitValue = as.character(traitValue), verbatimNotes = notes) %>% 
    select(-c(primaryReferenceDOI, secondaryReferenceDOI, traitTaxonID, 
              sizeAssocName, sizeAssocUnit, sizeAssocValue,
              sizeAssocN, sizeAssocSD, traitValueTemperature,
              sizeAssocReference, location, longitude, latitude, notes,
              isMerged, mergedTraitTaxonIDs, traitValueMeasurementNotes)) %>%  
    filter(str_detect(group, model$grp),
           traitName %in% model$X) %>% 
    filter(traitValue >= model$minX,
           traitValue <= model$maxX)
  
  # If the predictor is size trait, assign it as sizeAssoc
  if (model$X %in% c("bodyLengthMax", "carbonWeight", "dryWeight", "wetWeight")){
    df.calc <- df.calc %>%
      mutate(sizeAssocName = traitName,
             sizeAssocUnit = traitUnit, sizeAssocValue = traitValue,
             sizeAssocSD = traitValueSD, sizeAssocN = traitValueN,
             sizeAssocReference = if_else(!is.na(secondaryReference),
                                          secondaryReference, primaryReference))
  }
  # If y variable is a rate, assign temperature to defaul of 15C, if this is
  #   not the case, need to revise externally.
  if (grepl("_15C",model$Y)){
    df.calc <- df.calc %>% 
      mutate(traitValueTemperature = 15)
  }
  
  if(excludeCalculated == TRUE) {
    df.calc <- df.calc %>% 
      filter(!(traitValueSource == "calculated"))
  }
  df.calc <- df.calc %>% 
    dplyr::select(-c(primaryReference, secondaryReference, traitValueSD, traitValueN))
  
  if(applyToGeneralized == TRUE) {
    df.calc <- df.calc %>% 
      # apply only genus level generalization if for level 3
      filter(!(traitValueSource == "generalized" & valueRank != "Genus"))
  } else {
    df.calc <- df.calc %>% 
      filter(!(traitValueSource == "generalized"))
  }
  
  # calculate for traits - note that traits here are log-transformed to a 
  #  specific base. None of the models involve percent or ratio traits, and 
  #  those would need a separate transformation if ever.
  if (model$base == "10") {
    df.calc <- df.calc %>% 
      mutate(traitName = model$Y,
             traitValue =  conv.allom(traitValue, model$a, model$b, base = 10))
  } else if (model$base == "e" | model$base == "ln") {
    mutate(traitName = model$Y,
           traitValue =  conv.allom(traitValue, model$a, model$b))
  }
  
  df.calc <- df.calc %>% 
    mutate(traitValueSource = "calculated",
           notes = paste0("Value calculated from ",model$X, " using the equation: ",
                          "y = ",model$base,"^(",sprintf("%.3f",model$a),
                          "+(",sprintf("%.3f",model$b),
                          "*log(x,",model$base,")))."),
           uploadBy = "P.Pata", 
           uploadDate = as.character(ymd(Sys.Date()))) %>% 
    
    group_by(taxonID) %>% 
    mutate(maxObsNum = getMaxObsNum(traitName, taxonID, df)) %>% 
    ungroup() %>% 
    # If rate, note that traitValueTemperature was standardized to 15C
    mutate(traitValueTemperature = if_else(str_detect(traitName,"Rate"),
                                           15, -999),
           traitValueTemperature = na_if(traitValueTemperature, -999),
           traitValueTemperature = as.character(traitValueTemperature))
  
  # exclude taxa which already have literature/derived trait information
  if(excludeWithLit == TRUE){
    df.calc <- df.calc %>% 
      filter(taxonID %notin% df.withdata$taxonID)
  }
  
  df.calc <- df.calc %>% 
    # update units and traitTaxonIDs
    standardizeID(trait.directory) %>% 
    standardizeUnit(trait.directory) %>% 
    group_by(traitID,taxonID) %>% 
    mutate(observationNumber = maxObsNum + row_number()) %>% 
    mutate(maxObsNum = max(observationNumber)) %>% 
    mutate(traitTaxonID = paste0(traitID,"-",taxonID,"-",observationNumber)) %>% 
    ungroup()
}

plotAllometric <- function(df, grp, X, Y, base = "10") {
  trait.sub <- df %>% 
    filter(traitName %in% c(X,Y)) %>% 
    filter(str_detect(group, grp)) %>% 
    filter(rank %in% c("Subspecies","Species")) %>% 
    select(taxonID, scientificName, traitName, traitValue, majorgroup) %>% 
    pivot_wider(names_from = traitName, values_from = traitValue) %>% 
    filter(!is.na(get(X)) & !is.na(get(Y))) 
  
  if (base == "10") {
    base.trans <-  "log10"
  } else if(base == "e") {
    base.trans <-  "log"
  } else {
    stop("Error: Please select the axis scales to either be log10 or ln scaled.")
  }
  
  ggplot(trait.sub, aes(x = get(X), y = get(Y))) +
    geom_point(aes(color = majorgroup, text = scientificName)) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_x_continuous(labels = scaleFUN, trans = base.trans) +
    scale_y_continuous(labels = scaleFUN, trans = base.trans) +
    xlab(X) + ylab(Y) +
    theme_bw() + 
    stat_regline_equation(label.y = 1) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
             label.y = 1.5) 
}

# Plot regression line with confidence intervals - this only plots the function 
#  but not the data points
plotRegModel <- function(model){
  model <- model[1,]
  
  if (model$base == "10") {
    base.trans <-  "log10"
    base.allom <- 10
  } else if(model$base == "e") {
    base.trans <-  "log"
    base.allom <- exp(1)
  } else {
    stop("Error: Please select the axis scales to either be log10 or ln scaled.")
  }
  
  df <- data.frame( x = seq(model$minX, model$maxX, len = 100) ) %>% 
    mutate(y = conv.allom(x, model$a, model$b, base = base.allom),
           y.lower = conv.allom(x, model$a.ci.2.5, model$b.ci.2.5, base = base.allom),
           y.upper = conv.allom(x, model$a.ci.97.5, model$b.ci.97.5, base = base.allom))
  
  g <- ggplot(df, aes(x, y)) +
    geom_line() + 
    geom_line(aes(y = y.lower), lty = 2, color = "gray") +
    geom_line(aes(y = y.upper), lty = 2, color = "gray") +
    theme_bw() +
    xlab(model$X) + ylab(model$Y) +
    scale_x_continuous(labels = scaleFUN, trans = base.trans) +
    scale_y_continuous(labels = scaleFUN, trans = base.trans)
  
  return(g)
}


# Updated February 13, 2023 - uses the associated size value
calculate.WSRates <- function(traits.calculated, trait.X, trait.Y) {
  # Prepare for updates in traitName and traitUnit
  if (trait.X == "carbonWeight") {
    name.suffix <- "_WSC_15C"
    unit.suffix <- "mg C^-1 h"
  } else if (trait.X == "dryWeight") {
    name.suffix <- "_WSDW_15C"
    unit.suffix <- "mg^-1 h"
  } else if (trait.X == "wetWeight") {
    name.suffix <- "_WSWW_15C"
    unit.suffix <- "mg^-1 h"
  } else {
    stop("Error: Please select either carbonWeight, dryWeight, or wetWeight.")
  }
  
  traits.calculated <- traits.calculated %>% 
    filter(traitName %in% trait.Y) %>% 
    # Assign the verbatim trait information as the calculated values
    mutate(verbatimTraitName = traitName, verbatimTraitUnit = traitUnit,
           verbatimTraitValue = traitValue, verbatimNotes = notes,
           traitValue = as.numeric(traitValue),
           sizeAssocValue = as.numeric(sizeAssocValue)) %>% 
    group_by(traitTaxonID) %>% 
    filter(!is.na(traitValue) & !is.na(sizeAssocValue)) %>% 
    mutate(traitValue = traitValue / sizeAssocValue,
           traitName = str_replace(traitName,"_15C", name.suffix),
           traitUnit = str_replace(traitUnit,"h", unit.suffix),
           notes = "Calculated from per individual rate and associated size value.") %>% 
    standardizeID(trait.directory) %>% 
    mutate(traitValue = as.character(traitValue))
}
