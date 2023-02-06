# Quality of life tool kit
# Created by P. Pata
# Last modified January 10, 2023
#
# This file contains some functions that are useful in day to day data analysis.
# 
#
# geomean(x): Calculates the geometric mean of an array.
# scaleFun(x): Returns a number with two decimal places.
# standard_temp(rate0, t0, t, Q10): Calculate the value of a rate to a reference
#   temperature using a Q10 coefficient. Default Q10 is 2.8 but be mindful of
#   the appropriate Q10 value specific to the data.
# cleanStrings(A): Cleans spaces, NAs, and blanks from strings for notes and 
#   references. Also removes duplicated strings separated by ;.
# cleanStringsList(A): Apply cleanStrings() to a list.
# cleanScientificName(name): Cleans the verbatim scientific name from trailing 
#   spaces and life stage information. Also removes the sp, aff, and cf texts.

require(tidyverse)

`%notin%` <- Negate(`%in%`)

geomean <- function(x) {  exp(mean(log(x), na.rm = TRUE))  }

scaleFUN <- function(x) sprintf("%.2f", x)

standard_temp <- function(rate0, t0, t = 15, Q10 = 2.8) {
  10^(log10(rate0) + log10(Q10)*((t-t0)/10) )
}

# For cleaning concatenated strings
cleanStrings <- function(A){
  A <- str_split(A, pattern =";", simplify = TRUE) 
  A <- str_trim(A)
  A <- sort(A)
  A <- str_replace(A, "NA", replacement = "")
  A <- A[A != '']
  A <- unique(as.list(A))
  A <- paste(A, collapse = "; ")
  A <- as.character(A)
}

cleanStringsList <- function(A){
  A <- lapply(A, cleanStrings)
}

cleanScientificName <- function(name){
  # remove trailing *sp., ?-. symbols, and anything inside parenthesis
  # remove trailing life stage information
  name <- gsub(" aff\\.","", name)
  name <- gsub(" cf\\.","", name)
  name <- gsub(" C4| C5| CI| CIV| CV| III| IV| VI| NI| NII| NIII| NIV| NV","",name)
  name <- str_replace(name, "\\s*\\([^\\)]+\\)", "")
  name <- str_replace_all(name, "[[:punct:]]", "")
  name <- str_replace_all(name, "[:digit:]", "")
  name <- gsub(" sp$","", name)
  name <- gsub(" spp$","", name)
  name <- gsub(" V$","", name)
  name <- str_replace(name, " female","")
  name <- str_replace(name, " male","")
  name <- str_replace(name, "Agg","")
  name <- str_replace(name, "aggregate","")
  name <- str_replace(name, "solitary","")
  name <- str_squish(str_trim(name))
  name
}
