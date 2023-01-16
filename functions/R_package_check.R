# R packages check and download
# Created by P. Pata
# Last modified January 12, 2023
#
# This checks and loads the local library if the required packages for this 
# R project are present. If not, the packages are downloaded from CRAN.
# 
# prepareLibrary() function calls the package.check scripts and loads all 
# required libraries.

packages <-
  c("tidyverse",
    "vegan",
    "lubridate",
    "lmodel2",
    "openxlsx",
    "here",
    "knitr",
    "PBSmapping",
    "cowplot",
    "viridis",
    "ggbreak", #cite: 10.3389/fgene.2021.774846
    "Polychrome")

package.check <- lapply(
  packages,
  FUN = function(x)
  {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dependencies = TRUE,
                       repos = "http://cran.us.r-project.org")
      library(x, character.only = TRUE)
    }
  }
)
rm("package.check")

prepareLibrary <- function(){
  package.check <- lapply(
    packages,
    FUN = function(x)
    {
      if (!require(x, character.only = TRUE))
      {
        install.packages(x, dependencies = TRUE,
                         repos = "http://cran.us.r-project.org")
        library(x, character.only = TRUE)
      }
    }
  )
  
}
