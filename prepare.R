#--- load all the packages ---#
cat("Loading some generic packages.\n\n")

library(here)
library(rmarkdown)
library(sf)
library(data.table)
library(bookdown)
library(knitr)
library(parallel)
library(tmap)
library(stringr)
library(jsonlite)
library(tidyverse)

#--- source functions from github accounts ---#
cat("Sourcing functions from github repositories.\n\n")

source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")
source("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/Functions/functions_for_analysis.R?raw=TRUE")
source("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/Functions/functions_for_execution.R?raw=TRUE")
source("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/Functions/functions_for_organization.R?raw=TRUE")
source("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/Functions/functions_for_processing.R?raw=TRUE")
source("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/Functions/functions_for_trial_design.R?raw=TRUE")

