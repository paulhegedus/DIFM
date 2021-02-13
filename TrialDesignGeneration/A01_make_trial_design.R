######################################
# Process all the data and create reports
######################################
# objectives:
# 1.

# /*=================================================*/
#' # Preparation
# /*=================================================*/
#--- load all the packages ---#
library(sf)
library(rmarkdown)
library(data.table)
library(bookdown)
library(knitr)
library(parallel)
library(here)
library(tmap)
library(flextable)
library(officer)
library(stringr)
library(jsonlite)
library(tidyverse)

#--- set working directory ---#

# /*----------------------------------*/
#' ## source all the functions
# /*----------------------------------*/
#--- local ---#
list.files(here("Codes/Functions"), full.names = TRUE) %>% 
  .[str_detect(., "functions")] %>% 
  lapply(., function(x) source(x))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

# /*----------------------------------*/
#' ## Load the field parameter data
# /*----------------------------------*/
field_data <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "field_parameter.json"
  ),
  flatten = TRUE
) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")]

#--- get the field-year combinations ---#
field_year_ls <- field_data$field_year

#/*=================================================*/
#' # Make trial designs
#/*=================================================*/

ffy <- field_year_ls[3]

make_trial_design(ffy, rerun = TRUE)

#--- force headland to be 100 feet ---#
make_trial_design(ffy, head_dist = 100, rerun = TRUE)



