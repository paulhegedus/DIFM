#' ---
#' title: "Report on DIFM Field Trial \n field-year-here"
#' output:
#'  bookdown::word_document2:
#'    toc: false
#'    number_sections: false
#'    reference_docx: "/Users/tmieno2/Box/DIFM_Central/Reports/Growers/word_template.docx"
#'  bookdown::html_document2:
#'    toc: true
#'    toc_depth: 2
#'    toc_float:
#'      collapsed: true
#'    number_sections: true
#'---

#+ report-set-up, cache = F, echo = F, results = "hide"
library(knitr)
library(here)
knitr::opts_chunk$set(
  cache = TRUE,
  echo = FALSE,
  warning = FALSE,
  cache.lazy = FALSE,
  fig.retina = 6,
  fig.height = 9,
  fig.width = 9,
  message = FALSE
)

#+ a02-pacakages, cache = FALSE

library(sf)
library(ggplot2)
library(tmap)
library(ggcorrplot)
library(patchwork)
library(flextable)
library(officer)
library(parallel)
library(tidyverse)
library(corrplot)
library(data.table)
library(GWmodel)

#===================================
# Set thing up for reporting
#===================================
#+ results = "hide"

#--- working field-year ---#
ffy <- "field-year-here"

#--- root directory ---#
opts_knit$set(root.dir = here("Data", "Growers", ffy))
# setwd(here("Data", ffy))

#--- source functions ---#
source(
  "https://github.com/tmieno2/DIFM/blob/master/Functions/prepare.R?raw=TRUE",
  local = TRUE
)

#--- make pdf output smaller in size ---#
pdf.options(useDingbats = TRUE)

#--- field parameters ---#
source(
  get_r_file_name_git("Functions/unpack_field_parameters.R"),
  local = TRUE
)

#--- currently we do not have this as part of the field parameter data ---#
plot_width <- 60


#+ map-layout, cache = TRUE

tm_layout_to_add <- tm_layout(
  frame = FALSE,
  legend.outside = TRUE, 
  legend.title.size = 1,
  legend.text.size = 0.8,
  fontfamily = "Times"
)

#===================================
# Read in datasets for visualization 
#===================================
analysis_data <- here("Data/Growers", ffy, "Analysis-Ready/analysis_data.rds") %>% 
  readRDS()

trial_design <- here("Data/Growers", ffy, "TrialDesign/trial-design.shp") %>% 
  st_read() %>% 
  setnames(names(.), tolower(names(.))) %>% 
  st_transform_utm()

if (trial_type == "SN"){
  as_applied_s <- readRDS(here("Data/Growers", ffy, "Intermediate/as_applied_s.rds"))

  as_applied_n <- readRDS(here("Data/Growers", ffy, "Intermediate/as_applied_n.rds"))

} else if (trial_type == "S") {

  as_applied_s <- readRDS(here("Data/Growers", ffy, "Intermediate/as_applied_s.rds"))

} else if (trial_type == "N") {

  as_applied_n <- readRDS(here("Data/Growers", ffy, "Intermediate/as_applied_n.rds"))
}

yield_polygons <- readRDS(here("Data/Growers", ffy, "Intermediate/yield_polygons.rds"))

