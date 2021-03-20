######################################
# R codes to assist Trial Design Coordinators
######################################

# /*=================================================*/
#' # Preparation
# /*=================================================*/
#/*----------------------------------*/
#' ## Load packages and source functions
#/*----------------------------------*/
source(
  "https://github.com/tmieno2/DIFM/blob/master/Functions/prepare.R?raw=TRUE",
  local = TRUE
)

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
.[, field_year := paste(farm, field, year, sep = "_")] %>% 
.[year == 2021, ]

field_year_ls <- field_data$field_year

#/*=================================================*/
#' # Make trial designs
#/*=================================================*/

ffy <- field_year_ls[32]

make_trial_design(ffy, rerun = TRUE)

#--- force headland to be 100 feet ---#
make_trial_design(
  ffy, 
  plot_width = 20,
  gc_rate = 120, 
  head_dist = 100, 
  use_ab = TRUE,
  rerun = TRUE
)




