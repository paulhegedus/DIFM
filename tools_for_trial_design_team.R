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
    # "field_parameter.json"
    "fp_2021_TD.json"
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

ffy <- field_year_ls[8]

get_td_parameters(ffy, "fp_2021_TD.json")

#--- force headland to be 100 feet ---#

make_trial_design(
  ffy, 
  json_file = "fp_2021_TD.json", 
  rates = list(
    #=== include the gc_rate ===#
    c(28, 32, 35, 38),
    c(10, 17, 23.43, 29)
  ), 
  head_dist = c(120, 120), 
  use_ab = TRUE,
  rerun = TRUE
)




