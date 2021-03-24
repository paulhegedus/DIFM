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
field_data <- 
jsonlite::fromJSON(
  here("Data", "CommonData", "fp_2021_TD.json"),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")] %>% 
.[year == 2021, ]

field_year_ls <- field_data$field_year

#/*=================================================*/
#' # Make trial designs
#/*=================================================*/

ffy <- field_year_ls[2]

get_td_parameters(ffy, "fp_2021_TD.json")

#--- force headland to be 100 feet ---#

make_trial_design(
  ffy, 
  json_file = "fp_2021_TD.json", 
  num_levels = 5, 
  head_dist = c(120, 120), 
  side_plots_num = 1,
  use_ab = TRUE,
  assign_rates = FALSE,
  rerun = TRUE,
  locally_run = TRUE
)




