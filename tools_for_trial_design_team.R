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
    "field_parameter_example.json"
  ),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")]

#/*=================================================*/
#' # Make trial designs
#/*=================================================*/

ffy <- field_year_ls[2]

make_trial_design(ffy, rerun = TRUE)

#--- force headland to be 100 feet ---#
make_trial_design(ffy, head_dist = 100, rerun = TRUE)




