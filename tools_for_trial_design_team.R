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

ffy <- field_year_ls[17]

get_td_parameters(ffy, "fp_2021_TD.json")

#--- force headland to be 100 feet ---#

make_trial_design(
  ffy = field_year_ls[17], 
  json_file = "fp_2021_TD.json", 
  # head_dist = 120, # default uses 2 * the max of the input plot widths
  # side_plots_num = 1,
  # use_ab = TRUE,
  assign_rates = TRUE,
  #=== angled harvesting ===#
  # harvest angle relative to the planter/applicator
  # TRUE: uses both harvester and planter/applicator ab-lines or paste data
  # numeric: uses only planter/applicator ab-line or past data and apply the angle
  # rotation is clockwise 
  harvest_angle = TRUE, 
  #=== design type ===#
  # jcl: jump-conscious latin
  # ejca: extra jump-conscious alternate strip
  design_type = c("jcl", "jcl"),
  #=== number of levels ===#
  # used in "jcl", ignored in "ejca"
  num_levels = c(5, 5), 
  #=== maximum jump allowed  ===#
  # used in "ejca", ignored in "jcl"
  max_jumps = c(NA, NA),
  #=== if TRUE, create experiment plots again ===#
  # if TRUE, ignore the existence of exp_plots.rds
  start_from_scratch = TRUE,
  # cell_height = 10,
  rerun = TRUE
)

  rates <- c(40, 50, 60)

make_trial_design(
  ffy = field_year_ls[4], 
  json_file = "fp_2021_TD.json", 
  # head_dist = 120, # default uses 2 * the max of the machine widths
  side_plots_num = 1,
  # use_ab = TRUE,
  assign_rates = TRUE,
  #=== angled harvesting ===#
  # harvest angle relative to the planter/applicator
  # TRUE: uses both harvester and planter/applicator ab-lines or paste data
  # numeric: uses only planter/applicator ab-line or past data and apply the angle
  # rotation is clockwise 
  harvest_angle = 0, 
  # harvest_angle = TRUE, 
  #=== design type ===#
  # jcl: jump-conscious latin
  # ejca: extra jump-conscious alternate strip
  design_type = c("jcl", "jcl"),
  rates = list(c(40, 50, 60), c(50, 60, 70)),
  #=== number of levels ===#
  # used in "jcl", ignored in "ejca"
  num_levels = c(5, 5), 
  #=== maximum jump allowed  ===#
  # used in "ejca", ignored in "jcl"
  max_jumps = c(NA, NA),
  #=== if TRUE, create experiment plots again ===#
  # if TRUE, ignore the existence of exp_plots.rds
  start_from_scratch = TRUE,
  # cell_height = 10,
  rerun = TRUE,
  locally_run = TRUE
)



