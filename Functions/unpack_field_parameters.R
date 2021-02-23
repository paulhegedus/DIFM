#/*=================================================*/
#' # Extract input information from field data
#/*=================================================*/
#--- field data ---#
field_data <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "field_parameter.json"
  ),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")] %>% 
.[field_year == ffy, ]

#--- get field parameters for the field-year ---#
w_field_data <- field_data[field_year == ffy, ]

#--- get input data ---#
input_data <- dplyr::select(w_field_data, starts_with(
  "input")) %>%  map(1) %>% 
  rbindlist(fill = TRUE)

#/*----------------------------------*/
#' ## Crop information
#/*----------------------------------*/
crop <- w_field_data[, crop] 
crop_unit <- w_field_data[, crop_unit] 
crop_price <- w_field_data[, crop_price] 
if(!is.numeric(crop_price)) {
  crop_price <- case_when(
    crop == "soy" ~ 8.89, # $/bu
    crop == "corn" ~ 4 # $/bu
  )
}
land_unit <- w_field_data[, land_unit] 
reporting_unit <- w_field_data[, reporting_unit] 
harvester_width <- w_field_data[, h_width][[1]]

#/*----------------------------------*/
#' ## Input information
#/*----------------------------------*/
#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Seed
#/*~~~~~~~~~~~~~~~~~~~~~~*/
#--- should we process seed data? ---#
process_s <- "seed" %in% input_data[strategy == "trial", form]

if (process_s) {

  input_data_s <- input_data[strategy == "trial" & form == "seed", ]

  #++++++++++++++++
  # grower chosen rate and analysis type 
  #++++++++++++++++
  grower_chosen_rate_s <- input_data_s[, sq_rate]

  if(!is.numeric(grower_chosen_rate_s) | is.na(grower_chosen_rate_s)) {
      
    Rx_file_s <- file.path(
      here("Data/Growers", ffy, "Raw"), 
      paste0(grower_chosen_rate_s, ".shp")
    )

    if (file.exists(Rx_file_s)){
      #--- if the Rx file exists ---#
      gc_type_s <- "Rx"
    } else {
      #--- if the Rx file doe NOT exist ---#
      # default rate
      grower_chosen_rate_s <- case_when(
        crop == "corn" ~ 36,
        crop == "soy" ~ 120
      )
      gc_type_s <- "uniform"
    }
  } else {
    #--- seed rate conversion ---#
    if (grower_chosen_rate_s > 10000){
      #--- convert to K ---#
      grower_chosen_rate_s <- grower_chosen_rate_s / 1000
    }

    gc_type_s <- "uniform"
  }

  #++++++++++++++++
  # seed price
  #++++++++++++++++
  seed_price <- input_data_s[, price]

  if(!is.numeric(seed_price)) {
    #=== default price ===#
    if (crop == "corn") {
      seed_price <- 0.00275 * 1000 # (thousand seed)
    } else if(crop == "soy") {
      seed_price <- 0.000375 * 1000 # (thousand seed)
    }
  } else { # if price is numeric
    if (seed_price < 0.01) {
      seed_price <- seed_price * 1000
    }
  }

  #++++++++++++++++
  # planter width
  #++++++++++++++++
  planter_width <- input_data_s[, machine_width]
  
} else { # if not seed trial

  gc_type_s <- NA
  seed_price <- NA

}


#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Base nitrogen 
#/*~~~~~~~~~~~~~~~~~~~~~~*/
is_base_N <- "base" %in% input_data[, strategy]

if (is_base_N) {

  n_base_rate <- input_data[strategy == "base", ] %>% 
    rowwise() %>% 
    mutate(
      n_equiv_rate = convert_N_unit(form, unit, rate, reporting_unit) 
    ) %>% 
    data.table() %>% 
    .[, sum(n_equiv_rate)]

} else {

  n_base_rate <- 0  

}

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Nitrogen
#/*~~~~~~~~~~~~~~~~~~~~~~*/
#--- should we process N data? ---#
n_var_ls <- c("NH3", "urea", "uan32", "uan28")
process_n_idv <- n_var_ls %in% input_data[strategy == "trial", form]
process_n <- any(process_n_idv) 

if (process_n) {
  n_var <- n_var_ls[process_n_idv]
  input_data_n <- input_data[strategy == "trial" & form == n_var, ]

  #++++++++++++++++
  # grower chosen rate and analysis type 
  #++++++++++++++++
  grower_chosen_rate_n <- input_data_n[, sq_rate]

  if (!is.numeric(grower_chosen_rate_n)) {
      
    Rx_file_n <- file.path(
      here("Data/Growers", ffy, "Raw"), 
      paste0(grower_chosen_rate_n, ".shp")
    )

    if (file.exists(Rx_file_n)){
      #--- if the Rx file exists ---#
      gc_type_n <- "Rx"
    } else {
      #--- if the Rx file doe NOT exist ---#
      # default rate
      grower_chosen_rate_n <- 180
      gc_type_n <- "uniform"
    }
  } else {

    grower_chosen_rate_n <- convert_N_unit(
      input_data_n[, form], 
      input_data_n[, unit], 
      grower_chosen_rate_n, 
      reporting_unit
    ) + 
    n_base_rate

    gc_type_n <- "uniform"

  }

  #++++++++++++++++
  # N price
  #++++++++++++++++
  if ("price" %in% names(input_data_s)) {
    n_price <- input_data_s[, price]
  } else {
    n_price <- NA
  }

  if(!is.numeric(n_price)) {
    n_price <- 0.4
  }

  #++++++++++++++++
  # applicator width
  #++++++++++++++++
  applicator_width <- input_data_n[, machine_width]

} else {

  n_var <- NA
  gc_type_n <- NA
  n_price <- NA
}

#/*----------------------------------*/
#' ## Trial type
#/*----------------------------------*/
trial_type <- case_when(
  process_n & process_s ~ "SN",
  process_n & !process_s ~ "N",
  !process_n & process_s ~ "S"
)


#/*=================================================*/
#' # Dictionary
#/*=================================================*/
#/*----------------------------------*/
#' ## Variable name
#/*----------------------------------*/
dictionary <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "variable_name_dictionary.json"
  ),
  flatten = TRUE
) %>% 
data.table()

