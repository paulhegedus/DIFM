
render("/Users/tmieno2/Box/DIFM_Central/Reports/Growers/Scheider_Roby_2019/grower-report Taro Friday (DSB revised).Rmd")

field_data <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "fp_new_DSB.json"
  ),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")] 

field_data[, trial_supervisor := "Robert Dunker"]
field_data[, researcher := "University of Illinois"] 

field_data <- relocate(field_data, trial_supervisor, researcher, farm, field, year, crop)

jsonlite::write_json(
    field_data, 
    file.path(
      here("Data", "CommonData"),
      "fp_new_DSB.json"
    ),
    pretty = TRUE
  )

for (i in seq_len(nrow(field_data))) {
  #--- get field parameters for the field-year ---#
  w_field_data <- field_data[i, ]

  #--- get input data ---#
  input_data <- dplyr::select(w_field_data, starts_with(
    "input")) %>%  map(1) %>% 
    rbindlist(fill = TRUE) 

  # j <- 1
  for (j in seq_len(nrow(input_data))) {

    if (!"price" %in% names(input_data[j, ])) {
      input_data[j, price := "numeric (no double quotes needed)"]
    }
    if (!"unit" %in% names(input_data[j, ])) {
      input_data[j, unit := "gallons, lbs, Mg, kg, bales"]
    }
    if (!"date" %in% names(input_data[j, ])) {
      input_data[j, date := "mm/dd/yyyy"]
    }
    if (input_data[j, strategy == "trial"] & !"Rx_exists" %in% names(input_data[j, ])) {
      input_data[j, Rx_exists := "not available, exists (not received), received"]
    }

    if (input_data[j, strategy == "trial"]) {
      input_data[j, ] <- input_data[j, .(form, strategy, unit, price, date, data, sq_rate, Rx_exists, machine_width, use_target_rate_instead)]
    } else {
      input_data[j, ] <- input_data[j, .(form, strategy, unit, price, date, data, rate, machine_width)]
    }
    

    eval(parse(text=paste("field_data[i, input.", j , ":= list(temp_input_data)]", sep = "")))

  }
  
}

library(httr)
req <- GET("https://api.github.com/repos/KZPS/Spotkania/git/trees/master?recursive=1")
req <- GET("https://github.com/tmieno2/DIFM/tree/master/Codes_dev?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
grep("Matteo/literature/", filelist, value = TRUE, fixed = TRUE)





