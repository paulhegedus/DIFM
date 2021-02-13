# /*=================================================*/
#' # Non experimental data processing and reporting
# /*=================================================*/

non_exp_process_make_report <- function(ffy, rerun = FALSE, local = FALSE) {

  print(paste0("Proessing non-experiment data for ", ffy))

  boundary_file <- here("Data", "Growers", ffy) %>%
    file.path(., "Raw/boundary.shp")

  if (!file.exists(boundary_file)) {
    return(print("No boundary file exists."))
  }

  #--- read in the template ---#
  nep_rmd <- read_rmd("Codes_dev/DataProcessing/data_processing_template.Rmd", local = local)

  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "report_non_exp")] %>%
      .[str_detect(., c("cache|files"))] %>%
      unlink(recursive = TRUE)
  }

  #--- topography data ---#
  topo_file <- file.path(here("Data", "Growers", ffy), "Intermediate/topography.rds")

  if (!file.exists(topo_file)) {
    # ne01 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne01_topography.Rmd"
    # ) %>%
    # readLines() 
    ne01 <- read_rmd("Codes_dev/DataProcessing/ne01_topography.Rmd", local = local)
  } else {
    # ne01 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne01_topography_show.Rmd"
    # ) %>%
    # readLines() 
    ne01 <- read_rmd("Codes_dev/DataProcessing/ne01_topography_show.Rmd", local = local)
  }

  nep_rmd_t <- c(nep_rmd, ne01)

  #--- SSURGO data ---#
  ssurgo_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ssurgo.rds")

  if (!file.exists(ssurgo_file)) {
    # ne02 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne02_ssurgo.Rmd"
    # ) %>%
    # readLines() 
    ne02 <- read_rmd("Codes_dev/DataProcessing/ne02_ssurgo.Rmd", local = local)
  } else {
    # ne02 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne02_ssurgo_show.Rmd"
    # ) %>%
    # readLines() 
    ne02 <- read_rmd("Codes_dev/DataProcessing/ne02_ssurgo_show.Rmd", local = local)
  }

  nep_rmd_ts <- c(nep_rmd_t, ne02)

  #--- Weather data ---#
  weather_file <- file.path(here("Data", "Growers", ffy), "Intermediate/weather_daymet.rds")

   if (!file.exists(weather_file)) {
    # ne03 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne03_weather.Rmd"
    # ) %>%
    # readLines() 
    ne03 <- read_rmd("Codes_dev/DataProcessing/ne03_weather.Rmd", local = local)
  } else {
    # ne03 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne03_weather_show.Rmd"
    # ) %>%
    # readLines() 
    ne03 <- read_rmd("Codes_dev/DataProcessing/ne03_weather_show.Rmd", local = local)
  }

  nep_rmd_tsw <- c(nep_rmd_ts, ne03)

  #--- EC data ---#
  ec_exists <- field_data[field_year == ffy, ec]
  ec_raw_file <- file.path(here("Data", "Growers", ffy), "Raw/ec.shp")
  ec_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ec.rds")

  if (!file.exists(ec_file)) {
    # if ec.shp exists and it has not been processed
    # ne04 <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/ne04_ec_show.Rmd"
    # ) %>%
    # readLines() 
    ne04 <- read_rmd("Codes_dev/DataProcessing/ne04_ec_show.Rmd", local = local)
  } else {
    if (ec_exists & file.exists(ec_raw_file)) {
      # if ec.shp exists, but it has not been processed
      # ne04 <- file.path(
      #   here(), 
      #   "Codes/DataProcessing/ne04_ec.Rmd"
      # ) %>%
      # readLines()
      ne04 <- read_rmd("Codes_dev/DataProcessing/ne04_ec.Rmd", local = local)
    } else {
      # if ec.shp does not exist
      print("This field either does not have EC data or EC data has not been uploaded in the right place")
    }
  }

  nep_rmd_tswe <- c(nep_rmd_tsw, ne04) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Non-experiment Data Processing Report", .)

  # /*----------------------------------*/
  #' ## Write out the rmd and render
  # /*----------------------------------*/
  nep_report_rmd_file_name <- file.path(here(), "Data/Growers", ffy, "DataProcessingReport/dp_report_non_exp.Rmd")

  writeLines(nep_rmd_tswe, con = nep_report_rmd_file_name)

  #--- render ---#
  render(nep_report_rmd_file_name)

}

# /*=================================================*/
#' # Experiment data processing and reporting
# /*=================================================*/

exp_process_make_report <- function(ffy, rerun = FALSE, local = FALSE) {

  cat(paste0("============================================\n= Proessing experiment data for ", ffy, 
    "\n============================================")
  )
  #--- define field parameters ---#
  # source(
  #   here("Codes/Functions/unpack_field_parameters.R"),
  #   local = TRUE
  # )
  source(get_r_file_name_git("Functions/unpack_field_parameters.R"), local = TRUE)

  # exp_temp_rmd <- file.path(here(), "Codes/DataProcessing/data_processing_template.Rmd") %>%
    # readLines() 

  exp_temp_rmd <- read_rmd("Codes_dev/DataProcessing/data_processing_template.Rmd", local = local)

  # e01 <- file.path(here(), "Codes/DataProcessing/e01_gen_yield_polygons.Rmd") %>%
  #   readLines()

  e01 <- read_rmd("Codes_dev/DataProcessing/e01_gen_yield_polygons.Rmd", local = local)

  exp_rmd_y <- c(exp_temp_rmd, e01)
 
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Seed
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #--- check if trial design needs to be used ---#
  if (process_s) {
    use_td_s <- input_data[form == "seed", use_target_rate_instead]
  } else {
    use_td_s <- FALSE
  }

  if (process_s & !use_td_s) {
    #--- use as-planted data ---#
    # e02s <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/e02s_process_as_applied.Rmd"
    # ) %>%
    # readLines()

    e02s <- read_rmd("Codes_dev/DataProcessing/e02s_process_as_applied.Rmd", local = local)

  } else if (process_s & use_td_s){
    #--- use trial design instead ---#
    # e02s <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/e02s_use_td.Rmd"
    # ) %>%
    # readLines()

    e02s <- read_rmd("Codes_dev/DataProcessing/e02s_use_td.Rmd", local = local)

  } else {

    e02s <- NULL

  }

  exp_rmd_y_s <- c(exp_rmd_y, e02s)

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Nitrogen
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  if (process_n) {
    use_td_n <- input_data[form == n_var, use_target_rate_instead]
  } else {
    use_td_n <- FALSE  
  }
 
  if (process_n & !use_td_n) {
    # e02n <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/e02n_process_as_applied.Rmd"
    # ) %>%
    # readLines()
    e02n <- read_rmd("Codes_dev/DataProcessing/e02n_process_as_applied.Rmd", local = local)
  } else if (process_n & use_td_n){
    # e02n <- file.path(
    #   here(), 
    #   "Codes/DataProcessing/e02n_use_td.Rmd"
    # ) %>%
    # readLines()
    e02n <- read_rmd("Codes_dev/DataProcessing/e02n_use_td.Rmd", local = local)
  } else {
    e02n <- NULL
  }

  exp_rmd_y_sn <- c(exp_rmd_y_s, e02n)

  #/*----------------------------------*/
  #' ## Merge yield and input data
  #/*----------------------------------*/
  # e03 <- file.path(here(), "Codes/Codes_dev/DataProcessing/e03_yield_further_processing.Rmd") %>%
  #   readLines()

  e03 <- read_rmd("Codes_dev/DataProcessing/e03_yield_further_processing.Rmd", local = local)

  #/*----------------------------------*/
  #' ## Personalize the report 
  #/*----------------------------------*/
  exp_rmd_y_sn_m <- c(exp_rmd_y_sn, e03) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Experiment Data Processing Report", .) %>% 
    gsub("trial-type-here", trial_type, .)

  #/*=================================================*/
  #' # Remove cached files if rerun == TRUE
  #/*=================================================*/  
  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "report_exp")] %>%
      .[str_detect(., c("cache|files"))] %>%
      unlink(recursive = TRUE)
  }

  #/*=================================================*/
  #' # Write out the rmd and render
  #/*=================================================*/
  exp_report_rmd_file_name <- paste0(file.path(here(), "Data", "Growers", ffy, "DataProcessingReport/dp_report_exp.Rmd"))
  exp_report_r_file_name <- paste0(file.path(here(), "Data", "Growers", ffy, "DataProcessingReport/for_debug.R"))

  writeLines(exp_rmd_y_sn_m, con = exp_report_rmd_file_name)

  purl(exp_report_rmd_file_name, output = exp_report_r_file_name)


  render(exp_report_rmd_file_name)

}

# /*=================================================*/
#' # Final data processing and reporting
# /*=================================================*/
f_process_make_report <- function(ffy, rerun = FALSE, local = FALSE) {

  # /*----------------------------------*/
  #' ## Experiment data processing
  # /*----------------------------------*/
  # fp_temp_rmd <- here() %>%
  #   file.path(., "Codes/DataProcessing/data_processing_template.Rmd") %>%
  #   readLines() %>%
  #   gsub("field-year-here", ffy, .)

  fp_temp_rmd <- read_rmd("Codes_dev/DataProcessing/data_processing_template.Rmd", local = local) %>%
    gsub("field-year-here", ffy, .)

  # f01_rmd <- readLines(file.path(here(), "Codes/DataProcessing/f01_combine_all_datasets.Rmd"))

  f01_rmd <- read_rmd("Codes_dev/DataProcessing/f01_combine_all_datasets.Rmd", local = local)

  fp_rmd <- c(fp_temp_rmd, f01_rmd)

  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "final")] %>%
      .[str_detect(., "cache|files")] %>%
      unlink(recursive = TRUE)
  }

  # /*----------------------------------*/
  #' ## Data availability check (EC, soil sampling)
  # /*----------------------------------*/
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### topography data
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  topo_exist <- file.path(here("Data", "Growers", ffy), "Intermediate/topography.rds") %>%
    file.exists()

  if (topo_exist) {
    fp_rmd <- gsub("topo_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("topo_eval_here", FALSE, fp_rmd)
  }

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### SSURGO data
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  ssurgo_exist <- file.path(here("Data", "Growers", ffy), "Intermediate/ssurgo.rds") %>%
    file.exists()

  if (ssurgo_exist) {
    fp_rmd <- gsub("ssurgo_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("ssurgo_eval_here", FALSE, fp_rmd)
  }

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### EC
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  ec_exists <- field_data[field_year == ffy, ec == "received"]
  ec_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ec.rds")

  if (ec_exists & file.exists(ec_file)) {
    fp_rmd <- gsub("ec_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("ec_eval_here", FALSE, fp_rmd)
  }

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Soil sampling
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  gss_exists <- field_data[field_year == ffy, soil_sampling == "received"]
  gss_file <- file.path(here("Data", "Growers", ffy), "Intermediate/gss.rds")

  if (gss_exists & file.exists(gss_file)) {
    fp_rmd <- gsub("gss_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("gss_eval_here", FALSE, fp_rmd)
  }

  # /*----------------------------------*/
  #' ## Write out the rmd and render
  # /*----------------------------------*/
  exp_report_rmd_file_name <- "DataProcessingReport/final_processing_report.Rmd" %>%
    file.path(here(), "Data", "Growers", ffy, .) %>%
    paste0(.)

  writeLines(fp_rmd, con = exp_report_rmd_file_name)

  render(exp_report_rmd_file_name)
}


#/*=================================================*/
#' # Run analysis
#/*=================================================*/
run_analysis <- function(ffy, rerun = FALSE, local = FALSE){

  data_for_analysis_exists <- here("Data", "Growers", ffy, "Analysis-Ready", "analysis_data.rds") %>% 
    file.exists()

  if (!data_for_analysis_exists) {
    return(print("No data that is ready for analysis exist. Process the datasets first."))
  }


  if (rerun){
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Reports/Growers", ffy),
      full.names = TRUE
    ) %>% 
    .[str_detect(., "analysis")] %>% 
    .[str_detect(., c("cache|files"))] %>% 
    unlink(recursive = TRUE)
  }

  # temp_rmd <- "Codes/Analysis/a00_analysis_template.Rmd" %>% 
  #   file.path(here(), .) %>% 
  #   readLines() %>% 
  #   gsub("field-year-here", ffy, .)

  temp_rmd <- read_rmd("Codes_dev/Analysis/a00_analysis_template.Rmd", local = local) %>% 
    gsub("field-year-here", ffy, .)

  #/*----------------------------------*/
  #' ## Bifurcations
  #/*----------------------------------*/
  #--- define field parameters ---#
  # source(
  #   here("Codes/Functions/unpack_field_parameters.R"),
  #   local = TRUE
  # )
  source(get_r_file_name_git("Functions/unpack_field_parameters.R"), local = TRUE)

  if (trial_type == "S") {
    # analysis_rmd <- here("Codes/Analysis/a01_seed_analysis.Rmd") %>% 
    #   readLines()
    analysis_rmd <- read_rmd("Codes_dev/Analysis/a01_seed_analysis.Rmd", local = local)
  } else if (trial_type == "N"){
    # analysis_rmd <- here("Codes/Analysis/a01_nitrogen_analysis.Rmd") %>% 
    #   readLines()
    analysis_rmd <- read_rmd("Codes_dev/Analysis/a01_nitrogen_analysis.Rmd", local = local)
  } else {
     
  }
  
  temp_rmd <- c(temp_rmd, analysis_rmd)

  #/*----------------------------------*/
  #' ## Save and run
  #/*----------------------------------*/
  analysis_rmd_file_name <- here() %>% 
    paste0(., "/Reports/Growers/", ffy, "/analysis.Rmd")

  writeLines(temp_rmd, con = analysis_rmd_file_name)

  render(analysis_rmd_file_name)

}

#/*=================================================*/
#' # Make report (run after run_analysis)
#/*=================================================*/

make_grower_report <- function(ffy, rerun = TRUE, local = FALSE){
  
  analysis_data_exists <- here("Reports/Growers", ffy, "analysis_results.rds") %>% 
    file.exists()

  # source(
  #   here("Codes/Functions/unpack_field_parameters.R"),
  #   local = TRUE
  # )
  source(get_r_file_name_git("Functions/unpack_field_parameters.R"), local = TRUE)

  if (!analysis_data_exists) {
    return(print("No analysis results exist. Run analysis first."))
  }

  #/*----------------------------------*/
  #' ## Read analysis results
  #/*----------------------------------*/
  results <- readRDS(here("Reports", "Growers", ffy, "analysis_results.rds"))

  data_sf <- results$data_sf[[1]]
  ys_by_char <- results$ys_by_char[[1]]
  eval_data <- results$eval_data[[1]]
  opt_gc_data <- results$opt_gc_data[[1]]
  whole_profits_test <- results$whole_profits_test[[1]]
  pi_dif_test_zone <- results$pi_dif_test_zone[[1]]

  #/*----------------------------------*/
  #' ## If rerun = TRUE
  #/*----------------------------------*/
  if (rerun){
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Reports/Growers", ffy),
      full.names = TRUE
    ) %>% 
    .[str_detect(., "grower-report")] %>% 
    .[str_detect(., c("cache|files"))] %>% 
    unlink(recursive = TRUE)
  }

  # temp_rmd <- "Codes/Report/r01_soy_make_report_html.Rmd" %>% 
  #   file.path(here(), .) %>% 
  #   readLines() %>% 
  #   gsub("field-year-here", ffy, .)

  temp_rmd <- read_rmd("Codes_dev/Report/r01_soy_make_report_html.Rmd", local = local) %>% 
    gsub("field-year-here", ffy, .)

  #/*----------------------------------*/
  #' ## Insert appropriate texts  
  #/*----------------------------------*/
  # case 1: variable grower-chosen rates (Rx available)
  # case 2: uniform grower-chosen rate

  if (gc_type_s == "Rx") {

    t_whole_ovg <- whole_profits_test[type_short == "ovg", t]

    # results_discussions_rmd <- readLines(here("Codes", "Report", "ri01_results_by_zone_Rx.Rmd")) %>% 
    results_discussions_rmd <- read_rmd("Codes_dev/Report/ri01_results_by_zone_Rx.Rmd", local = local) %>% 
      gsub(
        "_stat_confidence_here_", 
        case_when(
          t_whole_ovg >= 1.96 ~ "high",
          t_whole_ovg >= 1.3 & t_whole_ovg < 1.96 ~ "moderate",
          t_whole_ovg < 1.3 ~ "low"
        ), 
        .
      )

    growe_plan_text <- "follow the commercial prescription depicted 
      in figure \\\\@ref(fig:rx-s-map)"

  } else {
    # results_discussions_rmd <- readLines(here("Codes", "Report", "ri01_results_by_zone_non_Rx.Rmd"))
    results_discussions_rmd <- read_rmd("Codes_dev/Report/ri01_results_by_zone_non_Rx.Rmd", local = local)

    growe_plan_text <- "apply grower_chosen_rate_hereK seeds per acre 
      uniformly across the field. numb_seed_rates_here 
      experimental seed rates were assigned randomly and in 
      roughly equal number to plots" 

  }

  temp_rmd <- gsub(
    "_grower-plan-here_", 
    growe_plan_text, 
    temp_rmd
  )
  
  results_discussions_index <- which(str_detect(temp_rmd, "_results-and-discussions-here_"))

  temp_rmd <- c(
    temp_rmd[1:(results_discussions_index-1)],
    results_discussions_rmd,
    temp_rmd[(results_discussions_index+1):length(temp_rmd)]
  )

  if (gc_type_s != "Rx") {
    gc_opt_comp_txt_ls <- c()
    num_zones <- nrow(pi_dif_test_zone)
    for (i in 1:num_zones) {
      temp_dif <- get_seed("gc", i) - get_seed("opt_v", i)
      gc_opt_comp_txt_ls <- c(gc_opt_comp_txt_ls,
        paste0(
          "`r get_seed(\"gc\", ", 
          i, 
          ") - get_seed(\"opt_v\", ", 
          i,
          ")`K seeds ", 
          ifelse(temp_dif > 0, "too high", "too low"),
          " in Zone ", i
        )
      )
    }
    gc_opt_comp_txt <- paste0(gc_opt_comp_txt_ls, collapse = ", ")

    temp_rmd <- gsub(
      "_gc-opt-comp-txt-comes-here_",
      gc_opt_comp_txt,
      temp_rmd
    )
  } 
  
  #/*----------------------------------*/
  #' ## Insert texts in the Summary section
  #/*----------------------------------*/ 
  whole_pi_t <- whole_profits_test[type_short == "ovg", t]

  if (whole_pi_t > 1.96) {

    text_summary <- "The data and model provide a high degree of statistical confidence in this result"

  } else if (whole_pi_t > 1.3) {

    text_summary <- "The data and model provide a moderate degree of statistical confidence in this result"

  } else {
    
    text_summary <- "But, the data and model provide a low degree of statistical confidence in this result"

  } 

  temp_rmd <- gsub("_statement-whole-field-pi-here_", text_summary, temp_rmd)

  #/*----------------------------------*/
  #' ## Write the rmd file and run
  #/*----------------------------------*/
  analysis_rmd_file_name <- here() %>% 
    paste0(., "/Reports/Growers/", ffy, "/grower-report.Rmd")

  writeLines(temp_rmd, con = analysis_rmd_file_name)

  render(analysis_rmd_file_name)

}

#/*=================================================*/
#' # Make trial design and create a report
#/*=================================================*/

make_trial_design <- function(ffy, rates = NA, plot_width = NA, head_dist = NA, use_ab = TRUE, rerun = FALSE, local = FALSE) {

  # head_dist in feet

  print(paste0("Generating a trial-design for ", ffy))

  boundary_file <- here("Data", "Growers", ffy) %>%
    file.path(., "Raw/boundary.shp")

  ab_line_file <- here("Data", "Growers", ffy) %>%
    file.path(., "Raw/ab-line.shp")

  if (!file.exists(boundary_file) | !file.exists(ab_line_file)) {
    return(print("No boundary file exists."))
  }

  #--- read in the template ---#
  # td_rmd <- file.path(here(), "Codes/TrialDesignGeneration/trial_design_header.Rmd") %>%
  #   readLines() %>% 
  td_rmd <- read_rmd("Codes_dev/TrialDesignGeneration/trial_design_header.Rmd", local = local) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Trial Design Generation Report", .)

  #--- if using ab-line ---#
  if(use_ab) {
    # ab_rmd <- file.path(here(), "Codes/TrialDesignGeneration/trial-design-ab-line.Rmd") %>% 
    #   readLines()
    ab_rmd <- read_rmd("Codes_dev/TrialDesignGeneration/trial-design-ab-line.Rmd", local = local) 
    td_rmd <- c(td_rmd, ab_rmd)
  }  

  if (!is.na(head_dist)) {
    td_rmd <- gsub(
      "head-dist-here", 
      conv_unit(head_dist, "ft", "m"),
      td_rmd
    )
  } else {
    td_rmd <- gsub(
      "head-dist-here", 
      "NA",
      td_rmd
    )
  }

  if (!is.na(plot_width) & is.numeric(plot_width)) {
    td_rmd <- gsub(
      "plot-width-here", 
      conv_unit(plot_width, "ft", "m"), 
      td_rmd
    )
  } else if (is.na(plot_width)) {
    td_rmd <- gsub(
      "plot-width-here", 
      "NA", 
      td_rmd
    )
  } else {
    writeLines("The plot width you provided are not valid.")
    break
  }

  if (!is.na(rates) & is.numeric(rates)) {
    td_rmd <- gsub(
      "rates-here", 
      paste0("c(", paste0(rates, collapse = ","), ")"), 
      td_rmd
    ) 

  } else if (is.na(rates)) {
    td_rmd <- gsub(
      "rates-here", 
      "NA", 
      td_rmd
    )  
    writeLines(
      "Rates were not provided. Multiple trial designs\nwill be created around the grower-chosen rate,\n and no trial design shape file will be created."
    )

  } else {
    writeLines("The rates you provided are not valid.")
    break
  }

  td_file_name <- file.path(here(), "Data/Growers", ffy, "TrialDesign/make_trial_design.Rmd")

  writeLines(td_rmd, con = td_file_name)

  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "TrialDesign"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "make_trial_design")] %>%
      .[str_detect(., c("cache|files"))] %>%
      unlink(recursive = TRUE)
  }

  #--- render ---#
  render(td_file_name)

}

#/*----------------------------------*/
#' ## Read rmd file from github repository
#/*----------------------------------*/

read_rmd <- function(file_name, local = FALSE) {

  if (local == FALSE) {
    file_name_on_github <- paste0("https://github.com/tmieno2/DIFM/blob/master/", file_name)  
    rmd_file <- suppressMessages(readLines(file_name_on_github))
  } else {
    rmd_file <- readLines(here("Codes", file_name))
  }

  return(rmd_file)

}

get_r_file_name_git <- function(file_name) {

  file_name_on_github <- paste0("https://github.com/tmieno2/DIFM/blob/master/Codes_dev/", file_name, "?raw=TRUE")  

}



