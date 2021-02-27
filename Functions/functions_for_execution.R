# /*=================================================*/
#' # Non experimental data processing and reporting
# /*=================================================*/

non_exp_process_make_report <- function(ffy, rerun = FALSE, locally_run = FALSE) {

  library(knitr)
  options(knitr.duplicate.label = "allow")

  print(paste0("Proessing non-experiment data for ", ffy))

  boundary_file <- here("Data", "Growers", ffy) %>%
    file.path(., "Raw/boundary.shp")

  if (!file.exists(boundary_file)) {
    return(print("No boundary file exists."))
  }

  #--- read in the template ---#
  nep_rmd <- read_rmd("DataProcessing/data_processing_template.Rmd", locally_run = locally_run)

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
    ne01 <- read_rmd("DataProcessing/ne01_topography.Rmd", locally_run = locally_run)
  } else {
    ne01 <- read_rmd("DataProcessing/ne01_topography_show.Rmd", locally_run = locally_run)
  }

  nep_rmd_t <- c(nep_rmd, ne01)

  #--- SSURGO data ---#
  ssurgo_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ssurgo.rds")

  if (!file.exists(ssurgo_file)) {
    ne02 <- read_rmd("DataProcessing/ne02_ssurgo.Rmd", locally_run = locally_run)
  } else {
    ne02 <- read_rmd("DataProcessing/ne02_ssurgo_show.Rmd", locally_run = locally_run)
  }

  nep_rmd_ts <- c(nep_rmd_t, ne02)

  #--- Weather data ---#
  weather_file <- file.path(here("Data", "Growers", ffy), "Intermediate/weather_daymet.rds")

   if (!file.exists(weather_file)) {
    ne03 <- read_rmd("DataProcessing/ne03_weather.Rmd", locally_run = locally_run)
  } else {
    ne03 <- read_rmd("DataProcessing/ne03_weather_show.Rmd", locally_run = locally_run)
  }

  nep_rmd_tsw <- c(nep_rmd_ts, ne03)

  #--- EC data ---#
  ec_exists <- field_data[field_year == ffy, ec]
  ec_raw_file <- file.path(here("Data", "Growers", ffy), "Raw/ec.shp")
  ec_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ec.rds")

  if (!file.exists(ec_file)) {
    ne04 <- read_rmd("DataProcessing/ne04_ec_show.Rmd", locally_run = locally_run)
  } else {
    if (ec_exists & file.exists(ec_raw_file)) {
      ne04 <- read_rmd("DataProcessing/ne04_ec.Rmd", locally_run = locally_run)
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

exp_process_make_report <- function(ffy, rerun = FALSE, locally_run = FALSE) {

  library(knitr)
  options(knitr.duplicate.label = "allow")

  cat(paste0("============================================\n= Processing experiment data for ", ffy, 
    "\n============================================")
  )
  #--- define field parameters ---#
  source(
    get_r_file_name("Functions/unpack_field_parameters.R"), 
    local = TRUE
  )
 
  exp_temp_rmd <- read_rmd(
    "DataProcessing/data_processing_template.Rmd", 
    locally_run = locally_run
  )

  e01 <- read_rmd(
    "DataProcessing/e01_gen_yield_polygons.Rmd", 
    locally_run = locally_run
  )

  exp_rmd_y <- c(exp_temp_rmd, e01)

  #/*----------------------------------*/
  #' ## Rmd(s) for input processing
  #/*----------------------------------*/
  e02 <- trial_info %>% 
    rowwise() %>% 
    mutate(
      e02_rmd = list(
        prepare_e02_rmd(
          input_type, 
          process, 
          use_td
        )
      )
    ) %>% 
    data.table() %>% 
    .[, e02_rmd] %>% 
    reduce(c)

  exp_rmd_yi <- c(exp_rmd_y, e02)

  #/*----------------------------------*/
  #' ## Merge yield and input data
  #/*----------------------------------*/
  e03 <- read_rmd(
    "DataProcessing/e03_yield_input_integration.Rmd", 
    locally_run = locally_run
  )

  #/*----------------------------------*/
  #' ## Personalize the report 
  #/*----------------------------------*/
  exp_rmd_yiy <- c(exp_rmd_yi, e03) %>% 
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
  exp_report_rmd_file_name <- here(
    "Data/Growers", 
    ffy, 
    "DataProcessingReport/dp_report_exp.Rmd"
  )

  exp_report_r_file_name <- here(
    "Data/Growers", 
    ffy, 
    "DataProcessingReport/for_debug.R"
  )

  writeLines(exp_rmd_yiy, con = exp_report_rmd_file_name)

  purl(exp_report_rmd_file_name, output = exp_report_r_file_name)

  render(exp_report_rmd_file_name)

}

# /*=================================================*/
#' # Final data processing and reporting
# /*=================================================*/
f_process_make_report <- function(ffy, rerun = FALSE, locally_run = FALSE) {

  library(knitr)
  options(knitr.duplicate.label = "allow")

  # /*----------------------------------*/
  #' ## Experiment data processing
  # /*----------------------------------*/
  # fp_temp_rmd <- here() %>%
  #   file.path(., "Codes/DataProcessing/data_processing_template.Rmd") %>%
  #   readLines() %>%
  #   gsub("field-year-here", ffy, .)
  source(
    get_r_file_name("Functions/unpack_field_parameters.R"), 
    local = TRUE
  )

  fp_temp_rmd <- read_rmd("DataProcessing/data_processing_template.Rmd", locally_run = locally_run) %>%
    gsub("field-year-here", ffy, .)

  # f01_rmd <- readLines(file.path(here(), "Codes/DataProcessing/f01_combine_all_datasets.Rmd"))

  f01_rmd <- read_rmd("DataProcessing/f01_combine_all_datasets.Rmd", locally_run = locally_run)

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
  ec_exists <- w_field_data[field_year == ffy, ec == "received"]
  ec_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ec.rds")

  if (ec_exists & file.exists(ec_file)) {
    fp_rmd <- gsub("ec_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("ec_eval_here", FALSE, fp_rmd)
  }

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Soil sampling
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  gss_exists <- w_field_data[field_year == ffy, soil_sampling == "received"]
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
run_analysis <- function(ffy, rerun = FALSE, locally_run = FALSE){

  library(knitr)
  options(knitr.duplicate.label = "allow")

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
    .[str_detect(., "cache|files")] %>% 
    unlink(recursive = TRUE)
  }
  
  analysis_rmd <- read_rmd(
    "Analysis/a01_analysis.Rmd", 
    locally_run = locally_run
  ) %>% 
  gsub("field-year-here", ffy, .)

  #/*----------------------------------*/
  #' ## Save and run
  #/*----------------------------------*/
  analysis_rmd_file_name <- here() %>% 
    paste0(., "/Reports/Growers/", ffy, "/analysis.Rmd")

  writeLines(analysis_rmd, con = analysis_rmd_file_name)

  render(analysis_rmd_file_name)

}

#/*=================================================*/
#' # Make report (run after run_analysis)
#/*=================================================*/

make_grower_report <- function(ffy, rerun = TRUE, locally_run = FALSE){
 
  library(knitr)
  options(knitr.duplicate.label = "allow")

  source(
    get_r_file_name("Functions/unpack_field_parameters.R"), 
    local = TRUE
  )
  
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

  base_rmd <- read_rmd(
    "Report/r00_report_header.Rmd",
    locally_run = locally_run
  ) %>% 
  gsub("field-year-here", ffy, .) 

  results_gen_rmd <- read_rmd(
    "Report/r01_gen_results.Rmd",
    locally_run = locally_run
  )  

  report_rmd_ls <- trial_info %>% 
    mutate(
      report_rmd = list(c(base_rmd, results_gen_rmd))  
    ) %>% 
    rowwise() %>% 
    mutate(
      unit_txt = case_when(
        input_type == "S" ~ "K seeds",
        input_type == "N" ~ "lbs",
        input_type == "K" ~ "lbs"
      )
    ) %>% 
    mutate(
      input_full_name = case_when(
        input_type == "S" ~ "Seed",
        input_type == "N" ~ "Nitrogen",
        input_type == "K" ~ "Potassium"
      )
    ) %>% 
    mutate(
      report_body = list(
        read_rmd(
          "Report/r01_make_report.Rmd",
          locally_run = locally_run
        )
      )
    ) %>% 
    mutate(
      res_disc_rmd = list(
        get_ERI_texts(
          input_type = input_type, 
          grower_chosen_rate = gc_rate,
          results = results, 
          gc_type = gc_type, 
          locally_run = locally_run
        )
      ),
      td_txt = list(
        get_td_text(
          input_type = input_type, 
          gc_type = gc_type, 
          locally_run = locally_run
        )
      ) 
    ) %>% 
    mutate(
      report_body = list(
        insert_rmd(
          target_rmd = report_body, 
          inserting_rmd = res_disc_rmd,
          target_text = "_results-and-discussions-here_"
        ) %>% 
        insert_rmd(
          target_rmd = ., 
          inserting_rmd = td_txt,
          target_text = "_trial_design_information_here_"
        ) %>% 
        gsub(
          "_input_full_name_l_", 
         tolower(input_full_name),
         .
        )
      )
    ) %>% 
    mutate(
      report_rmd = list(
        c(report_rmd, report_body)
      )
    ) %>% 
    mutate(
      write_file_name = list(
        here(
          "Reports/Growers", ffy, 
          paste0("grower-report-", tolower(input_type), ".Rmd")
        )
      )
    ) %>% 
    mutate(
      report_rmd = list(
        report_rmd %>% 
          gsub("_unit_here_", unit_txt, .) %>% 
          gsub("_input_full_name_here_c_", input_full_name, .) %>% 
          gsub("_input_type_here_", input_type, .)
      )
    )

  #/*----------------------------------*/
  #' ## Write to Rmd file(s)
  #/*----------------------------------*/
  report_rmd_ls %>% 
    summarise(
      list(
        writeLines(report_rmd, write_file_name)
      )
    )

  #/*----------------------------------*/
  #' ## Knit
  #/*----------------------------------*/
  report_rmd_ls %>% 
    pluck("write_file_name") %>% 
    lapply(., render)

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
  td_rmd <- read_rmd("TrialDesignGeneration/trial_design_header.Rmd", local = local) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Trial Design Generation Report", .)

  #--- if using ab-line ---#
  if(use_ab) {
    # ab_rmd <- file.path(here(), "Codes/TrialDesignGeneration/trial-design-ab-line.Rmd") %>% 
    #   readLines()
    ab_rmd <- read_rmd("TrialDesignGeneration/trial-design-ab-line.Rmd", local = local) 
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

# file_name <- "DataProcessing/data_processing_template.Rmd"
# rmd_file[1:10]

read_rmd <- function(file_name, locally_run = FALSE) {

  if (locally_run == FALSE) {
    file_name_on_github <- paste0("https://github.com/tmieno2/DIFM/blob/master/", file_name, "?raw=TRUE")  
    rmd_file <- suppressMessages(readLines(file_name_on_github))
  } else {
    rmd_file <- readLines(here("Codes", file_name))
  }

  return(rmd_file)

}

get_r_file_name <- function(file_name, locally_run = FALSE) {

  if (locally_run == FALSE) {
    file_name <- paste0("https://github.com/tmieno2/DIFM/blob/master/", file_name, "?raw=TRUE")  
  } else {
    file_name <- here("Codes", file_name)
  }

  return(file_name)

}

insert_rmd <- function(target_rmd, inserting_rmd, target_text) {

  inserting_index <- which(str_detect(target_rmd, target_text))

  return_md <- c(
    target_rmd[1:(inserting_index-1)],
    inserting_rmd,
    target_rmd[(inserting_index+1):length(target_rmd)]
  )

  return(return_md)

}   

get_ttest_text <- function(input_type, pi_dif_test_zone, zone){

  t <- pi_dif_test_zone[zone_txt == paste0("Zone ", zone), t]

  if (input_type == "S") {
    if (t < 1.30){
      temp_text <- "The data and model provide negligible evidence that the estimated optimal rate of `r get_seed(opt_gc_data_s, \"opt_v\", zone)`K does not provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(pi_dif_test_zone_s, zone)`)"
    } else if (1.30 <= t & t < 1.64){
      temp_text <- "The data and model provide only limited evidence that the estimated optimal rate of `r get_seed(opt_gc_data_s, \"opt_v\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(pi_dif_test_zone_s, zone)`)"
    } else if (1.64 <= t & t < 1.96){
      temp_text <- "The data and model provide moderate evidence that the estimated optimal rate of `r get_seed(opt_gc_data_s, \"opt_v\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(pi_dif_test_zone_s, zone)`)"
    } else {
      temp_text <- "The data and model provide strong evidence that the estimated optimal rate of `r get_seed(opt_gc_data_s, \"opt_v\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(pi_dif_test_zone_s, zone)`)" 
    }
  } else if (input_type == "N") {
    if (t < 1.30){
      temp_text <- "The data and model provide negligible evidence that the estimated optimal rate of `r get_n(opt_gc_data, \"opt_v\", zone)` does not provide greater profits than the grower-chosen rate of grower_chosen_rate_here (t-value of `r get_t_value(pi_dif_test_zone_n, zone)`)"
    } else if (1.30 <= t & t < 1.64){
      temp_text <- "The data and model provide only limited evidence that the estimated optimal rate of `r get_n(opt_gc_data, \"opt_v\", zone)` did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_here (t-value of `r get_t_value(pi_dif_test_zone_n, zone)`)"
    } else if (1.64 <= t & t < 1.96){
      temp_text <- "The data and model provide moderate evidence that the estimated optimal rate of `r get_n(opt_gc_data, \"opt_v\", zone)` did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_here (t-value of `r get_t_value(pi_dif_test_zone_n, zone)`)"
    } else {
      temp_text <- "The data and model provide strong evidence that the estimated optimal rate of `r get_n(opt_gc_data, \"opt_v\", zone)` did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_here (t-value of `r get_t_value(pi_dif_test_zone_n, zone)`)" 
    }
  }

  return(gsub(", zone", paste0(", ", zone), temp_text))
}


get_ERI_texts <- function(input_type, grower_chosen_rate, results, gc_type, locally_run = FALSE){

  whole_profits_test <- results$whole_profits_test[[1]]
  pi_dif_test_zone <- results$pi_dif_test_zone[[1]]
  opt_gc_data <- results$opt_gc_data[[1]]

  res_disc_rmd_file <- case_when(
    input_type == "N" & gc_type == "Rx" ~ 
      "Report/ri01_results_by_zone_Rx_N.Rmd", 
    input_type == "S" & gc_type == "Rx" ~ 
      "Report/ri01_results_by_zone_Rx_S.Rmd",  
    input_type == "N" & gc_type == "uniform" ~ 
      "Report/ri01_results_by_zone_non_Rx_N.Rmd",
    input_type == "S" & gc_type == "uniform" ~ 
      "Report/ri01_results_by_zone_non_Rx_S.Rmd"
  )

  pi_rmd_file <- case_when(
    input_type == "N" ~ "Report/ri02_profit_dif_statement_N.Rmd",
    input_type == "S" ~ "Report/ri02_profit_dif_statement_S.Rmd"
  )

  if (input_type == "S") {
    pi_rmd_file <- "Report/ri02_profit_dif_statement_S.Rmd"
  } else if (input_type == "N") {
    pi_rmd_file <- "Report/ri02_profit_dif_statement_N.Rmd"
  }

  if (gc_type == "Rx") {

    t_whole_ovg <- whole_profits_test[type_short == "ovg", t]

    res_disc_rmd <- read_rmd(res_disc_rmd_file, locally_run = locally_run) %>% 
    gsub(
      "_stat_confidence_here_", 
      case_when(
        t_whole_ovg >= 1.96 ~ "high",
        t_whole_ovg >= 1.3 & t_whole_ovg < 1.96 ~ "moderate",
        t_whole_ovg < 1.3 ~ "low"
      ), 
      .
    )

  } else {

    res_disc_rmd <- read_rmd(res_disc_rmd_file, locally_run = locally_run)
    
    #/*----------------------------------*/
    #' ## Profit differential narrative
    #/*----------------------------------*/
    # Statements about the difference between 
    # optimal vs grower-chosen rates

    num_zones <- nrow(pi_dif_test_zone)

    for (i in 2:num_zones) {
    # note: zone 1 has a longer version already in res_disc_rmd 
      if (i == 2) {

        pi_dif_rmd <- read_rmd(pi_rmd_file, locally_run = locally_run) %>% 
        gsub("_insert-zone-here_", i, .) %>% 
        gsub("_t-test-statement-here_", get_ttest_text(input_type, pi_dif_test_zone, i), .)

      } else {

        temp_pi_dif_rmd <- read_rmd(pi_rmd_file, locally_run = locally_run) %>% 
        gsub("_insert-zone-here_", i, .) %>% 
        gsub("_t-test-statement-here_", get_ttest_text(input_type, pi_dif_test_zone, i), .)

        pi_dif_rmd <- c(pi_dif_rmd, temp_pi_dif_rmd) 

      }
    }

    res_disc_rmd <- insert_rmd(
      target_rmd = res_disc_rmd, 
      inserting_rmd = pi_dif_rmd,
      target_text = "_rest-of-the-zones-here_"
    ) %>% 
    gsub("grower_chosen_rate_here", grower_chosen_rate, .)

    #/*----------------------------------*/
    #' ## Difference between optimal vs grower-chosen rates
    #/*----------------------------------*/
    # Statements about the difference between 
    # optimal vs grower-chosen rates
    gc_opt_comp_txt_ls <- c()
    
    for (i in 1:num_zones) {
      temp_dif <- get_seed(opt_gc_data, "gc", i) - get_seed(opt_gc_data, "opt_v", i)
      gc_opt_comp_txt_ls <- c(gc_opt_comp_txt_ls,
        paste0(
          "`r get_seed(opt_gc_data, \"gc\", ", 
          i, 
          ") - get_seed(opt_gc_data, \"opt_v\", ", 
          i,
          ")`K seeds ", 
          ifelse(temp_dif > 0, "too high", "too low"),
          " in Zone ", i
        ) %>% 
        gsub("opt_gc_data", paste0("opt_gc_data_", tolower(input_type)), .)
      )
    }
    gc_opt_comp_txt <- paste0(gc_opt_comp_txt_ls, collapse = ", ")

    res_disc_rmd <- gsub(
      "_gc-opt-comp-txt-comes-here_",
      gc_opt_comp_txt,
      res_disc_rmd
    )

  }

  return(res_disc_rmd) 

}

get_whole_pi_txt <- function(results) {

  whole_pi_t <- results$whole_profits_test[[1]][type_short == "ovg", t]

  if (whole_pi_t > 1.96) {

    text_summary <- "The data and model provide a high degree of statistical confidence in this result"

  } else if (whole_pi_t > 1.3) {

    text_summary <- "The data and model provide a moderate degree of statistical confidence in this result"

  } else {
    
    text_summary <- "But, the data and model provide a low degree of statistical confidence in this result"

  }

  return(text_summary)

}


get_td_text <- function(input_type, gc_type, locally_run = FALSE) {

  td_rmd_file <- case_when(
    input_type == "N" ~ "Report/ri03_trial_design_N.Rmd",
    input_type == "S" ~ "Report/ri03_trial_design_S.Rmd"
  )

  td_rmd <- read_rmd(td_rmd_file, locally_run = locally_run)

  if (gc_type == "Rx") {
    grower_plan_text <- "follow the commercial prescription depicted 
      in figure \\\\@ref(fig:rx-input-map)" %>% 
      gsub("input", input_type)
  } else if (gc_type == "uniform") {
    grower_plan_text <- "apply grower_chosen_rate_hereK seeds per acre 
      uniformly across the field. numb_seed_rates_here 
      experimental seed rates were assigned randomly and in 
      roughly equal number to plots" 
  }

  td_rmd <- gsub("_grower-plan-here_", grower_plan_text, td_rmd)

  return(td_rmd)    

}

prepare_e02_rmd <- function(input_type, process, use_td, locally_run = FALSE){

  if (process & !use_td) {

    return_rmd <- read_rmd(
      "DataProcessing/e02_process_as_applied_base.Rmd", 
      locally_run = locally_run
    ) %>% 
    gsub("input_type_here", input_type, .) %>% 
    gsub(
      "as-applied-file-name-here", 
      paste0("as-applied-", tolower(input_type)), 
      .
    )

  } else if (process & use_td){

    return_rmd <- read_rmd(
      "DataProcessing/e02_use_td.Rmd", 
      locally_run = locally_run
    ) %>% 
    gsub("input_type_here", input_type, .)


  } else {

    return_rmd <- NULL

  }

  return(return_rmd)
}



