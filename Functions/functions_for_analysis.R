
get_ttest_text <- function(test_results, zone){

  t <- test_results[zone_txt == paste0("Zone ", zone), t]

  if (t < 1.30){
    temp_text <- "The data and model provide negligible evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K does not provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.30 <= t & t < 1.64){
    temp_text <- "The data and model provide only limited evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.64 <= t & t < 1.96){
    temp_text <- "The data and model provide moderate evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else {
    temp_text <- "The data and model provide strong evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)." 
  }

  return(gsub("zone", zone, temp_text))
}

#/*=================================================*/
#' # Yield and Profit Predictions
#/*=================================================*/

# est <- data_analysis_s$gam_res[[1]]
# data <- data_analysis_s$data[[1]]
# var_name <- "seed_rate"

# data_dt[, n_rate := seq(1:nrow(data_dt))]

# all_var_names_ls_ls <- c(all_var_names_ls_ls, "n_rate")
# est$model$n_rate <- 1

# trial_type <- "S"

predict_yield_pi <- function(data, est, var_name, by = NULL) {

  data_dt <- data.table(data)

  if (is.null(by)) {

    var_names_ls <- est$model %>% 
      data.table() %>% 
      dplyr::select(- any_of(c(var_name, "yield"))) %>%
      names() 

    other_vars_exp <- paste0(var_names_ls, " = mean(data_dt$", var_names_ls, ")", collapse = ",")

    var_interest <- data_dt[, ..var_name] %>% 
      .[, lapply(.SD, function(x) 
        seq(
          quantile(x, prob = 0.025), 
          quantile(x, prob = 0.975), 
          length = 100
        )
      )] %>% 
      expand.grid() %>% 
      data.table()

    eval(parse(text=
      paste0("eval_data_row <- var_interest %>%  mutate(", other_vars_exp, ")")
    ))
    
    eval_data <- eval_data_row %>% 
      data.table()  

    yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

    #--- predict yield ---#
    eval_data[, yield_hat := yield_prediction$fit]
    eval_data[, yield_hat_se := yield_prediction$se.fit]

    if (all(c("seed_rate", "n_rate") %in% c(var_name))) {
      eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate - n_price * n_rate]
    } else if ("seed_rate" %in% c(var_name)) {
      eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate]
    } else {
      eval_data[, profit_hat := yield_hat * crop_price - n_price * n_rate]
    }

  } else {

    var_names_ls <- est$model %>% 
      data.table() %>% 
      dplyr::select(- any_of(c(var_name, "yield", by))) %>%
      names() 

    by_var_values <- unique(data_dt[, ..by]) %>% unlist()

    other_vars_exp <- paste0(var_names_ls, " = mean(data_dt$", var_names_ls, ")", collapse = ",")

    var_interest <- data_dt[, ..var_name] %>% 
      .[, lapply(.SD, function(x) 
        seq(
          quantile(x, prob = 0.025), 
          quantile(x, prob = 0.975), 
          length = 100
        )
      )] %>% 
      expand.grid() %>% 
      data.table() %>% 
      .[rep(seq_len(nrow(.)), each = length(by_var_values)),] %>% 
      .[, by_var := rep(by_var_values, nrow(.)/length(by_var_values))] %>% 
      data.table() %>% 
      setnames("by_var", by)

    eval(parse(text=
      paste0("eval_data_row <- var_interest %>%  mutate(", other_vars_exp, ")")
    ))
    
    eval_data <- eval_data_row %>% 
      data.table() 

    yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

    actual_mean_yield <- data_dt[, .(actual_m_yield = mean(yield)), by = by]

    #--- predict yield ---#
    eval_data <- eval_data %>% 
      .[, `:=`(
        yield_hat = yield_prediction$fit,
        yield_hat_se = yield_prediction$se.fit
      )] %>% 
      actual_mean_yield[, on = by] %>% 
      .[, mean_yield_hat := mean(yield_hat), by = by] %>% 
      .[, yield_hat := yield_hat + mean_yield_hat - actual_m_yield] %>% 
      .[, `:=`(
        actual_m_yield = NULL,
        mean_yield_hat = NULL
      )] 

    if (all(c("seed_rate", "n_rate") %in% c(var_name))) {
      eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate - n_price * n_rate]
    } else if ("seed_rate" %in% c(var_name)) {
      eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate]
    } else {
      eval_data[, profit_hat := yield_hat * crop_price - n_price * n_rate]
    }

  }

  eval_data[, profit_hat_se := crop_price * yield_hat_se] 

  return(eval_data)

}

predict_yield_pi_simple <- function(data, est, var_name) {

  eval_data <- copy(data)

  yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

  #--- predict yield ---#
  eval_data[, yield_hat := yield_prediction$fit]
  eval_data[, yield_hat_se := yield_prediction$se.fit]

  if (all(c("seed_rate", "n_rate") %in% c(var_name))) {
    eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate - n_price * n_rate]
  } else if ("seed_rate" %in% c(var_name)) {
    eval_data[, profit_hat := yield_hat * crop_price - seed_price * seed_rate]
  } else {
    eval_data[, profit_hat := yield_hat * crop_price - n_price * n_rate]
  }

  eval_data[, profit_hat_se := crop_price * yield_hat_se] 

  return(eval_data)

}

#/*=================================================*/
#' # Profit-differential test 
#/*=================================================*/
# Notes: test if the profit associated with the optimal and grower-chosen
# rates are statistically significantly different from zero 

get_dif_stat <- function(data, test_var, opt_var, gc_var, gam_res){
    
  
  base_data <- copy(data) %>% 
    .[, (test_var) := get(gc_var)]

  comp_data <- copy(data) %>% 
    .[, (test_var) := get(opt_var)]

  #/*----------------------------------*/
  #' ## Profit (gc)
  #/*----------------------------------*/
  Xmat_base <- predict(gam_res, newdata = base_data, type = "lpmatrix") 

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_base)[1], 1, dim(Xmat_base)[1])

  #--- average yield ---#
  yhat_base <- ones %*% Xmat_base %*% gam_res$coefficients

  #--- point estimate of profit differential ---#
  pi_gc <- crop_price * yhat_base - (seed_price * ones %*% base_data$seed_rate)  

  big_mat_base <- ones %*% Xmat_base

  #--- se of the profit differential  ---# 
  pi_gc_se <- crop_price * sqrt(big_mat_base %*% gam_res$Ve %*% t(big_mat_base))

  #/*----------------------------------*/
  #' ## Profit (optimal)
  #/*----------------------------------*/
  Xmat_comp <- predict(gam_res, newdata = comp_data, type = "lpmatrix") 

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_comp)[1], 1, dim(Xmat_comp)[1])

  #--- average yield ---#
  yhat_comp <- ones %*% Xmat_comp %*% gam_res$coefficients

  #--- point estimate of profit differential ---#
  pi_opt <- crop_price * yhat_comp - (seed_price * ones %*% comp_data$seed_rate)  

  big_mat_comp <- ones %*% Xmat_comp

  #--- se of the profit differential  ---# 
  pi_opt_se <- crop_price * sqrt(big_mat_comp %*% gam_res$Ve %*% t(big_mat_comp))

  #/*----------------------------------*/
  #' ## Profit differential
  #/*----------------------------------*/
  #--- difference in X mat ---#
  X_dif_mat <- Xmat_comp - Xmat_base

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(X_dif_mat)[1], 1, dim(X_dif_mat)[1])

  #--- X_dif_mat summed ---#
  big_mat_dif <- ones %*% X_dif_mat

  #--- point estimate of profit differential ---#
  pi_dif <- ones %*% ((crop_price * X_dif_mat %*% gam_res$coefficients) - seed_price * (comp_data$seed_rate - base_data$seed_rate))  

  #--- se of the profit differential  ---# 
  pi_dif_se <- crop_price * sqrt(big_mat_dif %*% gam_res$Ve %*% t(big_mat_dif))

  #--- t-stat ---#
  t_stat <- (pi_dif/pi_dif_se) %>% round(digits = 2)  

  return_data <- data.table(
    yhat_est_gc = yhat_base[1, 1],
    point_est_gc = pi_gc[1, 1],
    point_est_gc_se = pi_gc_se[1, 1],
    yhat_est_opt = yhat_comp[1, 1],
    point_est_opt = pi_opt[1, 1],
    point_est_opt_se = pi_opt_se[1, 1],
    point_est_dif = pi_dif[1, 1],
    point_est_dif_se = pi_dif_se[1, 1],
    t = t_stat[1, 1]
  )

  return(return_data)

}

get_dif_stat_zone <- function(data, test_var, opt_var, gc_var, gam_res, by_var){

  zone_ls <- data[, ..by_var] %>% 
    unique() %>% 
    unlist()

  temp_data <- setnames(copy(data), by_var, "by_var")

  # x <- zone_ls[1]

  return_data <- lapply(
    zone_ls,
    function(y) 
    get_dif_stat(
      data = temp_data[by_var == y, ] %>% 
        setnames("by_var", by_var),
      test_var,
      opt_var,
      gc_var,
      gam_res
    ) %>% 
    mutate(by_var = y)
  ) %>% 
  rbindlist() %>% 
  setnames("by_var", by_var)

  return(return_data)

}

find_opt_u <- function(data, var_name, gam_res) {

  data_dt <- data.table(data)

  input_rates <- data_dt[, ..var_name] %>% unlist()

  input_ls <- seq(
    quantile(input_rates, prob = 0.025), 
    quantile(input_rates, prob = 0.975), 
    length = 100
  )

  opt_input_u <- data_dt %>% 
  .[rep(1:nrow(.), length(input_ls)), ] %>% 
  .[, input_rate := rep(input_ls, each = nrow(.)/length(input_ls))] %>% 
  .[, yield_hat := predict(gam_res, newdata = .)] %>% 
  .[, profit_hat := crop_price * yield_hat - seed_price * seed_rate] %>% 
  .[, .(profit_hat = mean(profit_hat)), by = seed_rate] %>% 
  .[order(profit_hat), ] %>% 
  .[.N, seed_rate]

  return(opt_input_u)

} 

#/*=================================================*/
#' # GWR-analysis
#/*=================================================*/

# var_name <- "seed_rate"
# data_sf <- data

run_gwr <- function(data_sf, var_name) {

  reg_data_sp <- data_sf %>%
    as("Spatial")

  #--- regression formula ---#
  reg_formula <- formula(paste("yield ~ log(", var_name,")"))

  # library(matrixcalc)
  # is.singular.matrix(dMat)

  #--- find optimal bandwidth ---#
  dMat <- data_sf %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    as.matrix() %>% 
    gw.dist()

  obw <- bw.gwr(
    reg_formula,
    data = reg_data_sp,
    approach = "AICc",
    kernel = "gaussian",
    adaptive = T,
    dMat = dMat
  )

  #--- gwr estimation with optimal bw ---#
  gwr_est <- gwr.basic(
    reg_formula,
    data = reg_data_sp,
    # bw = 100,
    bw = obw * 3,
    kernel = "gaussian",
    adaptive = T
  )

  #--- join the coefficients to the sf ---#
  data_sf <- data.table(
    obs_id = data_sf$obs_id,
    b_int = gwr_est$SDF$Intercept,
    b_slope = gwr_est$SDF@data[, paste0("log(", var_name, ")")]
  ) %>%
    left_join(data_sf, ., by = "obs_id") 

  return(data_sf)

}


define_mz <- function(data_sf, max_num_zones, min_obs) {

  #--- number of zones ---#
  num_zones <- min(floor(nrow(data_sf) / min_obs), max_num_zones)

  #--- grouping the data into 5 groups based on beta ---#
  data_sf <- data_sf %>% 
  mutate(
    zone = cut(
      b_slope, 
      breaks = quantile(b_slope, prob = seq(0, 1, length = num_zones + 1)),
      include.lowest = TRUE
    )
  ) %>% 
  mutate(zone_txt = factor(paste0("Zone ", as.numeric(zone))))

}

#/*=================================================*/
#' # Create yield response functions by characteristic
#/*=================================================*/

make_ys_by_chars <- function(data_sf){

  #/*----------------------------------*/
  #' ## Get correlation table
  #/*----------------------------------*/
  vars_all <- names(data_sf) %>% 
    .[!str_detect(., "id")] %>% 
    .[!str_detect(., "yield")] %>% 
    .[!str_detect(., "ID")] %>% 
    .[!str_detect(., "geometry")] %>% 
    .[!str_detect(., "b_int")] %>% 
    .[!str_detect(., "gc_rate")] %>% 
    .[!str_detect(., "zone")] %>% 
    .[!str_detect(., "opt_")] %>% 
    .[. != "seed_rate"] %>% 
    .[. != "X"] %>% 
    .[. != "Y"]  

  cor_tab <- data_sf[, vars_all, with = FALSE] %>% 
    dplyr::select(where(is.numeric)) %>% 
    st_drop_geometry() %>% 
    cor(use = "complete.obs") %>% 
    .[, "b_slope", drop = FALSE] %>% 
    .[!(rownames(.) %in% c("b_slope")), , drop = FALSE]

  # g_cor <- cor_tab %>%  
  #   ggcorrplot(
  #     lab = TRUE,
  #     lab_size = 7
  #   ) +
  #   theme(
  #     axis.text.x = element_text(size = 16),
  #     axis.text.y = element_text(size = 16),
  #     legend.text = element_text(size = 16)
  #   )

  #/*----------------------------------*/
  #' ## Create maps and yield response figures
  #/*----------------------------------*/
  vars_plot_ls <- rownames(cor_tab)[abs(cor_tab) >= 0.2] %>% 
    .[!str_detect(., "yield")]

  vars_plot_ls_len <- length(vars_plot_ls)

  if (vars_plot_ls_len > 0) { # if any variable > 0.2
    
    plot_ls <- list()
    map_ls <- list()

    # var_p <- "ecd"

    for (var_p in vars_plot_ls){

      temp_data <- data_sf[, c("yield", "seed_rate", var_p)] %>% 
        setnames(var_p, "var_temp") %>% 
        filter(!is.na(var_temp))

      if (!str_detect(var_p, "ss")){

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = cut(
              var_temp, 
              breaks = quantile(
                var_temp, 
                prob = seq(0, 1, length = 4)
              ),
              include.lowest = TRUE
            )
          )

        g_map <- ggplot(data_sf) +
          geom_sf(aes_string(fill = var_p), color = NA) +
          theme_void() +
          scale_fill_distiller(
            palette = "YlOrRd",
            direction = -1
          ) + 
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 12),
            legend.key.width =  unit(1, "cm"),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            plot.margin = unit(c(0, 2, 0, 0), "cm") 
          )

        map_ls[[var_p]] <- g_map

      } else {

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = ifelse(var_temp < 0.5, 0, 1)
          )

      }

      plot_ls[[var_p]] <- ggplot(data = temp_data) +
        geom_point(aes(y = yield, x = seed_rate, color = factor(temp_cat)), size = 0.3) +
        geom_smooth(
          aes(
            y = yield, 
            x = seed_rate, 
            color = factor(temp_cat)
          ),
          method = "gam",
          formula = y ~ s(x, k = 3)
        ) +
        theme_bw() +
        scale_color_discrete(name = var_p) +
        ylab("Yield (bu/acre)") +
        xlab("Seed Rate (1000 seeds)") +
        theme_bw() +
        theme(
          legend.position = c(0.3, 0.2),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9)
        ) 

    }

    if(str_detect(var_p, "ss")){
      ssurgo_data <- readRDS("Intermediate/ssurgo.rds") %>%
        setnames(names(.), tolower(names(.)))

      ss_var <- gsub("ss_", "", var_p)

      g_map <- ggplot() +
        geom_sf(
          data = ssurgo_data,
          aes(fill = musym)
        ) + 
        theme_void()

      map_ls[["ssurgo"]] <- g_map
    }
    

    if (vars_plot_ls_len == 1){
      g_all <- plot_ls[[1]]
    } else if (vars_plot_ls_len == 2){
      g_all <- plot_ls[[1]] | plot_ls[[2]]
    } else if (vars_plot_ls_len == 3){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_spacer())
    } else if (vars_plot_ls_len >= 4){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_ls[[4]])
    }   

    if (length(map_ls) == 1){
      g_all_map <- map_ls[[1]]
    } else if (length(map_ls) == 2){
      g_all_map <- map_ls[[1]] | map_ls[[2]]
    } else if (length(map_ls) == 3){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | (ggplot() + theme_void()))
    } else if (length(map_ls) >= 4){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | map_ls[[4]])
    } 


  } else {
    g_all <- NULL
    g_all_map <- NULL
  }

  return(list(g_all, g_all_map))

}

expand_grid_df <- function(data_1, data_2) {

  expanded_data <- expand.grid(
    index_1 = seq_len(nrow(data_1)),
    index_2 = seq_len(nrow(data_2))
  ) %>% 
  tibble() %>% 
  rowwise() %>% 
  mutate(
    data = list(
      cbind(
        slice(data.table(data_1), index_1),
        slice(data.table(data_2), index_2)
      )
    )
  ) %>% 
  select(data) %>% 
  ungroup() %>% 
  .$data %>% 
  rbindlist() %>% 
  tibble()

  return(expanded_data)

}

get_seed <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), seed_rate] %>% round(digits = 0)
}

get_pi <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), profit_hat] %>% round(digits = 2)
}

get_t_value <- function(test_data, w_zone){
  test_data[zone_txt == paste0("Zone ", w_zone), t] %>% 
    round(digits = 2)
}  

 
