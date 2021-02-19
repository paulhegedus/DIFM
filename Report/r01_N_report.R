#/*=================================================*/
#' # Define functions
#/*=================================================*/
get_nitrogen <- function(opt_gc_data_n, c_type, w_zone){
  opt_gc_data_n[type == c_type & zone_txt == paste0("Zone ", w_zone), n_rate] %>% round(digits = 0)
}

get_pi <- function(opt_gc_data_n, c_type, w_zone){
  opt_gc_data_n[type == c_type & zone_txt == paste0("Zone ", w_zone), profit_hat] %>% round(digits = 2)
}

get_t_value <- function(pi_dif_test_zone_n, w_zone){
  pi_dif_test_zone_n[zone_txt == paste0("Zone ", w_zone), t] %>% 
    round(digits = 2)
}

#/*----------------------------------*/
#' ## Read in all the analysis results 
#/*----------------------------------*/
results <- readRDS(here("Reports", "Growers", ffy, "analysis_results_n.rds"))

data_sf_n <- results$data_sf[[1]]
ys_by_char_n <- results$ys_by_char[[1]]
eval_data_n <- results$eval_data[[1]]
opt_gc_data_n <- results$opt_gc_data[[1]]
pi_data_indiv_sf <- results$pi_data_indiv_sf[[1]]
whole_profits_test_n <- results$whole_profits_test[[1]]
pi_dif_test_zone_n <- results$pi_dif_test_zone[[1]]

#/*----------------------------------*/
#' ## Get the aspect ratio of the field
#/*----------------------------------*/
field_bbox <- st_bbox(analysis_data)

sn_length <- field_bbox["ymax"] - field_bbox["ymin"]
ew_length <- field_bbox["xmax"] - field_bbox["xmin"]

sn_ew_ratio <- sn_length / ew_length

#/*----------------------------------*/
#' ## Trial design information
#/*----------------------------------*/
dict_td <- dictionary[type == "trial_design", ]
col_list <- dict_td[, column]

trial_design <- make_var_name_consistent(
  trial_design, 
  dict_td 
)

#--- nitrogen rate conversion ---#
trial_design <- mutate(
  trial_design, 
  tgt_n = convert_N_unit(
    input_data_n$form, 
    input_data_n$unit, 
    tgt_n, 
    field_data$reporting_unit
  )
)

tgtn_ls <- unique(trial_design$tgt_n) 
tgtn_ls <- tgtn_ls[order(tgtn_ls)]


#/*=================================================*/
#' # Figures and Tables
#/*=================================================*/

g_opt_n <- data_sf_n %>% 
  mutate(opt_n = round(opt_n)) %>% 
  ggplot(data = .) +
    geom_sf(aes(fill = factor(opt_n)), size = 0.1) +
  scale_fill_brewer(
    palette = "Greens",
    direction = 1,
    name = "Optimal Nitrogen Rate \n (lbs)"
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  ) +
  fig_theme_sp

#/*----------------------------------*/
#' ## summary table
#/*----------------------------------*/
sum_tab_n <- copy(opt_gc_data_n) %>% 
  .[order(zone_txt, type),] %>% 
  .[, .(zone_txt, type, yield_hat, n_rate)] %>% 
  .[, type := case_when(
    type == "gc" ~ "Grower-chosen",
    type == "opt_v" ~ "Optimal site-specific",
  )] %>% 
  .[, n_rate := round(n_rate, digits = 1)] %>% 
  .[, yield_hat := round(yield_hat, digits = 1)] %>% 
  setnames(names(.), c("Zone", "Strategy", "Yield (bu/acre)", "Nitrogen Rate")) %>% 
  flextable() %>% 
  font(fontname = "Times", part = "all") %>% 
  hline(
    i = c(2, 4, 6, 8),
    border = fp_border(
      color = "black",
      width = 1
    )
  ) %>% 
  merge_v(
    j = 1
  ) %>% 
  align(
    align = "center",
    part = "all"
  ) %>% 
  fontsize(
    size = 12,
    part = "all"
  ) %>% 
  autofit() %>% 
  fix_border_issues()  


#/*----------------------------------*/
#' ## Whole-field profit comparison 
#/*----------------------------------*/
g_whole_pi_n <- ggplot(data = whole_profits_test_n) +
  geom_errorbar(
    aes(
    ymin = point_est_dif - point_est_dif_se * 1.96, 
    ymax = point_est_dif + point_est_dif_se * 1.96, 
    x = type
    ),
    size = 0.8
  ) +
  geom_point(
    aes(
      y = point_est_dif,
      x = type
    ),
    size = 4,
    color = "red"
  ) +
  geom_hline(yintercept = 0, color = "red", size = 1.5) +
  theme_bw() +
  ylab("Difference in Net Revenue ($/acre)") +
  xlab("") +
  theme(
    axis.title.x = element_text(size = 12, vjust = -1),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  ) +
  fig_theme_nonsp

#/*----------------------------------*/
#' ## Trial-design
#/*----------------------------------*/
if (sn_ew_ratio > 1.1) {
  tm_tgtn <- tm_shape(trial_design) +
    tm_fill(
      col = "tgt_n", 
      palette = "Greens",
      title = "Targeted\nNitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cat"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )

} else {

  tm_tgtn <- tm_shape(trial_design) +
    tm_fill(
      col = "tgt_n", 
      palette = "Greens",
      title = "Targeted Nitrogen Rate (lbs)",
      legend.is.portrait = FALSE,
      style = "cat"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )

}

#/*----------------------------------*/
#' ## As-planted Seed Rate
#/*----------------------------------*/

asp_plot_data <- trial_design %>% 
  filter(type != "Headland") %>% 
  as_applied_s[., ]

if (sn_ew_ratio > 1.1) {
  tm_aan <- tm_shape(asp_plot_data) +
    tm_fill(
      col = "n_rate", 
      palette = "Greens",
      title = "As-applied\nNitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )

} else {
 tm_aan <- tm_shape(asp_plot_data) +
  tm_fill(
    col = "n_rate", 
    palette = "Greens",
    title = "As-applied Nitrogen Rate (lbs)",
    legend.is.portrait = FALSE,
    style = "cont"
  ) +
  tm_layout_to_add + 
  tm_layout(
    legend.outside.position = "bottom",
    legend.position = c(0.25, 0.25)
  )
 
}

#/*----------------------------------*/
#' ## Raw yield
#/*----------------------------------*/
ry_plot_data <- trial_design %>% 
  filter(type != "Headland") %>% 
  yield_polygons[., ]

if (sn_ew_ratio > 1.1) {
  tm_ry <- tm_shape(filter(ry_plot_data, flag_bad == 0)) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_ry <- tm_shape(filter(ry_plot_data, flag_bad == 0)) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield \n (bu/acre)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}
 
#/*----------------------------------*/
#' ## Yield (processed)
#/*----------------------------------*/

analysis_data_td <- trial_design %>% 
  filter(type != "Headland") %>% 
  analysis_data[., ]

if (sn_ew_ratio > 1.1) {
  tm_p_yield <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_p_yield <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "yield_vol", 
      palette = "YlOrBr",
      title = "Yield (bu/acre)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}

#/*----------------------------------*/
#' ## Seed rate (processed)
#/*----------------------------------*/
if (sn_ew_ratio > 1.1) {
  tm_p_nitrogen <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "n_rate", 
      palette = "YlGn",
      title = "Nitrogen Rate\n(lbs)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_p_nitrogen <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "n_rate", 
      palette = "YlGn",
      title = "Nitrogen Rate (lbs)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}


#/*----------------------------------*/
#' ## Management zone
#/*----------------------------------*/

data_sf_n_td <- trial_design %>% 
  filter(type != "Headland") %>% 
  data_sf_n[., ]

if (sn_ew_ratio > 1.1) {
  tm_beta_n <- tm_shape(data_sf_n_td) +
    tm_fill(
      col = "b_slope", 
      palette = "PuRd",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_beta_n <- tm_shape(data_sf_n_td) +
    tm_fill(
      col = "b_slope", 
      palette = "PuRd",
      title = "Yield (bu/acre)",
      legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}

if (sn_ew_ratio > 1.1) {
  tm_zone_n <- tm_shape(data_sf_n_td) +
    tm_fill(
      col = "zone_txt", 
      palette = "YlGnBu",
      title = "Yield\n(bu/acre)",
      # legend.is.portrait = FALSE
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_zone_n <- tm_shape(data_sf_n_td) +
    tm_fill(
      col = "zone_txt", 
      palette = "YlGnBu",
      title = "Yield (bu/acre)",
      legend.is.portrait = FALSE
    ) +
    tm_layout_to_add + 
    tm_layout(
      legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
}

#/*----------------------------------*/
#' ## Yield response function by zone
#/*----------------------------------*/
g_ys_zone_s <- ggplot() +
  geom_point(
    data = filter(
      data_sf_n, 
      n_rate >= min(eval_data_n$n_rate),
      n_rate <= max(eval_data_n$n_rate),
    ), 
    aes(y = yield, x = n_rate),
    size = 0.4
  ) +
  geom_line(
    data = filter(eval_data_n, type == "opt_v"),
    aes(y = yield_hat, x = n_rate)
  ) +
  geom_ribbon(
    data = filter(eval_data_n, type == "opt_v"),
    aes(
      ymin = yield_hat - yield_hat_se * 1.96, 
      ymax = yield_hat + yield_hat_se * 1.96, 
      x = n_rate
    ),
    fill = "blue",
    alpha = 0.3
  ) +
  facet_grid(. ~ zone_txt) +
  xlab("Nitrogen Rate (lbs)") +
  ylab("Yield (bu/acre)") +
  theme(
    axis.title.x = element_text(size = 12, vjust = -1),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  fig_theme_nonsp

#/*----------------------------------*/
#' ## Interaction explanation
#/*----------------------------------*/
gen_yield_chigh <- function(x) {
  80 + 50 * log(x - 20)
}

gen_yield_clow <- function(x) {
  -30 + 90 * log(x - 25)
}

plot_data <- data.table(x = seq(27, 40, length =1000)) %>% 
  .[,y_chigh := gen_yield_chigh(x)] %>% 
  .[,y_clow := gen_yield_clow(x)] %>% 
  melt(id.var = "x") %>% 
  .[, type := case_when(
    variable == "y_chigh" ~ "c_high",
    variable == "y_clow" ~ "c_low"
  )]

g_int <- ggplot(plot_data) +
  geom_line(aes(y = value, x = x, color = type)) +
  labs(x = 'Seed Rate', y ='Yield') +
  geom_hline(yintercept = 27) +
  geom_vline(xintercept = 27) +
  scale_color_manual(
    values = c(
      "c_high" = "#000000", 
      "c_low" = "#C87700"
    )
  ) +
  annotate(
    'text',
    x = 32, y = 218,
    parse = TRUE, 
    label = ('f(seed, c^{high})'),
    color = "#000000",
    size = 4,
    family = "Times",
    fontface = 3
  ) +
  annotate('text',
    x = 37,
    y = 164,
    parse = TRUE,
    label = 'f(seed, c^{low})',
    color = "#C87700",
    size = 4,
    family = "Times",
    fontface = 3
  ) +
  theme(
    legend.position = "none",
  ) +
  fig_theme_nonsp

