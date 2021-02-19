#/*=================================================*/
#' # Define functions
#/*=================================================*/
get_seed <- function(opt_gc_data_s, c_type, w_zone){
  opt_gc_data_s[type == c_type & zone_txt == paste0("Zone ", w_zone), seed_rate] %>% round(digits = 0)
}

get_pi <- function(opt_gc_data_s, c_type, w_zone){
  opt_gc_data_s[type == c_type & zone_txt == paste0("Zone ", w_zone), profit_hat] %>% round(digits = 2)
}

get_t_value <- function(pi_dif_test_zone_s, w_zone){
  pi_dif_test_zone_s[zone_txt == paste0("Zone ", w_zone), t] %>% 
    round(digits = 2)
}

#/*----------------------------------*/
#' ## Read in all the analysis results 
#/*----------------------------------*/
results <- readRDS(here("Reports", "Growers", ffy, "analysis_results_s.rds"))

data_sf_s <- results$data_sf[[1]]
ys_by_char_s <- results$ys_by_char[[1]]
eval_data_s <- results$eval_data[[1]]
opt_gc_data_s <- results$opt_gc_data[[1]]
pi_data_indiv_sf <- results$pi_data_indiv_sf[[1]]
whole_profits_test_s <- results$whole_profits_test[[1]]
pi_dif_test_zone_s <- results$pi_dif_test_zone[[1]]

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

#--- seed rate conversion ---#
if (any(trial_design$tgt_seed > 10000)){
  #--- convert to K ---#
  trial_design <- mutate(trial_design, tgt_seed = tgt_seed / 1000)
}

tgts_ls <- unique(trial_design$tgt_seed) 
tgts_ls <- tgts_ls[order(tgts_ls)]


#/*=================================================*/
#' # Figures and Tables
#/*=================================================*/

g_opt_s <- data_sf_s %>% 
  mutate(opt_s = round(opt_s)) %>% 
  ggplot(data = .) +
    geom_sf(aes(fill = factor(opt_s)), size = 0.1) +
  scale_fill_brewer(
    palette = "Greens",
    direction = 1,
    name = "Optimal Seed Rate \n (1000 seeds)"
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
sum_tab_s <- copy(opt_gc_data_s) %>% 
  .[order(zone_txt, type),] %>% 
  .[, .(zone_txt, type, yield_hat, seed_rate)] %>% 
  .[, type := case_when(
    type == "gc" ~ "Grower-chosen",
    type == "opt_v" ~ "Optimal site-specific",
  )] %>% 
  .[, seed_rate := round(seed_rate, digits = 1)] %>% 
  .[, yield_hat := round(yield_hat, digits = 1)] %>% 
  setnames(names(.), c("Zone", "Strategy", "Yield (bu/acre)", "Seed Rate (1000 seeds)")) %>% 
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

g_whole_pi_s <- ggplot(data = whole_profits_test_s) +
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
  tm_tgts <- tm_shape(trial_design) +
    tm_fill(
      col = "tgt_seed", 
      palette = "Greens",
      title = "Targeted\nSeed Rate\n(1000 seeds)",
      # legend.is.portrait = FALSE,
      style = "cat"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )

} else {

  tm_tgts <- tm_shape(trial_design) +
    tm_fill(
      col = "tgt_seed", 
      palette = "Greens",
      title = "Targeted Seed Rate (1000 seeds)",
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
  tm_asp <- tm_shape(asp_plot_data) +
    tm_fill(
      col = "seed_rate", 
      palette = "Greens",
      title = "As-planted\nSeed Rate\n(1000 seeds)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )

} else {
 tm_asp <- tm_shape(asp_plot_data) +
  tm_fill(
    col = "seed_rate", 
    palette = "Greens",
    title = "As-planted Seed Rate (1000 seeds)",
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
  tm_p_seed <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "seed_rate", 
      palette = "YlGn",
      title = "Seed Rate\n(1000 seeds)",
      # legend.is.portrait = FALSE,
      style = "cont"
    ) +
    tm_layout_to_add + 
    tm_layout(
      # legend.outside.position = "bottom",
      legend.position = c(0.25, 0.25)
    )
} else {
  tm_p_seed <- tm_shape(analysis_data_td) +
    tm_fill(
      col = "seed_rate", 
      palette = "YlGn",
      title = "Seed Rate (1000 seeds)",
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

data_sf_s_td <- trial_design %>% 
  filter(type != "Headland") %>% 
  data_sf_s[., ]

if (sn_ew_ratio > 1.1) {
  tm_beta_s <- tm_shape(data_sf_s_td) +
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
  tm_beta_s <- tm_shape(data_sf_s_td) +
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
  tm_zone_s <- tm_shape(data_sf_s_td) +
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
  tm_zone_s <- tm_shape(data_sf_s_td) +
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
      data_sf_s, 
      seed_rate >= min(eval_data_s$seed_rate),
      seed_rate <= max(eval_data_s$seed_rate),
    ), 
    aes(y = yield, x = seed_rate),
    size = 0.4
  ) +
  geom_line(
    data = filter(eval_data_s, type == "opt_v"),
    aes(y = yield_hat, x = seed_rate)
  ) +
  geom_ribbon(
    data = filter(eval_data_s, type == "opt_v"),
    aes(
      ymin = yield_hat - yield_hat_se * 1.96, 
      ymax = yield_hat + yield_hat_se * 1.96, 
      x = seed_rate
    ),
    fill = "blue",
    alpha = 0.3
  ) +
  facet_grid(. ~ zone_txt) +
  xlab("Seed Rate (1000 seeds)") +
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
