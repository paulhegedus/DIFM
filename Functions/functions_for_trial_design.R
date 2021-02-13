# /*=================================================*/
#' # Make experiment grids (basic cell, plot, strip)
# /*=================================================*/
# This codes relies on ab_line

make_trial_grids <- function(field, ab_line, plot_width, cell_height, headland_length) {

  # /*=================================================*/
  #' # Define functions
  # /*=================================================*/
  # /*----------------------------------*/
  #' ## make polygons
  # /*----------------------------------*/
  make_polygon <- function(strt_point_new, multiplier, dir_p, dir_v) {

    point_1 <- strt_point_new + cell_height * dir_v * ab_xy_nml * (multiplier - 1)
    point_2 <- point_1 - plot_width * dir_p * ab_xy_nml_p90
    point_3 <- point_2 + dir_v * ab_xy_nml * cell_height
    point_4 <- point_3 + plot_width * dir_p * ab_xy_nml_p90

    temp_polygon <- rbind(
      point_1,
      point_2,
      point_3,
      point_4,
      point_1
    ) %>%
      list() %>%
      st_polygon()

    return(temp_polygon)
  }
  # /*----------------------------------*/
  #' ## rotation matrix
  # /*----------------------------------*/
  rotate_mat_p90 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #/*----------------------------------*/
  #' ## Create plots
  #/*----------------------------------*/
  detect_directions <- function(strt_point, dir_p, dir_v, num_subplots) {

    is_intersecting <- rep(TRUE, 100)
    
    exp_sf_ls <- list()

    # group <- 1
    for (group in 1:100) {

      # print(group)

      exp_sf_ls[[paste(group)]] <- lapply(
        1:num_subplots,
        function(x) {
          make_polygon(
            strt_point + plot_width * dir_p * ab_xy_nml_p90 * (group - 1),
            x,
            dir_p,
            dir_v
          )
        }
      ) %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field)) %>%
      st_as_sf() %>%
      mutate(
        group = group,
        id = 1:nrow(.)
      )

      is_intersecting[group] <- st_intersects(exp_sf_ls[[paste(group)]], field, sparse = F)[, 1] %>% any()

      if (is_intersecting[group]) {
        return(TRUE)
      } else if (all(!is_intersecting[1:50])) {
        return(FALSE)
      }
    }
  }

  create_plots <- function(strt_point, dir_p, dir_v, num_subplots){

    is_intersecting <- rep(TRUE, 1000)
    
    exp_sf_ls <- list()

    # group <- 1
    for (group in 1:1000) {

      # print(group)

      exp_sf_ls[[paste(group)]] <- lapply(
        1:num_subplots,
        function(x) {
          make_polygon(
            strt_point + plot_width * dir_p * ab_xy_nml_p90 * (group - 1),
            x,
            dir_p,
            dir_v
          )
        }
      ) %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field)) %>%
      st_as_sf() %>%
      mutate(
        group = group,
        id = 1:nrow(.)
      )

      is_intersecting[group] <- st_intersects(exp_sf_ls[[paste(group)]], field, sparse = F)[, 1] %>% any()

      min_intersecting_group <- which(is_intersecting) %>% min()

      if(group > 20){
        if (!is_intersecting[group - 10] & ((group - 10) > min_intersecting_group)) {
          all_plygons <- do.call("rbind", exp_sf_ls) %>% 
            .[st_buffer(field, 5 * plot_width), ] %>% 
            rename(geometry = x)
          return(all_plygons)
        } else if (all(!is_intersecting[1:50])) {
          return(NULL)
        }
      }
    } 
  }

  # /*----------------------------------*/
  #' ## vector of points of sf of points
  # /*----------------------------------*/
  vect_to_sf_point <- function(vec) {
    st_as_sfc(list(st_point(vec))) %>%
      st_set_crs(st_crs(field))
  }

  # /*----------------------------------*/
  #' ## Re-assign plot id based on observation numbers per plot
  # /*----------------------------------*/
  reassign_plot_id <- function(data, grp) {

    if (max(data$plot_id) == 1) {
      #--- if there is only one plot_id in the strip ---#
      return(data[, .(id, plot_id, group, group_in_group, geometry, type)])
    }

    if (nrow(data[too_short == TRUE, ]) == 0) {
      return(data[, .(id, plot_id, group, group_in_group, geometry, type)])
    }

    num_obs_short <- data[too_short == TRUE, obs_per_plot] %>%
      unique()

    short_plot_id <- data[too_short == TRUE, plot_id] %>%
      unique()

    num_obs_short_1 <- data[plot_id == (short_plot_id - 1), obs_per_plot] %>%
      unique()

    if (num_obs_short >= (2 * min_obs - mean_obs)) { # make the last two short

      first_obs_set <- ceiling((num_obs_short + mean_obs) / 2)

      data[plot_id %in% c(short_plot_id, short_plot_id - 1), cum_num_reassign := cumsum(dummy)] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 1]
    } else if ((max(data$plot_id) >= 3) & num_obs_short >= (3 * min_obs - 2 * mean_obs)) {

      # make the last three short (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 3)

      data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        #--- third last ---#
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2] %>%
        .[cum_num_reassign > first_obs_set & cum_num_reassign <= 2 * first_obs_set, plot_id := short_plot_id - 1]
    } else if (max(data$plot_id) >= 3) {

      # make the 2nd and 3rd last longer (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 2)

      data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id - 1] %>%
        #--- third last ---#
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2]
    } else {

      # make the two into one (there needs to be at least 2 plot ids)
      data[, plot_id := 1]
    }

    # data[, .N, by = plot_id]

    # return(data[, .(id, plot_id, group_contiguous, x)])
    return(data[, .(id, plot_id, group, group_in_group, geometry, type)])
  }

  #/*----------------------------------*/
  #' ## Calculate the distance 
  #/*----------------------------------*/
  # Calculate the distance between a strip of polygons and the ab_line

  cal_dist_to_ab <- function(data_sf, ab_int_group) {

    centroids <- data_sf %>% 
      st_centroid() %>% 
      .[c(1, nrow(.)), ] %>% 
      st_geometry()

    line <- list(st_linestring(c(centroids[[1]], centroids[[2]]))) %>% 
      st_as_sfc() %>% 
      st_set_crs(st_crs(field))

    correction_dist <- st_distance(line, ab_line) %>% 
      as.numeric()

    return(correction_dist)
  }
 
  # /*=================================================*/
  #' # Main code
  # /*=================================================*/
  #--- get the vector (direction machines run)  ---#
  ab_xy <- st_geometry(ab_line)[[1]][2, ] - st_geometry(ab_line)[[1]][1, ]
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  # /*----------------------------------*/
  #' ## identify the number of subplots in a strip
  # /*----------------------------------*/
  f_bbox <- st_bbox(field)

  #--- maximum distance ---#
  max_dist <- sqrt(
    (f_bbox["xmax"] - f_bbox["xmin"])^2 +
      (f_bbox["ymax"] - f_bbox["ymin"])^2
  ) + 50

  max_dist_cover <- ceiling(max_dist / 10) * 10

  #--- number of subplots to create ---#
  num_subplots_in_a_strip <- ceiling(max_dist_cover / cell_height)

  # /*----------------------------------*/
  #' ## Detect which direction to go
  # /*----------------------------------*/
  starting_point <- c(f_bbox["xmin"] - 100, f_bbox["ymin"] - 100) 

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### which direction (perpendicular to the ab-line) 
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  print("Detecting the direction to go in")

  plots_ls <- expand.grid(dir_p = c(-1, 1), dir_v = c(-1, 1)) %>% 
  data.table() %>% 
  .[,
    keep := map2(dir_p, dir_v, ~
      detect_directions(
        strt_point = starting_point,
        dir_p = .x,
        dir_v = .y,
        num_subplots = 100
      )
    )
  ] %>% 
  .[keep == TRUE, ]  

  dir_p <- plots_ls[, dir_p]
  dir_v <- plots_ls[, dir_v]

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Create the full plots
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  print("Creating the full polygons")

  plots <- create_plots(
    strt_point = starting_point,
    dir_p = dir_p,
    dir_v = dir_v,
    num_subplots = num_subplots_in_a_strip
  ) 

  # ggplot() +
  #   geom_sf(data = plots, col = "blue") +
  #   geom_sf(data = vect_to_sf_point(starting_point), col = "green") +
  #   geom_sf(data = field, col = "blue") +
  #   geom_sf(data = ab_line, col = "red")

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Cut off unnecessary parts
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  

  # min_group <- min(plots$group)
  # max_group <- max(plots$group)

  # keep_to_the_left <- (ab_int_group - min_group) > (max_group- ab_int_group) 

  # if (keep_to_the_left) {
  #   plots_kept <- filter(plots, group <= ab_int_group) %>% 
  #     mutate(group = max(group) - group + 1) %>% 
  #     mutate(unique_id := paste0(group, "_", id))
  # } else {
  #   plots_kept <- filter(plots, group >= ab_int_group) %>% 
  #     mutate(group = group - min(group) + 1) %>% 
  #     mutate(unique_id := paste0(group, "_", id))
  # }

  # ggplot() +
  #   geom_sf(data = plots_kept, col = "blue") +
  #   geom_sf(data = ab_line, col = "red")

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Shift the polygons for the right starting point
  #/*~~~~~~~~~~~~~~~~~~~~~~*/   
  print("Shifting the polygons for the right starting point")

  ab_int_group <- st_intersection(plots, ab_line) %>% 
    pull(group) %>% unique()

  int_group <- filter(plots, group == ab_int_group)
  correction_dist <- cal_dist_to_ab(int_group, ab_int_group)

  int_group_corrected <- st_shift(int_group, correction_dist * ab_xy_nml_p90)

  if (cal_dist_to_ab(int_group_corrected, ab_int_group) > correction_dist) {
    #--- if moved further away ---#
    plots_shifted <- st_shift(plots, - (plot_width/2 + correction_dist) * ab_xy_nml_p90) %>% 
      mutate(unique_id := paste0(group, "_", id))
  } else {
    #--- if get close ---#
    plots_shifted <- st_shift(plots, (plot_width/2 + correction_dist) * ab_xy_nml_p90) %>% 
      mutate(unique_id := paste0(group, "_", id))
  }
  
  # ggplot() +
  #   geom_sf(data = plots_shifted, col = "blue") +
  #   geom_sf(data = ab_line, col = "red")

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Remove all the non-intersecting (or almost)
  #/*~~~~~~~~~~~~~~~~~~~~~~*/  
  print("Removing all the non-intersecting grids")

  keep_ids <- st_intersection(plots_shifted, field) %>% 
    mutate(area = as.numeric(st_area(.))) %>% 
    data.table() %>% 
    .[, area := sum(area), by = .(group, id)] %>% 
    .[area > 0.2 * (plot_width * cell_height), ] %>% 
    .[, unique_id]

  how_many_cells_in <- ceiling(headland_length / cell_height)  

  plots_intersecting <- filter(plots_shifted, unique_id %in% keep_ids) %>% 
    data.table() %>% 
    #--- id starting from 1 from each group---#
    .[, id := id - min(id) + 1, by = group] %>% 
    #--- remove the first and last `how_many_cells_in`  ---#
    .[, type := "experiment"] %>% 
    .[id <= how_many_cells_in, type := "headland", by = group] %>% 
    .[, id_threshold_up := max(max(id) - how_many_cells_in, 0), by = group] %>% 
    .[id > id_threshold_up, type := "headland", by = group] %>% 
    # .[, .SD[id > how_many_cells_in &  id <= max(id) - how_many_cells_in, ], by = group] %>% 
    .[type != "headland",] %>% 
    st_as_sf() 

  # filter(plots_intersecting, group == 47)$id %>% max()
  # filter(plots_intersecting, group == 47)$type

  # ggplot() +
  #   # geom_sf(data = filter(plots_intersecting, group == 47), aes(fill = type)) +
  #   geom_sf(data = plots_intersecting, aes(fill = type)) +
  #   # geom_sf(data = plots_intersecting) +
  #   geom_sf(data = field, fill = NA) +
  #   geom_sf(data = ab_line, col = "red")

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Cut off the plots on the sides that are perpendicular to the machine direction
  #/*~~~~~~~~~~~~~~~~~~~~~~*/ 
  headland_buffer <- st_buffer(field, - plot_width) %>% 
    st_difference(field, .)

  headland_ids <- st_intersection(plots_intersecting, headland_buffer) %>% 
    mutate(area = as.numeric(st_area(.))) %>% 
    data.table() %>% 
    .[, area := sum(area), by = .(group, id)] %>% 
    .[area > 0.2 * (plot_width * cell_height),]  %>% 
    .[, unique_id]

  exp_plots_all <- filter(
    plots_intersecting, 
    !(unique_id %in% headland_ids)
  )

  # ggplot() +
  #   geom_sf(data = field, fill = NA) +
  #   geom_sf(data = exp_plots_all, aes(fill = type)) +
  #   geom_sf(data = ab_line, col = "red")

  # /*----------------------------------*/
  #' ## Reassign plot id
  # /*----------------------------------*/
  print("Reassigning plot id")
  # group: strip id
  # id: subplot id
  # plot_id: plot id
  min_obs <- 20 # (200 feet)
  mean_obs <- 24 # (240 feet)
  max_obs <- 30 #  (300 feet)

  # tm_shape(filter(final_plots, strip_id == 42)) +
  #   tm_fill(
  #     col = "plot_id", 
  #     palette = "Spectral", 
  #     style = "order"
  #   ) + 
  #   tm_layout_to_add

  exp_plots_pid <- exp_plots_all %>% 
    cbind(., st_coordinates(st_centroid(.))) %>% 
    data.table() %>%
    #--- detect gap ---# %>% 
    .[, d_X := c(0, diff(X)), by = group] %>% 
    .[, d_Y := c(0, diff(Y)), by = group] %>% 
    .[, distance := sqrt(d_X ^ 2 + d_Y ^ 2)] %>% 
    .[, gap := distance > (2 * cell_height)] %>% 
    .[, group_in_group := cumsum(gap) + 1, by = group] %>% 
    .[, group_contiguous := paste0(group, "_", group_in_group)] %>% 
    #--- observations per strip ---#
    .[, obs_per_strip := .N, by = .(group_contiguous)] %>%
    #--- drop the strip if there are less than `min_obs` subplots in it ---#
    # .[obs_per_strip < min_obs, type := "headland"] %>% 
    .[obs_per_strip > min_obs, ] %>% 
    #--- (initial) plot id ---#
    .[, dummy := 1] %>%
    .[, cum_num := cumsum(dummy), by = .(group_contiguous)] %>%
    .[, plot_id := (cum_num - 1) %/% mean_obs + 1, by = .(group_contiguous)] %>%
    #--- max number of plots per group_contiguous ---#
    .[, max_plot_id := max(plot_id), by = .(group_contiguous, plot_id)] %>%
    #--- number of subplots per plot ---#
    .[, obs_per_plot := .N, by = .(group_contiguous, plot_id)] %>%
    .[, too_short := obs_per_plot <= min_obs] %>% 
    #--- nest the data by group_contiguous ---#
    group_by(group_contiguous) %>% 
    nest() %>% 
    mutate(
      data = map(data, ~ data.table(.x))
    ) %>% 
    data.table() %>% 
    #--- apply reassign_plot_id ---#
    .[, map2(data, group_contiguous, ~ reassign_plot_id(.x, .y)) %>% rbindlist()] %>% 
    #--- make plot id in the same strip consecutive ---#
    .[, plot_id := .GRP, by = .(group, paste0(plot_id, group_in_group))] %>% 
    .[, plot_id := plot_id - min(plot_id) + 1, by = group] %>% 
    st_as_sf() 

  final_exp_plots <- exp_plots_pid %>%
    rename(strip_id = group, group_in_strip = group_in_group) %>%
    mutate(cell_id := 1:nrow(.)) %>%
    mutate(strip_id = strip_id - min(strip_id) + 1) %>%
    dplyr::select(-id)

  # final_headland <- filter(exp_plots_all, type == "headland") %>% 
  #   st_intersection(., field) %>% 
  #   dplyr::select(geometry) %>% 
  #   rbind(., filter(exp_plots_pid, type == "headland") %>% dplyr::select(geometry))

  return(final_exp_plots)
  # return(list(experiment = final_exp_plots, headland = final_headland))

}

# /*=================================================*/
#' # Assign rates to the trial design data
# /*=================================================*/
# data_sf <- st_as_sf(data)
# rates_ls <- N_levels 

assign_rates <- function(data_sf, rates_ls, pattern = "fixed-latin-square", merge = TRUE) {

  gen_sequence <- function(length) {
    if (length %% 2 == 0) {
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else {
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }
    return(seq_r)
  }

  gen_rd_seq <- function(seq_element, num) {
    for (i in 1:num) {
      if (i == 1) {
        seq_return <- seq_element
      } else {
        if (runif(1) < 0.5) {
          # seq_return <- c(seq_return, rev(seq_element))
          seq_return <- c(seq_return, seq_element)
        } else {
          seq_return <- c(seq_return, seq_element)
        }
      }
    }
    return(seq_return)
  }

  get_seq_for_strip <- function(pattern, rate_ranks_seq, num_seq, exclude_ls = NULL) {

    seq_possible <- gen_rd_seq(rate_ranks_seq, num_seq)

    position_ls <- 1:rates_len
    remaining_positions <- position_ls[!(position_ls %in% exclude_ls)]

    if (pattern == "block_randomized"){
      if (length(remaining_positions) == 1){
        position <- remaining_positions
      } else {
        position <- sample(remaining_positions, 1)
      }
    } else if (pattern == "sequential") {
      if (all(exclude_ls != 0)){
        previous_position <- exclude_ls[length(exclude_ls)]
      } else {
        previous_position <- 0
      }
      position <- previous_position + 1
    } else if (pattern == "fixed-latin-square") {

      if (all(exclude_ls != 0)){
        previous_position <- exclude_ls[length(exclude_ls)]
        which_furthest <- which.max(abs(rate_ranks_seq[remaining_positions] - rate_ranks_seq[previous_position]))
        position <- remaining_positions[which_furthest]
      } else {
        position <- 1
      }
    }

    return(seq_possible[position:(position + max_plot_id - 1)])

  }

  # /*=================================================*/
  #' # Assign rates
  # /*=================================================*/

  rates_data <- data.table(
    rate = rates_ls,
    rate_rank = seq_len(length(rates_ls))
  )

  rates_len <- nrow(rates_data)

  #--- create a sequence of rate ranks ---#
  rate_ranks_seq <- gen_sequence(rates_len)

  data_dt <- data.table(data_sf)

  strip_ls <- data_dt[, strip_id] %>% unique() %>% 
    .[order(.)]

  design_data_ls <- list()

  # i <- 12
  for (i in strip_ls) {
  # for (i in 1:10) {

    max_plot_id <- data_dt[strip_id == i, max(plot_id)]
    num_seq <- ceiling(max_plot_id / rates_len + 1)

    if ((i %% rates_len) == 1) {
      if (i == 1) {
        #--- the very first ---#
        rates_seq <- get_seq_for_strip(pattern, rate_ranks_seq, num_seq, 0)
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      } else {
        rates_seq <- get_seq_for_strip(
          pattern = pattern,
          rate_ranks_seq = rate_ranks_seq,
          num_seq = num_seq,
          #--- avoid having the same rate right next to it in the previous block ---#
          exclude_ls = init_rate_memeory[rates_len]
        )
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      }
    } else {
      rates_seq <- get_seq_for_strip(
        pattern = pattern,
        rate_ranks_seq = rate_ranks_seq,
        num_seq = num_seq,
        exclude_ls = init_rate_memeory
      )
      init_rate_memeory <- c(init_rate_memeory, which(rates_seq[1] == rate_ranks_seq))
    }

    design_data_ls[[i]] <- data.table(
      plot_id = seq_len(max_plot_id),
      strip_id = i,
      rate_rank = rates_seq
    )

    # print(rates_seq)
    # print(init_rate_memeory)

  }

  design_data <- rbindlist(design_data_ls)

  # design_data[, .N, by = rate_rank]

  if (merge == TRUE) {
    data <- left_join(data_sf, design_data, by = c("plot_id", "strip_id")) %>%
      left_join(., rates_data, by = "rate_rank")
    return(data)
  } else {
    design_data <- left_join(design_data, rates_data, by = "rate_rank")
    return(design_data)
  }

}

#/*=================================================*/
#' # Tilt the field
#/*=================================================*/

st_tilt <- function(data_sf, angle) {

  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  wf_bbox <- st_bbox(data_sf)
  data_geom <- st_geometry(data_sf)

  base_point <- c(wf_bbox["xmax"], wf_bbox["ymin"]) %>%
    st_point() %>%
    st_sfc()

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf)) 

  data_sf$geometry <- data_tilted 

  return(data_sf)

}

#/*=================================================*/
#' # Shift the field
#/*=================================================*/

st_shift <- function(data_sf, shift) {

  data_geom <- st_geometry(data_sf)
  
  shift_sfc <- st_point(shift) %>% st_sfc()

  geom_shifted <- data_geom + shift_sfc 

  data_sf$geometry <- geom_shifted  %>%
    st_set_crs(st_crs(data_sf))

  return(data_sf)

}




