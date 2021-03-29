# /*=================================================*/
#' # Make experiment grids (basic cell, plot, strip)
# /*=================================================*/

make_trial_grids <- 
function(
  field, 
  ab_line, 
  plot_width, 
  machine_width,
  cell_height, 
  headland_length, 
  harvest_angle,
  side_plots_num = 1,
  second_input = FALSE
) {

  #/*=================================================*/
  #' # Define functions
  #/*=================================================*/
  get_through_line <- function(geometry, radius) {

    centroid <- st_coordinates(st_centroid(geometry))
    end_point <- centroid + radius * ab_xy_nml
    start_point <- centroid - radius * ab_xy_nml
    return_line <- st_linestring(rbind(start_point, end_point)) %>% 
      st_sfc() %>% 
      st_set_crs(st_crs(geometry)) %>% 
      st_as_sf()

    return(return_line)

  }

  # /*=================================================*/
  #' # Prepare ab-lines and vectors
  # /*=================================================*/
  ab_line_data <- prepare_ablines(ab_line, harvest_angle, field)

  #=== ab-line tilted by harvester angle ===#
  ab_line_tilted <- ab_line_data$ab_line_tilted
  #=== unit vector pointing in the direction the machine moves ===#
  ab_xy_nml <- ab_line_data$ab_xy_nml
  #=== unit vector pointing in the direction PERPENDICULAR to the direction the machine moves ===#
  ab_xy_nml_p90 <- ab_line_data$ab_xy_nml_p90

  #/*=================================================*/
  #' # Create strips
  #/*=================================================*/
  f_bbox <- st_bbox(field)

  #--- maximum distance ---#
  radius <- 
  sqrt(
    (f_bbox["xmax"] - f_bbox["xmin"])^2 +
    (f_bbox["ymax"] - f_bbox["ymin"])^2
  ) / 2 + 50

  strips <- create_strips(field, ab_line_tilted, plot_width, radius)

  # ggplot() +
  #   geom_sf(data = strips, aes(fill = "group")) +
  #   geom_sf(data = field, col = "black", fill = NA) +
  #   geom_sf(data = ab_line_tilted, col = "red")
  
  #/*=================================================*/
  #' # Shift the polygons (the ab-line goes through the center of a strip)
  #/*=================================================*/
  #=== find the group id for the cells that are intersecting with the ab-line  ===#
  ab_int_group <- st_intersection(strips, ab_line_tilted) %>% 
    pull(group) %>% unique()

  #=== get the sf of the intersecting group ===# 
  int_group <- filter(strips, group == ab_int_group)

  #=== the distance between the ab-line and the line that connect the centroids of the intersecting sf ===#
  correction_dist <- st_distance(
    get_through_line(int_group, radius), 
    ab_line_tilted
  ) %>% 
  as.numeric()

  #=== shift the intersecting sf  ===#
  int_group_corrected <- st_shift(
    int_group, 
    correction_dist * ab_xy_nml_p90,
    merge = FALSE
  )

  # ggplot() +
  #   geom_sf(data = int_group, fill = "blue", color = NA) +
  #   geom_sf(data = ab_line_tilted, color = "red")   
  # ggplot() +
  #   geom_sf(data = int_group_corrected, fill = "blue", color = NA) +
  #   geom_sf(data = ab_line_tilted, color = "red") 

  new_dist <- 
  st_distance(
    get_through_line(int_group_corrected, radius), 
    ab_line_tilted
  ) %>% 
  as.numeric()

  if (second_input == FALSE) {
    #=== if the first input ===# 
    # Note: for the first input, the cell center is aligned to the 
    # supplied ab-line (which is not the final ab-line)

    if (new_dist > correction_dist) {
      #--- if moved further away ---#
      strips_shifted <- st_shift(strips, - correction_dist * ab_xy_nml_p90)
    } else {
      #--- if get close ---#
      strips_shifted <- st_shift(strips, correction_dist * ab_xy_nml_p90)
    }
  } else {
    #=== if the second input ===#
    # Note: line_edge is used as the ab-line for the second input
    # the left (right) edge of the cells is shifted so that it is
    # aligned with the line_edge
    if (new_dist > correction_dist) {
      #--- if moved further away ---#
      strips_shifted <- 
      strips %>% 
      st_shift(., - correction_dist * ab_xy_nml_p90) %>% 
      st_shift(., - plot_width * ab_xy_nml_p90 / 2)
    } else {
      #--- if get close ---#
      strips_shifted <- 
      strips %>% 
      st_shift(., correction_dist * ab_xy_nml_p90) %>% 
      st_shift(., plot_width * ab_xy_nml_p90 / 2)
    }
  }
  
  # ggplot() +
  #   geom_sf(data = strips_shifted, fill = "blue", color = NA) +
  #   geom_sf(data = ab_line_tilted, col = "red")

  #/*=================================================*/
  #' # Create experiment plots
  #/*=================================================*/
  min_length <- conv_unit(180, "ft", "m") # (200 feet)
  mean_length <- conv_unit(240, "ft", "m") # (240 feet)
  max_length <- conv_unit(300, "ft", "m") #  (300 feet) 

  side_length <- 1.5 * side_plots_num * plot_width
  # ggplot(final_exp_plots) +
  #   geom_sf(aes(fill = strip_id))

  final_exp_plots <- field %>% 
    #=== create an inner buffer ===#
    st_buffer(- side_length) %>% 
    #=== intersect strips and the field ===#
    st_intersection(strips, .) %>% 
    dplyr::select(group) %>% 
    rowwise() %>% 
    #=== split multipolygons to individual polygons ===#
    mutate(indiv_polygon = list(
      st_cast(geometry, "POLYGON") %>% 
        st_as_sf() %>% 
        data.table() %>% 
        .[, group := group]
    )) %>% 
    pluck("indiv_polygon") %>% 
    reduce(rbind) %>% 
    .[, id_in_group := 1:.N, by = group] %>%
    st_as_sf() %>% 
    rowwise() %>% 
    #=== get the original strip geometry by group ===#
    left_join(., strips[, c("group", "geometry")], by = "group") %>% 
    #=== draw a line that goes through the middle of the strips ===#
    mutate(through_line = list(
      get_through_line(geometry, radius)
    )) %>% 
    mutate(int_line  = list(
      st_intersection(x, through_line)
    )) %>% 
    filter(length(int_line) != 0) %>% 
    #=== move int_points inward by (head_dist - side_distance) ===#
    mutate(new_center_line = list(
      move_points_inward(
        int_line, 
        max(headland_length - side_length, 0),
        ab_xy_nml
      )  
    )) %>% 
    filter(!is.null(new_center_line)) %>% 
    mutate(tot_plot_length = list(
      as.numeric(st_length(new_center_line))
    )) %>% 
    mutate(plot_data = list(
      get_plot_length(
        tot_plot_length, 
        min_length,
        mean_length
      )
    )) %>% 
    filter(!is.null(plot_data)) %>% 
    mutate(plots = list(
      create_plots_in_strip(
        plot_data, 
        new_center_line,
        plot_width,
        ab_xy_nml,
        ab_xy_nml_p90
      ) %>% 
      mutate(group = group) %>% 
      mutate(id_in_group = id_in_group)  
    )) %>% 
    pluck("plots") %>% 
    reduce(rbind) %>% 
    rename(strip_id = group, group_in_strip = id_in_group) %>%
    mutate(strip_id = strip_id - min(strip_id) + 1) %>% 
    st_set_crs(st_crs(field))

  #/*----------------------------------*/
  #' ## Get the ab-line
  #/*----------------------------------*/
  # plot_width
  # machine_width <- conv_unit(90, "ft","m")

    # ggplot() + 
    #   geom_sf(data = field) +
    #   geom_sf(data = ab_lines_data$ab_line_for_direction_check[[2]])

  # ggplot() + 
  #     geom_sf(data = field) +
  #     geom_sf(data = ab_lines_data$geometry[[1]])


  if (harvest_angle == 0) {

    ab_lines_data <- 
    rbind(
      get_through_line(
        filter(
          final_exp_plots, 
          strip_id == min(strip_id) & plot_id == 1
        ),
        radius
      ),
      get_through_line(
        filter(
          final_exp_plots, 
          strip_id == max(strip_id) & plot_id == 1
        ),
        radius
      )
    ) %>% 
    mutate(ab_id = seq_len(nrow(.))) %>% 
    expand_grid_df(tibble(dir_p = c(-1, 1)), .) %>% 
    rowwise() %>% 
    mutate(geometry = list(x)) %>% 
    mutate(ab_line_for_direction_check = list(
      st_shift(
        geometry, 
        dir_p * ab_xy_nml_p90 * (5 * plot_width), 
        merge = FALSE
      )
    )) %>% 
    mutate(intersection = list(
      st_as_sf(ab_line_for_direction_check[final_exp_plots, ])
    )) %>% 
    mutate(int_check = nrow(intersection))

    ab_lines <- ab_lines_data %>% 
      dplyr::select(ab_id, x) %>% 
      unique(by = "ab_id") %>% 
      st_as_sf() %>% 
      ungroup()

  } else { # if harvest angle is non-zero

    ab_line <- 
    get_through_line(
      filter(
        final_exp_plots, 
        strip_id == min(strip_id) & plot_id == 1
      ),
      radius
    ) %>% 
    #=== normalize ===#
    st_extend_line(., as.numeric(1 / st_length(.))) %>%
    st_tilt(., - harvest_angle, merge = FALSE)  

    # Create ab-lines at bunch of places
    cell_coordinates <- st_coordinates(st_centroid(final_exp_plots)) %>% 
      data.table() %>% 
      .[, cell_index := 1:.N]

    cells_to_use <- c(
      cell_coordinates[X == min(X), cell_index],
      cell_coordinates[X == max(X), cell_index],
      cell_coordinates[Y == min(Y), cell_index],
      cell_coordinates[Y == max(Y), cell_index]
    )

    ab_lines <- cell_coordinates[cells_to_use, ] %>% 
      rowwise() %>% 
      mutate(shift_X = X - ab_line[[1]][1, 1]) %>% 
      mutate(shift_Y = Y - ab_line[[1]][1, 2]) %>% 
      mutate(ab_lines_shifted = list(
        st_shift(ab_line, c(shift_X, shift_Y), merge = FALSE)
      )) %>% 
      mutate(ab_lines_shifted_extended = list(
        #=== 50 meter ===#
        st_extend_line(ab_lines_shifted, 200)
      )) %>% 
      pluck("ab_lines_shifted_extended") %>% 
      reduce(c) %>% 
      st_as_sf() %>% 
      mutate(ab_id = seq_len(nrow(.)))

  }

  #=== ab-line re-centering when machine width > plot_width ===#
  if (machine_width != plot_width & harvest_angle == 0) {

    ab_lines <- ab_lines_data %>% 
    #=== which direction to go ===#
    # Notes: go inward (intersecting) if machine_width > plot_width, otherwise outward
    filter(int_check == ifelse(machine_width > plot_width, 1, 0)) %>% 
    mutate(ab_recentered = list(
      st_shift(
        geometry, 
        dir_p * ab_xy_nml_p90 * abs(machine_width - plot_width) / 2,
        merge = FALSE
      )
    )) %>% 
    pluck("ab_recentered") %>% 
    reduce(c) %>% 
    st_as_sf() %>% 
    mutate(ab_id = seq_len(nrow(.)))

  } 

  if (second_input == FALSE & harvest_angle == 0) {
    # this is used to align the left (or) right edges of the first input experiment plot
    #=== which way to move for the first to go inward ===#
    # ab_lines_data$int_check 

    dir_p <- filter(ab_lines_data, ab_id == 1 & int_check == 1) %>% 
      pull(dir_p)

    line_edges <- 
    rbind(
      line_edge_f = st_shift(ab_lines[1, ], - dir_p * ab_xy_nml_p90 * plot_width / 2),
      line_edge_s = st_shift(ab_lines[2, ], dir_p * ab_xy_nml_p90 * plot_width / 2)
    )
  }

  # ggplot() +
  #   geom_sf(data = field, fill = NA) +
  #   geom_sf(data = final_exp_plots, aes(fill = type), color = NA) +
  #   geom_sf(data = line_edge_f, col = "red", size = 1) +
  #   geom_sf(data = line_edge_s, col = "darkgreen", size = 1)

  # ggplot() +
  #   geom_sf(data = field, fill = NA) +
  #   geom_sf(data = final_exp_plots, fill = "blue", color = NA) +
  #   geom_sf(data = ab_lines, aes(col = factor(ab_id)), size = 1)

  # ggplot() +
  #   geom_sf(data = field, fill = NA) +
  #   geom_sf(data = final_exp_plots, fill = "blue", color = NA) +
  #   geom_sf(data = ab_line, size = 1)

  #/*----------------------------------*/
  #' ## Save
  #/*----------------------------------*/  

  if (second_input == FALSE) {
    return(list(
      exp_plots = final_exp_plots, 
      ab_lines = ab_lines,
      line_edges = line_edges
    ))
  } else {
    return(list(
      exp_plots = final_exp_plots, 
      ab_lines = ab_lines
    ))
  }

}

#/*=================================================*/
#' # Assign rates (latin and jump-rate-conscious)
#/*=================================================*/
assign_rates_latin <- function(
  data_sf,
  rates_ls,
  push,
  merge = TRUE
) {

  gen_sequence <- function(length, push = FALSE) {

    if (length %% 2 == 0) { # even 
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else { # odd
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }

    if (push) {
      seq_r <- c(seq_r[-1], seq_r[1])
    }

    return(seq_r)
  }

  get_seq_start <- function(rate_rank, basic_seq) {

    max_rank <- length(basic_seq)
    start_position <- which(basic_seq == rate_rank)
    
    f_seq <- start_position:max_rank
    s_seq <- 1:start_position

    return_rank <- basic_seq[c(f_seq, s_seq) %>% unique()]

    return(return_rank)
    
  }

  get_starting_rank_across_strips <- function(num_levels) {

    temp_seq <- 2:(num_levels - 1)
    return_seq <- rep(1, num_levels)

    i <- 1
    while (i <= num_levels - 2) {

      if (i %% 2 == 1) { # odd
        temp_value <- max(temp_seq)
      } else {
        temp_value <- min(temp_seq)
      }

      return_seq[i + 1] <- temp_value

      temp_seq <- temp_seq[-which(temp_seq == temp_value)]

      i <- i + 1

    }

    return_seq[length(return_seq)] <- num_levels

    return(return_seq)
  }

  num_levels <- length(rates_ls)
  max_plot_id <- max(data_sf$plot_id)
  max_strip_id <- max(data_sf$strip_id)

  rates_data_base <- 
  data.table(
    rate = rates_ls,
    rate_rank = 1:num_levels
  )

  #=== get the rate sequence within a strip ===#
  basic_seq <- gen_sequence(num_levels)
  if (push) {
    basic_seq <- c(basic_seq[2:num_levels], basic_seq[1])
  }

  #=== get the starting ranks across strips for the field ===#
  full_start_seq <- rep(
    get_starting_rank_across_strips(num_levels),
    ceiling(max_strip_id / num_levels)
  ) %>% 
  .[1:max_strip_id]
  
  rates_data <- 
  data.table(
    strip_id = 1:max_strip_id,
    start_rank = full_start_seq
  ) %>% 
  rowwise() %>% 
  mutate(rate_rank = list(
    rep(
      get_seq_start(start_rank, basic_seq),
      ceiling(max_plot_id / num_levels)
    )
  )) %>% 
  unnest(rate_rank) %>% 
  data.table() %>% 
  .[, dummy := 1] %>% 
  .[, plot_id := cumsum(dummy), by = strip_id] %>% 
  rates_data_base[., on = "rate_rank"] %>% 
  .[, .(strip_id, plot_id, rate) ]  

  return_data <- 
  left_join(
    data_sf, 
    rates_data, 
    by = c("strip_id", "plot_id")
  )

  ggplot() +
    geom_sf(data = return_data, aes(fill = factor(rate)), color = NA) +
    scale_fill_brewer(palette = "Greens")

  return(return_data)

}

#/*=================================================*/
#' # Assign rates (latin and jump-rate-conscious)
#/*=================================================*/
# data_sf <- trial_data_eh$exp_plots[[1]]
# min_rate <- trial_data_eh$min_rate[[1]]
# max_rate <- trial_data_eh$max_rate[[1]]
# gc_rate <- trial_data_eh$gc_rate[[1]]
# max_jump <- 4
#

# rates_ls <- seq(min_rate, max_rate, length = 9)

assign_rates <- function(
  data_sf,
  design_type = "jcl",
  max_jump,
  gc_rate,
  min_rate,
  max_rate,
  num_levels,
  rates,
  push,
  merge = TRUE
) {

#/*----------------------------------*/
#' ## Define functions
#/*----------------------------------*/
  gen_sequence <- function(length, push = FALSE) {

    if (length %% 2 == 0) { # even 
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else { # odd
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }

    if (push) {
      seq_r <- c(seq_r[-1], seq_r[1])
    }

    return(seq_r)
  }

  get_seq_start <- function(rate_rank, basic_seq) {

    max_rank <- length(basic_seq)
    start_position <- which(basic_seq == rate_rank)
    
    f_seq <- start_position:max_rank
    s_seq <- 1:start_position

    return_rank <- basic_seq[c(f_seq, s_seq) %>% unique()]

    return(return_rank)
    
  }

  get_starting_rank_across_strips <- function(num_levels) {

    temp_seq <- 2:(num_levels - 1)
    return_seq <- rep(1, num_levels)

    i <- 1
    while (i <= num_levels - 2) {

      if (i %% 2 == 1) { # odd
        temp_value <- max(temp_seq)
      } else {
        temp_value <- min(temp_seq)
      }

      return_seq[i + 1] <- temp_value

      temp_seq <- temp_seq[-which(temp_seq == temp_value)]

      i <- i + 1

    }

    return_seq[length(return_seq)] <- num_levels

    return(return_seq)
  }

#/*----------------------------------*/
#' ## Assign rates
#/*----------------------------------*/

  if (design_type == "jcl") {
  
    if (any(is.na(rates))) {
      rates_ls <- get_rates(min_rate, max_rate, gc_rate, num_levels)
    } else {
      rates_ls <- rates
      num_levels <- length(rates)
    }

    max_plot_id <- max(data_sf$plot_id)
    max_strip_id <- max(data_sf$strip_id)

    rates_data_base <- 
    data.table(
      rate = rates_ls,
      rate_rank = 1:num_levels
    )

    #=== get the rate sequence within a strip ===#
    basic_seq <- gen_sequence(num_levels)

    #=== get the starting ranks across strips for the field ===#
    full_start_seq <- rep(
      get_starting_rank_across_strips(num_levels),
      ceiling(max_strip_id / num_levels) + 1
    ) %>% 
    .[1:(max_strip_id + 1)]

    if (push) {
      full_start_seq <- full_start_seq[2:(max_strip_id + 1)]
    } else {
      full_start_seq <- full_start_seq[1:max_strip_id]
    }
    
    rates_data <- 
    data.table(
      strip_id = 1:max_strip_id,
      start_rank = full_start_seq
    ) %>% 
    rowwise() %>% 
    mutate(rate_rank = list(
      rep(
        get_seq_start(start_rank, basic_seq),
        ceiling(max_plot_id / num_levels)
      )
    )) %>% 
    unnest(rate_rank) %>% 
    data.table() %>% 
    .[, dummy := 1] %>% 
    .[, plot_id := cumsum(dummy), by = strip_id] %>% 
    rates_data_base[., on = "rate_rank"] %>% 
    .[, .(strip_id, plot_id, rate) ]  

    return_data <- 
    left_join(
      data_sf, 
      rates_data, 
      by = c("strip_id", "plot_id")
    )

  } else if (design_type == "ejca") { # Extra jump-conscious alternate

    #=== num_levels internally determined ===#
    num_levels <- seq(min_rate, max_rate, by = max_jump * 0.8) %>% 
      length()

    if (num_levels < 5) {
      num_levels <- 8 
      print("max jump rate is too high. setting the number of levels to 8")
    }

    if (any(is.na(rates))) {
      rates_ls <- get_rates(min_rate, max_rate, gc_rate, num_levels)
    } else {
      rates_ls <- rates
      num_levels <- length(rates)
    }

    total_num_levels <- length(rates_ls)
    max_plot_id <- max(data_sf$plot_id)
    max_strip_id <- max(data_sf$strip_id)

    rates_data_base <- 
    data.table(
      rate = rates_ls,
      rate_rank = 1:total_num_levels
    ) %>% 
    .[, tier := ifelse(rate_rank < median(rate_rank), 1, 2)] %>% 
    .[, rank_in_tier := rowid(tier)]

    rates_data <- rates_data_base %>% 
      nest_by(tier) %>% 
      mutate(num_levels = nrow(data)) %>% 
      mutate(basic_seq = list(
        gen_sequence(num_levels)
      )) %>% 
      mutate(basic_seq = list(
        if (push) {
          c(basic_seq[2:num_levels], basic_seq[1])
        } else {
          basic_seq
        }
      )) %>% 
      mutate(strip_plot_data = list(
        if (tier == 1) {
          filter(data_sf, (strip_id %% 2) == 1) %>% 
            data.table() %>% 
            .[, .(strip_id, plot_id)] %>% 
            unique(by = c("strip_id", "plot_id"))
        } else {
          filter(data_sf, (strip_id %% 2) == 0) %>% 
            data.table() %>% 
            .[, .(strip_id, plot_id)] %>% 
            unique(by = c("strip_id", "plot_id"))
        }
      )) %>% 
      mutate(strip_plot_data = list(
        strip_plot_data[, group_in_strip := .GRP, by = strip_id]
      )) %>% 
      mutate(strip_plot_data = list(
        lapply(
          unique(strip_plot_data$strip_id),
          function (x) {
            temp_data <- strip_plot_data[strip_id == x, ]
            if ((unique(temp_data$group_in_strip) %% 2) == 0) {
              temp_data <- temp_data[order(rev(plot_id)), ]
            } 
            return(temp_data)
          }  
        ) %>% 
        rbindlist()
      )) %>% 
      mutate(strip_plot_data = list(
        strip_plot_data[, rank_in_tier :=
          rep(basic_seq, ceiling(nrow(strip_plot_data) / num_levels))[1:nrow(strip_plot_data)]
        ]
      )) %>% 
      mutate(rate_data = list(
        data.table(data)[strip_plot_data[, .(strip_id, plot_id, rank_in_tier)], on = "rank_in_tier"]
      )) %>% 
      pluck("rate_data") %>% 
      rbindlist()

    return_data <- 
    left_join(
      data_sf, 
      rates_data, 
      by = c("strip_id", "plot_id")
    )
  }

  # ggplot() +
  #   geom_sf(data = return_data, aes(fill = factor(rate)), color = NA) +
  #   scale_fill_viridis_d()

  return(return_data)

}

#/*=================================================*/
#' # Tilt the field
#/*=================================================*/
# data_sf = ab_line
# angle = harvest_angle

st_tilt <- function(data_sf, angle, base_sf = FALSE, merge = TRUE) {

  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  if ("sf" %in% class(base_sf)) {
    wf_bbox <- st_bbox(base_sf) %>% st_as_sfc()
  } else {
    wf_bbox <- st_bbox(data_sf) %>% st_as_sfc()
  }
  
  base_point <- st_centroid(wf_bbox)
  data_geom <- st_geometry(data_sf)

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf)) 

  if (merge == TRUE) {
    data_sf$geometry <- data_tilted 
    return(data_sf)
  } else {
    return(data_tilted)
  }

  # ggplot() +
  #   geom_sf(data_sf, fill = "red", alpha = 0.4) +
  #   geom_sf(data_tilted, fill = "blue", alpha = 0.4)
}

#/*=================================================*/
#' # Shift the field
#/*=================================================*/

# data_sf <- ab_lines[1, ]
# shift <- dir_p * ab_xy_nml_p90 * plot_width / 2

st_shift <- function(data_sf, shift, merge = TRUE) {

  data_geom <- st_geometry(data_sf) 
  temp_crs <- st_crs(data_sf) 

  shift_sfc <- st_point(shift) %>% st_sfc()

  geom_shifted <- (data_geom + shift_sfc) %>% 
    st_set_crs(temp_crs)

  if (merge == TRUE){
    data_sf <- st_drop_geometry(data_sf) 
    data_sf$geometry <- geom_shifted
    return(st_as_sf(data_sf))
  } else {
    return(geom_shifted)
  }

}

#/*=================================================*/
#' # Get an ab-line
#/*=================================================*/

make_ab_line <- function(past_aa_input, field) {

  temp_sf <- dplyr::select(past_aa_input, geometry)

  # tm_shape(past_aa_input) +
  #   tm_dots()

  #=== polygons? ===#
  inlude_polygon <- "POLYGON" %in% st_geometry_type(past_aa_input)

  if (inlude_polygon) {
    return(NULL)
  } else {

    dominant_slope <- group_points_sc(temp_sf, angle_threshold = 30) %>% 
      nest_by(group) %>% 
      rowwise() %>% 
      mutate(slope = 
        lm(Y ~ X, data = data)$coef[2]
      ) %>% 
      filter(!is.na(slope)) %>% 
      unnest() %>% 
      mutate(cluster = kmeans(slope, 6)$cluster) %>% 
      data.table() %>% 
      .[, .(slope, cluster)] %>% 
      .[, num_obs := .N, by = cluster] %>% 
      .[num_obs == max(num_obs), ] %>% 
      .[, mean(slope)]
  }

  ab_start <- st_geometry(st_centroid(field))[[1]]
  ab_end <- ab_start + c(1, dominant_slope)

  ab_line <- 
  list(
    st_linestring(c(ab_start, ab_end))
  ) %>% 
  st_as_sfc() %>% 
  st_set_crs(st_crs(field))

  return(ab_line)

}

#/*=================================================*/
#' # Get mean_Rx value on a field
#/*=================================================*/
get_mean_Rx <- function(ffy, input){

  dictionary <- jsonlite::fromJSON(
    here("Data", "CommonData", "variable_name_dictionary.json"),
    flatten = TRUE) %>%
    data.table()
  
  dict_rx <- dictionary[type == paste0("Rx-", input), ]
  
  #--- bring in rx for the input ---#

  rx <- st_read(here("Data/Growers", ffy, paste0("Raw/Rx-", input, ".shp"))) %>%
   setnames(names(.), tolower(names(.))) %>%
   mutate(area = as.numeric(st_area(.)))
  
  #--- rename tgt value to tgti ---#
  rx <- make_var_name_consistent(
   rx, 
   dict_rx)
  
  #--- if input is seed, put in K, not seeds ---#
  if (input == "s") {
    if (any(rx$tgti > 10000)){
      rx <- mutate(rx, tgti = tgti / 1000)
    }
  }
  
  #--- take weighted average of the tgt rate ---#
  rx <- rx %>%
    mutate(area_weight = area/sum(rx$area)) %>%
    mutate(weighted_tgt = tgti*area_weight) 
    
  gc_rate <- sum(rx$weighted_tgt)

  return(gc_rate)
}

#/*=================================================*/
#' # Get input type for bringing in Rx (need to add more inputs)
#/*=================================================*/
find_input <- function(form){
    if(form == "seed"){
      input = "s"
    }else{
      input = "n"
    }
    return(input)
  }

#/*=================================================*/
#' # Get field parameters for trial design
#/*=================================================*/

get_td_parameters <- function(
  ffy, 
  json_file, 
  input_data
){

  #--- bring in field data ---#
  field_data <- jsonlite::fromJSON(
    here("Data", "CommonData", json_file),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")] %>%
  .[field_year == ffy, ]

  trial_info <- dplyr::select(field_data, starts_with(
    "input")) %>%  map(1) %>% 
    rbindlist(fill = TRUE)  

  #=== trial information (N base not included) ===#
  td_parameters <-  trial_info %>% 
    filter(strategy == "trial") %>% 
    dplyr::select(
      form, sq_rate, unit, min_rate,
      max_rate, input_plot_width, machine_width
    ) %>% 
    rename(gc_rate = sq_rate) %>% 
    mutate(year = field_data$year) %>% 
    #=== the input with shorter plot length comes first ===#
    arrange(desc(input_plot_width))
  
  #=== For those with an Rx we need to find mean Rx value ===#
  if("Rx" %in% td_parameters$gc_rate){

    td_parameters_rx <- td_parameters %>%
      filter(gc_rate == "Rx") %>%
      rowwise() %>%
      mutate(input = find_input(form)) %>%
      mutate(gc_rate = get_mean_Rx(ffy, input))

    td_parameters <- rbind(td_parameters_rx, td_parameters %>% filter(gc_rate != "Rx")) %>%
      data.table()
  }
  
  #=== check if there are N base rate entries ===#  
  if ("base" %in% trial_info$strategy){
    base_rate <- trial_info %>%
    filter(strategy == "base") %>%
    dplyr::select("rate")
  }else{
    base_rate <- 0
  }

  #--- convert min_rate and max_rate into n_form units ---#
  # min_rate, max_rate, and base_rate are all in N-equivalent (lbs)
  n_parameters <- td_parameters[form != "seed"] 

  if (nrow(n_parameters) > 0) {
    n_parameters <- n_parameters %>%
      rowwise() %>%
      mutate(
        min_rate = min_rate - base_rate, 
        max_rate = max_rate - base_rate, 
        .keep = "unused"
      ) %>%
      mutate(
        min_rate = convert_N_unit(form, unit, min_rate, "Imperial", conversion_type = "to_n_form"),
        max_rate = convert_N_unit(form, unit,  max_rate, "Imperial", conversion_type = "to_n_form")
      )

    input_data <- rbind(td_parameters[form == "seed"], n_parameters)

  } else {

    input_data <- td_parameters 

  }
  
  input_data <- arrange(input_data, desc(input_plot_width))

  return(input_data)

}

#/*=================================================*/
#' # Get experiment rates
#/*=================================================*/

# min_rate <- 80
# max_rate <- 180
# gc_rate <- 140
# num_level <- 5

get_rates <- 
function(
  min_rate, 
  max_rate, 
  gc_rate, 
  num_levels
) {

  dif_min <- gc_rate - min_rate 
  dif_max <- max_rate - gc_rate

  num_levels_temp <- num_levels + 1

  if (dif_max > dif_min) {
    if (num_levels_temp %% 2 == 1) {
      num_high <- num_levels_temp %/% 2 + 1
      num_low <- num_levels_temp %/% 2
    } else if ((dif_max / dif_min) > 1.5){ 
      num_high <- num_levels_temp %/% 2 + 1
      num_low <- num_levels_temp %/% 2 - 1
    } else { 
      num_high <- num_levels_temp %/% 2
      num_low <- num_levels_temp %/% 2
    }
  } else {
    if (num_levels_temp %% 2 == 1) {
      num_high <- num_levels_temp %/% 2 
      num_low <- num_levels_temp %/% 2 + 1
    } else if ((dif_min / dif_max) > 1.5){ 
      num_high <- num_levels_temp %/% 2 - 1
      num_low <- num_levels_temp %/% 2 + 1
    } else {
      num_high <- num_levels_temp %/% 2
      num_low <- num_levels_temp %/% 2
    }
  }

  rates_low <- seq(min_rate, gc_rate, length = num_low) %>% round()
  rates_high <- seq(gc_rate, max_rate, length = num_high) %>% round()

  rates <- c(rates_low, rates_high) %>% unique()

  return(rates)
}

#/*=================================================*/
#' # Extend a line
#/*=================================================*/

# line <- ab_line_recentered$geometry[[1]]
# multiplier <- 3
# st_extend_line(line, multiplier)

st_extend_line <- function(line, multiplier) {

  new_line <- st_geometry(line)[[1]]
  strt <- new_line[1, ]
  vec <- new_line[2, ] - new_line[1, ]
  new_line[2, ] <- strt + multiplier * vec

  return_line <- st_sfc(new_line) %>% 
    st_set_crs(st_crs(line))

  return(return_line)
}


#/*=================================================*/
#' # Get shape file name 
#/*=================================================*/

get_shp_name <- function(ffy, folder, key) {

  file_name <- here("Data", "Growers", ffy, folder) %>% 
    list.files(recursive = TRUE, full.names = TRUE) %>%
    #--- search for as-applied-s file ---#
    .[str_detect(., "shp")] %>%
    .[!str_detect(., "xml")] %>%
    .[str_detect(., key)] 

  return(file_name)

}

#/*=================================================*/
#' # Convert to utm
#/*=================================================*/

make_sf_utm <- function(data_sf) {

  return_sf <- data_sf %>% 
    st_set_4326() %>% 
    st_make_valid() %>%
    #=== force WGS84 ===#
    st_transform(4326) %>% 
    st_transform_utm()  

  return(return_sf)

}

#/*=================================================*/
#' # Get harvester angle relative to input ab-line
#/*=================================================*/
# line_1 <- ab_line_tilted
# line_2 <- through_line

get_angle_lines <- function(line_1, line_2) {

  rotate <- function(angle) {
    matrix(
      c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2
    )
  }

  h_mat <- st_geometry(line_1)[[1]]
  h_vec <- h_mat[2, ] - h_mat[1, ]
  h_vec_n <- h_vec / sqrt(sum(h_vec ^ 2))

  i_mat <- st_geometry(line_2)[[1]]
  i_vec <- i_mat[2, ] - i_mat[1, ]
  i_vec_n <- i_vec / sqrt(sum(i_vec ^ 2))

  angle <- acos(sum(i_vec_n * h_vec_n)) / pi * 180

  angle <- 
  tibble(angle = c(angle, 180 - angle)) %>% 
  rowwise() %>% 
  mutate(i_vect_rotated = list(
    i_vec_n %*% rotate(angle / 180 * pi) 
  )) %>% 
  mutate(dot_product = sum(i_vect_rotated * h_vec_n)) %>% 
  mutate(dist = abs(dot_product) - 1) %>% 
  arrange(abs(dist)) %>% 
  ungroup() %>% 
  slice(1) %>% 
  pull(angle)

  return(angle)

}

move_points_inward <- function(line, dist, ab_xy_nml) {

  #=== in case the intersected line is multi-linestring ===#
  temp_lines <- st_cast(line, "LINESTRING")
  line <- temp_lines[[length(temp_lines)]]

  if (as.numeric(st_length(line)) > 2 * dist) {

    start_point <- line[1, ]
    end_point <- line[2, ]

    new_start_point <- line[1, ] + ab_xy_nml * dist
    new_end_point <- line[2, ] - ab_xy_nml * dist

    new_through_line <- 
    st_linestring(
      rbind(
        new_start_point,
        new_end_point
      )
    ) %>% 
    st_sfc() %>% 
    st_set_crs(st_crs(line))

    return(new_through_line)

  } else {

    return(NULL)

  }

}

get_plot_length <- function(tot_plot_length, min_length, mean_length) {

  num_comp_plots <-  tot_plot_length %/% mean_length
  remainder <- tot_plot_length %% mean_length

  return_data <- data.table(plot_id = seq_len(num_comp_plots + 1))

  if (num_comp_plots == 0) { # if no complete plots
    if (remainder < min_length) {
      return(NULL)
    } else {
      return_data[, plot_length := remainder]
    }
  } else if (remainder >= (2 * min_length - mean_length)) {
    # make the last two short
    return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 1),
      rep((mean_length + remainder) / 2, 2)
    )]
  } else if (
    num_comp_plots >= 2 & 
    remainder >= (3 * min_length - 2 * mean_length)
  ) 
  {
    # make the last three short 
    return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 2),
      rep((2 * mean_length + remainder) / 3, 3)
    )]
  } else if (
    num_comp_plots >= 2  
  ) {
    # make the 2nd and 3rd last longer 
    return_data <- return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 2),
      rep((2 * mean_length + remainder) / 2, 2),
      NA
    )] %>% 
    .[!is.na(plot_length),]
  } else {
    # only 1 complete plot 
    return_data <- return_data[, plot_length := mean_length + remainder] %>% 
      .[1,]
  }
  
  return(return_data)  

}

# plots <- temp$plots[[4]]
# new_center_line <- temp$new_center_line[[4]]
# ggplot() +
#   geom_sf(data = plots, fill = "blue") +
#   geom_sf(data = new_center_line, fill = "red") 
# create_plots_in_strip(
#   plot_data, 
#   new_center_line,
#   plot_width,
#   ab_xy_nml,
#   ab_xy_nml_p90
# )

create_plots_in_strip <- function(
  plot_data, 
  new_center_line,
  plot_width,
  ab_xy_nml,
  ab_xy_nml_p90
) {

  make_polygon <- function(
    base_point, 
    start_length, 
    plot_length,
    plot_width,
    ab_xy_nml,
    ab_xy_nml_p90
  ) {

    point_0 <- base_point + ab_xy_nml * start_length
    point_1 <- point_0 + (plot_width / 2) * ab_xy_nml_p90
    point_2 <- point_1 + ab_xy_nml * plot_length
    point_3 <- point_2 - plot_width * ab_xy_nml_p90
    point_4 <- point_3 - ab_xy_nml * plot_length

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

  base_point <- st_geometry(new_center_line)[[1]][1, ]

  return_polygons <- 
  plot_data %>% 
  .[, plot_start := data.table::shift(plot_length, type = "lag")] %>% 
  .[is.na(plot_start), plot_start := 0] %>% 
  .[, start_length := cumsum(plot_start)] %>% 
  rowwise() %>% 
  mutate(geometry = list(
    make_polygon(
      base_point = base_point, 
      start_length = start_length, 
      plot_length = plot_length,
      plot_width = plot_width,
      ab_xy_nml = ab_xy_nml,
      ab_xy_nml_p90 = ab_xy_nml_p90     
    )
  )) %>% 
  data.table() %>% 
  .[, .(plot_id, geometry)] %>% 
  st_as_sf()

  return(return_polygons)

}

prepare_ablines <- function(ab_line, harvest_angle, field) {

  rotate_mat_p90 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #=== tilt based on harvest angle ===#
  ab_line_tilted <- st_tilt(ab_line, harvest_angle, merge = FALSE)
  #--- get the vector (direction machines run)  ---#
  ab_xy <- st_geometry(ab_line_tilted)[[1]][2, ] - st_geometry(ab_line_tilted)[[1]][1, ]  
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  #=== if ab-line is outside of the field boundary ===#
  if (nrow(st_as_sf(st_intersection(field, ab_line_tilted))) == 0) {

    b <- t(
      st_coordinates(st_centroid(field)) - 
      st_geometry(ab_line_tilted)[[1]][1, ] 
    )
    a <- cbind(
      t(ab_xy_nml_p90),
      ab_xy_nml
    )

    multiplier <- solve(a, b)

    ab_line_tilted <- 
    st_shift(
      ab_line_tilted, 
      round(multiplier[[1]] / plot_width) * plot_width * ab_xy_nml_p90 + 
      multiplier[[2]] * ab_xy_nml, 
      merge = FALSE
    )

  }

  return(list(
    ab_line_tilted = ab_line_tilted,
    ab_xy_nml = ab_xy_nml,
    ab_xy_nml_p90 = ab_xy_nml_p90
  ))
}

get_line_through_centroids <- function(data_sf, crs) {

  centroids <- data_sf %>% 
    st_centroid() %>% 
    .[c(1, nrow(.)), ] %>% 
    st_geometry()

  line <- list(st_linestring(c(centroids[[1]], centroids[[2]]))) %>% 
    st_as_sfc() %>% 
    st_set_crs(crs)

  return(line)

}

#/*----------------------------------*/
#' ## Calculate the distance 
#/*----------------------------------*/
# Calculate the distance between a strip of polygons and the ab_line

cal_dist_to_ab <- function(data_sf, ab_line) {

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
reassign_plot_id <- function(data) {

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
#' ## Create plots (cells) that covers the entire field
#/*----------------------------------*/
create_plots <- function(
  strt_point,
  dir_p,
  dir_v,
  plot_width,
  num_subplots,
  cell_height,
  field,
  ab_xy_nml_p90
) {

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
          dir_v,
          cell_height
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

#/*----------------------------------*/
#' ## Create plots
#/*----------------------------------*/
detect_directions <- 
function(
  strt_point, 
  dir_p, dir_v, 
  plot_width, 
  num_subplots,
  cell_height,
  field,
  ab_xy_nml_p90
) {

  is_intersecting <- rep(TRUE, 100)
  
  exp_sf_ls <- list()

  # group <- 1
  for (group in 1:100) {
    # group = 2
    # strt_point <- starting_point
    temp_line <- tibble(x = c(1, num_subplots)) %>% 
      rowwise() %>% 
      mutate(end_polygons = list(
        make_polygon(
          strt_point + plot_width * dir_p * ab_xy_nml_p90 * (group - 1),
          x,
          dir_p,
          dir_v,
          cell_height
        )
      )) %>% 
      pluck("end_polygons") %>% 
      st_sfc() %>% 
      st_as_sf() %>% 
      get_line_through_centroids(st_crs(field))

    is_intersecting[group] <- st_intersects(temp_line, field, sparse = F)[, 1]

    if (is_intersecting[group]) {
      return(TRUE)
    } else if (all(!is_intersecting[1:50])) {
      return(FALSE)
    }
  }
}

# /*----------------------------------*/
#' ## make polygons
# /*----------------------------------*/
make_polygon <- function(
  strt_point_new, 
  multiplier,
  dir_p,
  dir_v,
  cell_height,
  ab_xy_nml,
  ab_xy_nml_p90
) {

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

# /*=================================================*/
#' # Create strips
# /*=================================================*/
# ggplot() +
#   geom_sf(data = circle, fill = NA) +
#   geom_sf(data = field) +
#   geom_sf(data = strips, aes(fill = group), color = NA, alpha = 0.4) 

create_strips <- function(field, ab_line_tilted, plot_width, radius) {

  circle <- st_buffer(st_centroid(field), radius)

  strips <- st_make_grid(circle, cellsize = c(plot_width, radius * 2 + 50)) %>% 
      st_as_sf() %>% 
      cbind(., st_coordinates(st_centroid(.))) %>% 
      data.table() %>% 
      .[order(X),] %>% 
      .[, group := .GRP, by = X] %>% 
      setnames("x", "geometry") %>% 
      st_as_sf()

  vertical_line <- 
  rbind(
    c(0, 0),
    c(0, 10)
  ) %>% 
  st_linestring() %>% 
  st_sfc() %>% 
  st_set_crs(st_crs(field)) %>% 
  st_as_sf()

  strips <- 
  st_tilt(
    data_sf = strips, 
    angle = get_angle_lines(ab_line_tilted, vertical_line),
    base_sf = circle,
    merge = TRUE
  )

  return(strips)

}
