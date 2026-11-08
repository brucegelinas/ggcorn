create_blisters_for_cob <- function(cob, aspect_ratio) {
  requested_share <- cob$smut / 100
  clusters <- seq_len(max_clusters)
  blisters_per_cluster <- 5 + round(5 * requested_share)

  tibble(
    cluster_id = rep(clusters, each = blisters_per_cluster),
    rank_in_cluster = rep(seq_len(blisters_per_cluster), times = length(clusters))
  ) |>
    mutate(
      cluster_progression = 0.05 + 0.92 * ((0.137 * cob$cob_id + cluster_id * 0.6180339887) %% 1)^0.8,
      cluster_transverse = 2 * ((0.291 * cob$cob_id + cluster_id * 0.7548776662) %% 1) - 1,
      cluster_size = 0.8 + 0.5 * stable_noise(cluster_id, cob$cob_id),
      cluster_radius = cob$half_thickness * (0.34 + 0.5 * requested_share^0.6) * cluster_size,
      cluster_angle = rank_in_cluster * 2.39996 + cluster_id,
      cluster_distance = cluster_radius * sqrt((rank_in_cluster - 0.5) / blisters_per_cluster),
      radius_y = cob$half_thickness * (0.2 + 0.26 * requested_share^1.4) *
        (0.8 + 0.5 * stable_noise(cluster_id + rank_in_cluster, cob$cob_id + 1)),
      radius_x = radius_y * aspect_ratio,
      limit_x = cob$x - radius_x * max_blister_lumpiness,
      center_x = cluster_progression * cob$x + cluster_distance * cos(cluster_angle) * aspect_ratio,
      center_x = pmax(pmin(center_x, limit_x), 0),
      cob_radius = relative_radius(center_x / cob$x, cob$cob_id),
      limit_y = pmax(
        cob$half_thickness * cob_radius * (1.08 + 0.3 * requested_share) - radius_y,
        0
      ),
      axis_offset = cob$half_thickness * cob_radius * cluster_transverse * 0.8 +
        cluster_distance * sin(cluster_angle),
      axis_offset = pmax(pmin(axis_offset, limit_y), -limit_y),
      center_y = cob$y + axis_offset,
      maturity_draw = 0.6 * stable_noise(rank_in_cluster + 3, cluster_id) +
        0.4 * stable_noise(cluster_id + 7, cob$cob_id + 2),
      ruptured_blister = maturity_draw < 0.08 + 0.72 * requested_share,
      blister_tint = stable_noise(rank_in_cluster + 11, cluster_id + cob$cob_id),
      shape_seed = rank_in_cluster + 31 * cluster_id + 17 * cob$cob_id,
      cob_id = cob$cob_id,
      half_thickness = cob$half_thickness
    )
}

choose_blisters_for_cob <- function(cob, cob_kernels, aspect_ratio) {
  requested_share <- cob$smut / 100
  if (requested_share <= 0) {
    return(NULL)
  }

  blisters <- create_blisters_for_cob(cob, aspect_ratio)
  covered_kernels <- rep(FALSE, nrow(cob_kernels))
  last_cluster <- max_clusters

  for (current_cluster in seq_len(max_clusters)) {
    cluster_blisters <- filter(blisters, cluster_id == current_cluster)
    for (blister_rank in seq_len(nrow(cluster_blisters))) {
      blister <- cluster_blisters[blister_rank, ]
      covered_kernels <- covered_kernels |
        ((cob_kernels$x_grain - blister$center_x) / blister$radius_x)^2 +
          ((cob_kernels$y_grain - blister$center_y) / blister$radius_y)^2 < 1
    }
    if (mean(covered_kernels) >= requested_share) {
      last_cluster <- current_cluster
      break
    }
  }

  filter(blisters, cluster_id <= last_cluster)
}

compute_blisters <- function(cobs, kernels, aspect_ratio) {
  kernel_centers <- distinct(kernels, cob_id, kernel_id, x_grain, y_grain)
  blisters <- lapply(seq_len(nrow(cobs)), function(cob_rank) {
    cob <- cobs[cob_rank, ]
    choose_blisters_for_cob(cob, filter(kernel_centers, cob_id == cob$cob_id), aspect_ratio)
  })
  bind_rows(blisters)
}

blister_body_color <- function(ruptured_blister, blister_tint, blister_angle) {
  healthy_ramp <- colorRamp(c("#DCE4D6", "#C5CEC2", "#A9ADB0"))
  ruptured_ramp <- colorRamp(c("#3D3935", "#262220"))
  healthy_colors <- rgb(healthy_ramp(blister_tint), maxColorValue = 255)
  ruptured_colors <- rgb(ruptured_ramp(blister_tint), maxColorValue = 255)
  shade_kernels(if_else(ruptured_blister, ruptured_colors, healthy_colors), blister_angle)
}

blister_outlines <- function(blisters) {
  layers <- tribble(
    ~layer_name,   ~layer_scale, ~offset_x, ~offset_y,
    "shadow",   1.04,            0.03,        -0.10,
    "body",   1.00,            0,           0,
    "highlight",  0.34,            -0.26,       0.30
  ) |>
    mutate(layer_rank = row_number())

  blisters |>
    mutate(depth_order = -abs(center_y - mean(center_y)), .by = cob_id) |>
    arrange(cob_id, depth_order) |>
    mutate(
      blister_rank = consecutive_id(cob_id, cluster_id, rank_in_cluster),
      blister_angle = asin(pmax(pmin((center_y - mean(center_y)) / (1.45 * half_thickness), 1), -1)),
      body_color = blister_body_color(ruptured_blister, blister_tint, blister_angle),
      .by = cob_id
    ) |>
    cross_join(layers) |>
    cross_join(tibble(angle_contour = seq(0, 2 * pi, length.out = 25)[-25])) |>
    mutate(
      lumpiness = 1 +
        0.07 * sin(2 * angle_contour + shape_seed) +
        0.05 * sin(3 * angle_contour + 2.3 * shape_seed),
      x_contour = center_x + radius_x *
        (offset_x + layer_scale * lumpiness * cos(angle_contour)),
      y_contour = center_y + radius_y *
        (offset_y + layer_scale * lumpiness * sin(angle_contour)),
      blister_polygon_id = 3 * (as.integer(factor(paste(cob_id, blister_rank))) - 1) + layer_rank,
      polygon_color = case_when(
        layer_name == "shadow" ~ "#45443F",
        layer_name == "body" ~ body_color,
        ruptured_blister ~ "#6A655F",
        .default = "#F3F6F0"
      ),
      polygon_outline = if_else(
        layer_name == "body",
        if_else(ruptured_blister, "#171412", "#6F726C"),
        NA_character_
      )
    )
}
