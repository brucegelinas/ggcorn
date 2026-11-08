valid_cutworm_zones <- c("tip", "middle", "base")

parse_cutworm_zones <- function(cutworm_zone) {
  zones <- trimws(strsplit(tolower(cutworm_zone), "[+,]")[[1]])
  unknown_zones <- setdiff(zones, valid_cutworm_zones)
  if (length(zones) == 0 || length(unknown_zones) > 0) {
    rlang::abort(paste0(
      "`cutworm_zone` must combine \"tip\", \"middle\" and \"base\", ",
      "for example \"tip+middle\"."
    ))
  }
  unique(zones)
}

cutworm_zone_distance <- function(zone, progression, angle_radians, cob_id) {
  if (zone == "tip") {
    return((0.985 - progression) / 0.1 + 0.45 * sin(1.7 * angle_radians + cob_id))
  }
  zone_center <- if (zone == "middle") 0.5 else 0.15
  zone_seed <- if (zone == "middle") 0.37 else 0.71
  entry_angle <- (((zone_seed * cob_id) %% 1) - 0.5) * 1.8
  sqrt(((progression - zone_center) / 0.08)^2 + ((angle_radians - entry_angle) / 0.7)^2)
}

attack_distance <- function(cutworm_zone, progression, angle_radians, cob_id, kernel_row, kernel_rank) {
  distances <- lapply(
    parse_cutworm_zones(cutworm_zone),
    cutworm_zone_distance,
    progression = progression, angle_radians = angle_radians, cob_id = cob_id
  )
  do.call(pmin, distances) + 0.9 * stable_noise(kernel_row + 5, kernel_rank + cob_id)
}

attacked_kernel_color <- function(kernel_color, hollowed_kernel) {
  base_colors <- t(col2rgb(kernel_color))
  attacked_beige <- matrix(col2rgb("#D8BC8A"), nrow(base_colors), 3, byrow = TRUE)
  hollowed_beige <- matrix(col2rgb("#B79568"), nrow(base_colors), 3, byrow = TRUE)
  attacked_colors <- mix_colors(base_colors, attacked_beige, 0.68)
  attacked_colors[hollowed_kernel, ] <- mix_colors(
    base_colors, hollowed_beige, 0.85
  )[hollowed_kernel, , drop = FALSE]
  rgb(attacked_colors, maxColorValue = 255)
}

irregular_outline <- function(polygons, n_sides) {
  polygons |>
    cross_join(tibble(angle_contour = seq(0, 2 * pi, length.out = n_sides + 1)[-1])) |>
    mutate(
      irregularity = 1 + 0.32 * sin(2 * angle_contour + shape_seed) +
        0.2 * sin(3 * angle_contour + 2.3 * shape_seed),
      x_contour = center_x + radius_x * irregularity * cos(angle_contour),
      y_contour = center_y + radius_y * irregularity * sin(angle_contour)
    )
}

compute_holes <- function(attacked_kernels) {
  attacked_kernels |>
    filter(!hollowed_kernel) |>
    transmute(
      cob_id,
      shape_seed = kernel_id,
      center_x = x_grain + 0.1 * kernel_width * (2 * stable_noise(kernel_id, 3) - 1),
      center_y = y_grain + 0.1 * kernel_height * (2 * stable_noise(kernel_id, 5) - 1),
      radius_x = kernel_width * 0.27,
      radius_y = kernel_height * 0.27,
      polygon_color = "#7A5A36",
      draw_rank = 1L
    )
}

compute_crumbs <- function(attacked_kernels, aspect_ratio) {
  crumb_colors <- c("#F6F0E0", "#EFE6D0", "#E2D2AE", "#C7AA78", "#A08050")
  crumbs_per_kernel <- tibble(crumb_rank = 1:3)

  attacked_kernels |>
    cross_join(crumbs_per_kernel) |>
    mutate(
      seed_value = kernel_id + 13 * crumb_rank,
      size_draw = stable_noise(seed_value, 1),
      compact_clump = stable_noise(seed_value, 2) < 0.07,
      radius_y = half_thickness * (0.035 + 0.05 * size_draw) * if_else(compact_clump, 2.4, 1),
      radius_x = radius_y * aspect_ratio,
      local_radius = half_thickness * relative_radius(pmin(x_grain / x, 1), cob_id),
      center_x = x_grain + kernel_width * 2.4 * (2 * stable_noise(seed_value, 3) - 1),
      center_x = pmax(pmin(center_x, x - radius_x * 1.5), 0),
      center_y = y_grain + half_thickness * 0.16 * (2 * stable_noise(seed_value, 4) - 1),
      limit_y = pmax(half_thickness * relative_radius(pmin(center_x / x, 1), cob_id) * 1.05 - radius_y, 0),
      center_y = y + pmax(pmin(center_y - y, limit_y), -limit_y),
      shape_seed = seed_value,
      color_draw = stable_noise(seed_value, 6),
      polygon_color = crumb_colors[1 + floor(color_draw * length(crumb_colors))],
      draw_rank = if_else(compact_clump, 2L, 3L)
    ) |>
    select(
      cob_id, shape_seed, center_x, center_y, radius_x, radius_y, polygon_color, draw_rank
    )
}

damage_backing_outlines <- function(attacked_kernels) {
  attacked_kernels |>
    transmute(
      damage_polygon_id = row_number(),
      shape_seed = kernel_id,
      center_x = x_grain,
      center_y = y_grain,
      radius_x = kernel_width * 0.8,
      radius_y = kernel_height * 0.8,
      polygon_color = "#C2A170"
    ) |>
    irregular_outline(n_sides = 8)
}

cutworm_outlines <- function(attacked_kernels, aspect_ratio) {
  bind_rows(
    compute_holes(attacked_kernels),
    compute_crumbs(attacked_kernels, aspect_ratio)
  ) |>
    arrange(draw_rank, cob_id, shape_seed) |>
    mutate(damage_polygon_id = row_number()) |>
    irregular_outline(n_sides = 8)
}
