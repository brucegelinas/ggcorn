husk_margin_fraction <- 0.125
husk_root_fraction <- 0.03
label_margin_fraction <- 0.13
key_height_ratio <- 0.36
key_half_thickness <- 0.3
key_kernel_surface_share <- 3
key_kernels_per_cob <- 26
key_cob_left_edge <- 0.02
reference_row_count <- 9
aborted_kernel_color <- "#C8B184"
kernel_outline_exponent <- 3.2
max_clusters <- 90
max_blister_lumpiness <- 1.15
blister_aspect_fraction <- 0.12

husk_template <- tribble(
  ~y_start, ~x_tip,  ~y_end, ~leaf_width, ~husk_color,
   0.83,     -0.103,   1.38,   0.29,     "#B99A63",
  -0.83,     -0.097,  -1.41,   0.29,     "#AD8E58",
   0.52,     -0.112,   0.69,   0.34,     "#CDB27C",
  -0.52,     -0.106,  -0.76,   0.34,     "#C2A670",
   0.17,     -0.116,   0.17,   0.36,     "#DCC594",
  -0.21,     -0.109,  -0.28,   0.34,     "#E6D3A8"
)

mix_colors <- function(colors_rgb, target_colors_rgb, target_share) {
  colors_rgb * (1 - target_share) + target_colors_rgb * target_share
}

shade_kernels <- function(kernel_color, angle_radians) {
  base_colors <- t(col2rgb(kernel_color))
  shadow_colors <- base_colors * 0.6
  highlight_colors <- mix_colors(base_colors, matrix(255, nrow(base_colors), 3), 0.65)
  shadow_share <- (1 - cos(angle_radians))^1.2 * 0.85
  highlight_share <- pmax(0, cos(angle_radians - 0.55))^6 * 0.32
  base_colors |>
    mix_colors(shadow_colors, shadow_share) |>
    mix_colors(highlight_colors, highlight_share) |>
    rgb(maxColorValue = 255)
}

cob_core_color <- function(kernel_color) {
  base_colors <- t(col2rgb(kernel_color))
  cob_brown <- matrix(col2rgb("#7A4A0E"), nrow(base_colors), 3, byrow = TRUE)
  kernel_saturation <- rgb2hsv(t(base_colors))["s", ]
  core_color <- mix_colors(base_colors * 0.45, cob_brown, sqrt(kernel_saturation))
  rgb(pmin(core_color, base_colors * 0.55), maxColorValue = 255)
}

cob_body_profile <- splinefun(
  x = c(0, 0.10, 0.28, 0.50, 0.70, 0.85, 1),
  y = c(0.88, 0.96, 1, 0.97, 0.87, 0.67, 0.38),
  method = "monoH.FC"
)

diameter_profile <- function(progression) {
  rounding_start <- 0.955
  rounding_position <- pmax(0, progression - rounding_start) / (1 - rounding_start)
  cob_body_profile(progression) * sqrt(pmax(0, 1 - rounding_position^2))
}

relative_radius <- function(progression, cob_id) {
  waviness <- 1 + 0.012 * sin(2 * pi * (1.6 * progression + 0.37 * cob_id))
  diameter_profile(progression) * waviness
}

relative_kernel_length <- function(progression) {
  0.5 + 0.5 * diameter_profile(progression)
}

compute_cob_outlines <- function(cobs) {
  progression_steps <- c(seq(0, 0.9, by = 0.01), seq(0.9025, 1, by = 0.0025))
  outline_path <- tibble(
    progression = c(progression_steps, rev(progression_steps)),
    side = rep(c(1, -1), each = length(progression_steps))
  )

  cobs |>
    cross_join(outline_path) |>
    mutate(
      x_contour = x * progression,
      y_contour = y + side * half_thickness * relative_radius(progression, cob_id)
    )
}

progression_from_rank <- function(kernel_rank, x, kernel_length) {
  grid_points <- seq(0, 1, length.out = 400)
  step_in_kernels <- diff(grid_points) * x / kernel_length /
    relative_kernel_length(grid_points[-length(grid_points)])
  approx(c(0, cumsum(step_in_kernels)), grid_points, xout = kernel_rank)$y
}

size_kernels <- function(cobs, reference_kernel_length) {
  cobs |>
    mutate(
      n_rows = pmin(pmax(round(reference_row_count / sqrt(kernel_surface_share)), 5), 25),
      height_factor = reference_row_count / n_rows,
      kernel_length = reference_kernel_length * kernel_surface_share / height_factor
    )
}

stable_noise <- function(kernel_row, kernel_number) {
  (sin(kernel_row * 12.9898 + kernel_number * 78.233) * 43758.5453) %% 1
}

kernel_inside_cob <- function(progression, relative_half_width, row_center,
                           relative_half_height, cob_id) {
  radius <- relative_radius(progression, cob_id)
  fits_outline <- function(width_fraction) {
    remaining_height <- (1 - width_fraction^kernel_outline_exponent)^(1 / kernel_outline_exponent)
    checked_progression <- progression + width_fraction * relative_half_width
    kernel_extent <- (row_center + relative_half_height * remaining_height) * radius
    kernel_extent <= 1.02 * relative_radius(checked_progression, cob_id)
  }
  Reduce(`&`, lapply(c(0.4, 0.6, 0.8, 0.95), fits_outline))
}

compute_kernels <- function(cobs, reference_kernel_length) {
  kernel_roundness <- 2 / kernel_outline_exponent

  kernels <- cobs |>
    size_kernels(reference_kernel_length) |>
    reframe(
      expand_grid(
        kernel_row = seq_len(n_rows),
        kernel_rank = seq(0.5, 2 * x / kernel_length + 1, by = 1)
      ),
      .by = c(
        cob_id, x, y, half_thickness, fill, n_rows, kernel_length, unpollinated,
        western_bean_cutworm, cutworm_zone
      )
    ) |>
    mutate(
      kernel_rank = kernel_rank + if_else(kernel_row %% 2 == 0, 0.5, 0),
      progression = progression_from_rank(kernel_rank, x[1], kernel_length[1]),
      .by = cob_id
    ) |>
    filter(!is.na(progression)) |>
    mutate(
      x_grain = x * progression,
      radius = relative_radius(progression, cob_id),
      half_row_gap = pi / (2 * n_rows),
      angle_radians = -pi / 2 + (2 * kernel_row - 1) * half_row_gap,
      y_grain = y + half_thickness * radius * sin(angle_radians) * cos(half_row_gap),
      relative_height = 0.96 * (sin(angle_radians + half_row_gap) - sin(angle_radians - half_row_gap)),
      kernel_height = half_thickness * radius * relative_height,
      kernel_width = kernel_length * 0.94 * relative_kernel_length(progression),
      relative_half_width = kernel_width / (2 * x),
      row_center = abs(sin(angle_radians)) * cos(half_row_gap)
    ) |>
    filter(
      progression + relative_half_width < 0.995,
      kernel_inside_cob(progression, relative_half_width, row_center, relative_height / 2, cob_id)
    ) |>
    mutate(
      irregular_position = progression + 0.035 * stable_noise(kernel_row, kernel_rank),
      rank_from_tip = rank(-irregular_position, ties.method = "first"),
      aborted_kernel = rank_from_tip <= round(n() * unpollinated / 100),
      .by = cob_id
    ) |>
    mutate(
      damage_distance = attack_distance(
        cutworm_zone[1], progression, angle_radians, cob_id[1], kernel_row, kernel_rank
      ),
      attack_rank = rank(damage_distance, ties.method = "first"),
      attacked_kernel = attack_rank <= round(n() * western_bean_cutworm / 100),
      .by = cob_id
    ) |>
    mutate(
      kernel_id = row_number(),
      hollowed_kernel = attacked_kernel & stable_noise(kernel_id, 9) < 0.38,
      visible_color = if_else(aborted_kernel, aborted_kernel_color, fill),
      visible_color = if_else(
        attacked_kernel, attacked_kernel_color(visible_color, hollowed_kernel), visible_color
      ),
      kernel_color = shade_kernels(visible_color, angle_radians),
      kernel_shrinkage = case_when(aborted_kernel ~ 0.5, hollowed_kernel ~ 0.68, .default = 1),
      kernel_width = kernel_width * kernel_shrinkage,
      kernel_height = kernel_height * kernel_shrinkage
    )

  kernels |>
    cross_join(tibble(angle_contour = seq(0, 2 * pi, length.out = 17)[-17])) |>
    mutate(
      x_contour = x_grain + kernel_width / 2 *
        sign(cos(angle_contour)) * abs(cos(angle_contour))^kernel_roundness,
      y_contour = y_grain + kernel_height / 2 *
        sign(sin(angle_contour)) * abs(sin(angle_contour))^kernel_roundness
    )
}

compute_husks <- function(cobs, max_yield) {
  husk_positions <- seq(0, 1, length.out = 50)
  husk_path <- tibble(
    progression = c(husk_positions, rev(husk_positions)),
    side = rep(c(1, -1), each = 50)
  )
  x_root <- husk_root_fraction * max_yield

  cobs |>
    select(cob_id, y, half_thickness) |>
    cross_join(mutate(husk_template, leaf_id = row_number())) |>
    cross_join(husk_path) |>
    mutate(
      x_leaf_tip = x_tip * max_yield,
      x_contour = x_root + (x_leaf_tip - x_root) * progression,
      y_center = y + half_thickness * (y_start + (y_end - y_start) * progression^2.2),
      y_contour = y_center + side * half_thickness * leaf_width * sin(pi * progression^0.5),
      husk_id = consecutive_id(cob_id, leaf_id)
    )
}

cob_polygon_layers <- function(cobs, reference_kernel_length, aspect_ratio, panel_number) {
  kernels <- compute_kernels(cobs, reference_kernel_length)
  attacked_kernels <- kernels |>
    filter(attacked_kernel) |>
    distinct(cob_id, kernel_id, .keep_all = TRUE)
  has_damage <- nrow(attacked_kernels) > 0

  list(
    cores = compute_cob_outlines(cobs) |>
      as_polygons(cob_id, cob_core_color(fill), panel_number),
    damage_backing = if (has_damage) {
      attacked_kernels |>
        damage_backing_outlines() |>
        as_polygons(damage_polygon_id, polygon_color, panel_number)
    },
    kernels = as_polygons(kernels, kernel_id, kernel_color, panel_number),
    damage = if (has_damage) {
      attacked_kernels |>
        cutworm_outlines(aspect_ratio) |>
        as_polygons(damage_polygon_id, polygon_color, panel_number)
    },
    galls = if (any(cobs$smut > 0)) {
      compute_blisters(cobs, kernels, aspect_ratio) |>
        blister_outlines() |>
        as_polygons(
          blister_polygon_id, polygon_color, panel_number,
          colour = polygon_outline, linewidth = 0.2
        )
    }
  )
}

as_polygons <- function(outlines, group, fill, panel_number, colour = NA, linewidth = 0) {
  outlines |>
    transmute(
      x = x_contour, y = y_contour, group = {{ group }}, fill = {{ fill }},
      colour = {{ colour }}, linewidth = linewidth, linetype = 1, alpha = NA,
      PANEL = panel_number
    ) |>
    as.data.frame()
}
