#' @keywords internal
#' @importFrom dplyr arrange bind_rows case_when consecutive_id cross_join distinct filter if_else mutate n
#' @importFrom dplyr reframe row_number select transmute
#' @importFrom ggplot2 Geom GeomPolygon GeomText aes continuous_scale discrete_scale ggproto layer
#' @importFrom ggplot2 element_blank element_text expansion facet_grid geom_raster ggplot
#' @importFrom ggplot2 margin resolution scale_y_continuous vars
#' @importFrom ggplot2 scale_fill_gradientn scale_fill_identity scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous theme theme_minimal waiver
#' @importFrom grDevices col2rgb colorRamp colorRampPalette rgb rgb2hsv
#' @importFrom tibble tibble tribble
#' @importFrom grid grobTree unit
#' @importFrom rlang %||%
#' @importFrom scales gradient_n_pal squish
#' @importFrom stats approx setNames splinefun
#' @importFrom tidyr expand_grid
#' @importFrom utils globalVariables
"_PACKAGE"

globalVariables(c(
  "attacked_kernel", "blister_polygon_id", "damage_polygon_id", "husk_id", "kernel_color",
  "kernel_id", "polygon_color", "polygon_outline",
  "palette_name", "strip_color", "strip_position",
  "PANEL", "maturity_draw", "cluster_angle", "blister_angle", "angle_contour", "angle_radians",
  "lumpiness", "row_center", "center_x", "center_y", "smut", "ruptured_blister",
  "polygon_outline", "side", "layer_name", "body_color", "kernel_color", "polygon_color",
  "husk_color", "axis_offset", "offset_x", "offset_y", "half_row_gap", "half_thickness",
  "relative_half_width", "cluster_distance", "layer_scale", "height_factor", "fill",
  "aborted_kernel", "shape_seed", "kernel_height", "relative_height", "cluster_id", "cob_id",
  "leaf_id", "kernel_id", "blister_polygon_id", "husk_id", "leaf_width", "kernel_width",
  "limit_x", "limit_y", "kernel_length", "n_rows", "unpollinated", "depth_order",
  "kernel_surface_share", "thousand_kernel_weight", "irregular_position", "progression",
  "cluster_progression", "blister_rank", "layer_rank", "rank_in_cluster", "rank_from_tip",
  "kernel_rank", "kernel_row", "radius", "cluster_radius", "cob_radius", "radius_x", "radius_y",
  "cluster_size", "blister_tint", "cluster_transverse", "x", "x_tip", "x_leaf_tip",
  "x_contour", "x_grain", "y", "y_center", "y_contour", "y_start", "y_end", "y_grain",
  "y_numeric", "color_draw", "size_draw", "compact_clump", "visible_color",
  "damage_distance", "seed_value", "attacked_kernel", "hollowed_kernel", "damage_polygon_id",
  "irregularity", "attack_rank", "draw_rank", "local_radius", "kernel_shrinkage", "western_bean_cutworm",
  "cutworm_zone"
))
