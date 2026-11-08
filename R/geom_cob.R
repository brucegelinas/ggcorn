#' Draw corn cobs
#'
#' `geom_cob()` works like [ggplot2::geom_col()], except that each bar is a corn cob
#' with kernels, a tip and husks. The cob narrows toward the tip and its diameter
#' varies along its length. The cob's length, from `0` to the end of the tip,
#' represents the value of `x`.
#'
#' Yield goes on `x` and the category on `y`. Add [ggplot2::coord_flip()] for vertical
#' cobs. Any unit works (index, t/ha, bu/ac), because the tip and husks scale with the
#' range of the data.
#'
#' There is one cob per row, so `position_dodge()` is not supported. Use facets to
#' compare groups.
#'
#' @section Aesthetics:
#' `geom_cob()` requires `x` and `y`. The following aesthetics are optional.
#'
#' * `fill` colors the kernels, which are golden by default. See [scale_fill_cob_c()].
#' * `thousand_kernel_weight` sets the kernel area, which is proportional to weight. The
#'   highest weight on the plot is the reference. Larger kernels therefore mean fewer
#'   kernels on a cob of the same length. Any unit works. By default, neither a label nor
#'   a legend shows the value. Use `weight_label = TRUE` to write it at the end of the cob,
#'   or `guides(thousand_kernel_weight = "legend")` to show one kernel per break (see
#'   [scale_thousand_kernel_weight_continuous()]).
#' * `smut` gives the percentage of kernels covered by common smut galls, from 0 to 100.
#'   Galls are clusters of silvery blisters that swell the cob's outline. As severity
#'   rises, they grow, merge and turn black. The cob keeps its length: galls never go
#'   past the value of `x`.
#' * `western_bean_cutworm` gives the percentage of kernels attacked by western bean
#'   cutworm, from 0 to 100. Kernels are punctured or hollowed, and a mix of flour and
#'   frass spills out of them. `cutworm_zone` sets where the damage is: `"tip"` (the
#'   default), `"middle"`, `"base"` or a combination such as `"tip+base"`. It can be a
#'   column in `aes()` or a fixed value in `geom_cob()`.
#' * `unpollinated` gives the percentage of unpollinated kernels, from 0 to 100. These
#'   kernels are removed starting from the tip, with an irregular edge. The cob keeps its
#'   full length: if `x` is yield, the bare tip is part of the bar.
#'
#' @section Legends:
#' Mapping `smut`, `western_bean_cutworm` or `unpollinated` adds a legend by default, with a
#' mini cob for 0, 25, 50, 75 and 100%. To hide them:
#'
#' ```r
#' # hide one legend
#' plot + guides(smut = "none")
#'
#' # hide several legends
#' plot + guides(smut = "none", western_bean_cutworm = "none", unpollinated = "none")
#'
#' # hide every legend of the cob layer
#' geom_cob(aes(smut = severity), show.legend = FALSE)
#'
#' # hide every legend of the plot
#' plot + theme(legend.position = "none")
#' ```
#'
#' See [scale_smut_continuous()] to change the title or breaks.
#'
#' @param mapping,data,show.legend,inherit.aes,na.rm See [ggplot2::geom_col()].
#' @param ... Other arguments passed to [ggplot2::layer()], such as `fill` for a fixed
#'   color.
#' @param thickness Share of the distance between two rows taken up by the cob. Default:
#'   `0.58`.
#' @param kernels_per_cob Approximate number of kernels along the longest cob, for the
#'   largest kernels on the plot. Aim for about 45 in a small panel and 90 in a
#'   full-width plot. Default: `90`.
#' @param husks `TRUE` draws the husks. `FALSE` removes them and the margin reserved for
#'   them left of zero. Default: `TRUE`.
#' @param weight_amplification Exponent applied to the difference in area between
#'   kernels. `1` keeps area proportional to weight. A larger value exaggerates the
#'   differences, but area is then no longer proportional, so say so on the plot.
#'   Default: `1`.
#' @param legend_kernel_size Height in millimeters of the largest kernel in the legend.
#'   Default: `9`.
#' @param legend_cob_width Width in centimeters of the mini cobs in the legend. Default:
#'   `2.4`.
#' @param weight_label `TRUE` writes `thousand_kernel_weight` at the end of each cob.
#'   Default: `FALSE`.
#' @param weight_unit Unit written in the weight label. Default: `"g"`.
#' @param yield_label `TRUE` writes the value of `x` at the end of each cob, which helps
#'   readers see that length represents yield. If the thousand-kernel weight is also
#'   written, the two values are stacked on two lines, with yield on top. Default: `FALSE`.
#' @param yield_unit Unit written in the yield label. Use `""` for no unit, as with an
#'   index. Default: `"t/ha"`.
#' @param decimal_mark Decimal mark used in the labels, such as `","` for French. Default:
#'   `"."`.
#' @param label_size Label text size, as in [ggplot2::geom_text()]. Default: `4.2`.
#' @param label_family Label font family, such as `"Source Sans Pro"`. Default: `""`, the
#'   device's default font.
#' @return A `ggplot2` layer.
#' @examples
#' library(ggplot2)
#'
#' yields <- data.frame(
#'   rotation = c("Monoculture", "Corn / soybean", "Corn / soybean / cereal"),
#'   grain_yield = c(9.1, 10.2, 10.9)
#' )
#'
#' ggplot(yields, aes(grain_yield, rotation)) +
#'   geom_cob(kernels_per_cob = 40) +
#'   labs(x = "Yield (t/ha)", y = NULL)
#'
#' components <- data.frame(
#'   hybrid = c("A", "B", "C"),
#'   grain_yield = c(10.8, 10.8, 10.8),
#'   kernel_weight = c(270, 320, 370),
#'   unpollinated_kernels = c(0, 12, 30)
#' )
#'
#' ggplot(components, aes(grain_yield, hybrid)) +
#'   geom_cob(
#'     aes(thousand_kernel_weight = kernel_weight, unpollinated = unpollinated_kernels),
#'     kernels_per_cob = 40
#'   )
#'
#' # symptoms add a legend of mini cobs
#' symptoms <- ggplot(components, aes(grain_yield, hybrid)) +
#'   geom_cob(aes(unpollinated = unpollinated_kernels), kernels_per_cob = 40)
#' symptoms
#'
#' # hide that legend
#' symptoms + guides(unpollinated = "none")
#'
#' # write yield at the end of each cob
#' ggplot(yields, aes(grain_yield, rotation)) +
#'   geom_cob(kernels_per_cob = 40, yield_label = TRUE)
#'
#' # yield and weight stack on two lines
#' ggplot(components, aes(grain_yield, hybrid)) +
#'   geom_cob(
#'     aes(thousand_kernel_weight = kernel_weight),
#'     kernels_per_cob = 40,
#'     yield_label = TRUE,
#'     weight_label = TRUE
#'   )
#' @export
geom_cob <- function(mapping = NULL, data = NULL, ..., thickness = 0.58, kernels_per_cob = 90,
                     husks = TRUE, weight_amplification = 1, legend_kernel_size = 9,
                     legend_cob_width = 2.4,
                     weight_label = FALSE, weight_unit = "g", yield_label = FALSE,
                     yield_unit = "t/ha", decimal_mark = ".", label_size = 4.2,
                     label_family = "", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = "identity", geom = GeomCob, position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      thickness = thickness, kernels_per_cob = kernels_per_cob,
      husks = husks, weight_amplification = weight_amplification,
      legend_kernel_size = legend_kernel_size, legend_cob_width = legend_cob_width,
      weight_label = weight_label,
      weight_unit = weight_unit, yield_label = yield_label, yield_unit = yield_unit,
      decimal_mark = decimal_mark, label_size = label_size,
      label_family = label_family, na.rm = na.rm, ...
    )
  )
}

format_label_value <- function(value, unit, decimal_mark = ".") {
  formatted_value <- format(
    round(value, 1),
    trim = TRUE, scientific = FALSE, drop0trailing = TRUE, decimal.mark = decimal_mark
  )
  trimws(paste(formatted_value, unit))
}

build_tip_labels <- function(yield_values, kernel_weights, show_yield, show_weight,
                             yield_unit, weight_unit, decimal_mark) {
  label_lines <- list()
  if (show_yield) {
    label_lines$yield <- format_label_value(yield_values, yield_unit, decimal_mark)
  }
  if (show_weight) {
    label_lines$weight <- format_label_value(kernel_weights, weight_unit, decimal_mark)
  }
  do.call(paste, c(unname(label_lines), sep = "\n"))
}

right_margin_fraction <- function(yield_values, yield_label, show_weight, yield_unit, decimal_mark) {
  if (yield_label) {
    longest_label <- max(nchar(format_label_value(yield_values, yield_unit, decimal_mark)))
    return(0.035 + 0.0165 * longest_label)
  }
  if (show_weight) label_margin_fraction else 0
}

legend_surface_share <- function(legend_weight, reference_weight, weight_amplification) {
  if (is.finite(legend_weight)) (legend_weight / reference_weight)^weight_amplification else 1
}

draw_key_kernel <- function(data, params, size) {
  max_size_mm <- params$legend_kernel_size %||% 9
  surface_share <- legend_surface_share(
    data$thousand_kernel_weight,
    params$reference_weight %||% 1,
    params$weight_amplification %||% 1
  )
  kernel_height_mm <- max_size_mm * sqrt(surface_share)
  outline_angles <- seq(0, 2 * pi, length.out = 33)[-33]
  kernel_roundness <- 2 / kernel_outline_exponent
  horizontal_semi_axis <- sign(cos(outline_angles)) * abs(cos(outline_angles))^kernel_roundness
  vertical_semi_axis <- sign(sin(outline_angles)) * abs(sin(outline_angles))^kernel_roundness

  kernel <- grid::polygonGrob(
    x = unit(0.5, "npc") + unit(0.4 * kernel_height_mm * horizontal_semi_axis, "mm"),
    y = unit(0.5, "npc") + unit(0.5 * kernel_height_mm * vertical_semi_axis, "mm"),
    gp = grid::gpar(fill = data$fill, col = "#7A4A0E", lwd = 0.8)
  )
  key_size_cm <- (max_size_mm + 1.5) / 10
  attr(kernel, "width") <- key_size_cm
  attr(kernel, "height") <- key_size_cm
  kernel
}

polygons_as_grob <- function(polygons) {
  polygon_ids <- sort(unique(polygons$group))
  first_rows <- polygons[match(polygon_ids, polygons$group), ]
  grid::polygonGrob(
    x = key_cob_left_edge + (1 - 2 * key_cob_left_edge) * polygons$x,
    y = polygons$y,
    id = polygons$group,
    gp = grid::gpar(fill = first_rows$fill, col = first_rows$colour, lwd = 0.3)
  )
}

draw_key_cob <- function(data, params, size) {
  unpollinated_key <- data$unpollinated %||% -1
  smut_key <- data$smut %||% -1
  cutworm_key <- data$western_bean_cutworm %||% -1
  if (max(unpollinated_key, smut_key, cutworm_key) < 0) {
    return(draw_key_kernel(data, params, size))
  }

  key_width_cm <- params$legend_cob_width %||% 2.4
  key_height_cm <- key_width_cm * key_height_ratio
  key_cob <- tibble(
    cob_id = 1, x = 1, y = 0.5, half_thickness = key_half_thickness, fill = data$fill,
    kernel_surface_share = key_kernel_surface_share,
    unpollinated = unpollinated_key, smut = smut_key,
    western_bean_cutworm = cutworm_key, cutworm_zone = data$cutworm_zone %||% "tip"
  )
  key_layers <- cob_polygon_layers(
    key_cob,
    reference_kernel_length = 1 / key_kernels_per_cob,
    aspect_ratio = key_height_ratio,
    panel_number = 1
  )
  drawn_layers <- Filter(Negate(is.null), key_layers)
  mini_cob <- do.call(grobTree, lapply(drawn_layers, polygons_as_grob))
  attr(mini_cob, "width") <- key_width_cm
  attr(mini_cob, "height") <- key_height_cm
  mini_cob
}

#' @rdname geom_cob
#' @format NULL
#' @usage NULL
#' @export
GeomCob <- ggproto(
  "GeomCob", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("thousand_kernel_weight", "unpollinated", "smut", "western_bean_cutworm"),
  extra_params = c(
    "na.rm", "thickness", "weight_amplification", "legend_kernel_size", "legend_cob_width"
  ),
  default_aes = aes(
    fill = "#F7A91B", colour = NA, linewidth = 0, linetype = 1, alpha = NA,
    thousand_kernel_weight = Inf, unpollinated = -1, smut = -1,
    western_bean_cutworm = -1, cutworm_zone = "tip"
  ),
  draw_key = draw_key_cob,

  setup_params = function(data, params) {
    params$reference_weight <- max(data$thousand_kernel_weight %||% 1, na.rm = TRUE)
    params
  },

  setup_data = function(data, params) {
    unpollinated_share <- data$unpollinated %||% 0
    kernel_weight <- data$thousand_kernel_weight %||% 1
    if (any(unpollinated_share < 0 | unpollinated_share > 100, na.rm = TRUE)) {
      rlang::abort("`unpollinated` must be a percentage between 0 and 100.")
    }
    smut_share <- data$smut %||% 0
    if (any(smut_share < 0 | smut_share > 100, na.rm = TRUE)) {
      rlang::abort("`smut` must be a percentage between 0 and 100.")
    }
    cutworm_share <- data$western_bean_cutworm %||% 0
    if (any(cutworm_share < 0 | cutworm_share > 100, na.rm = TRUE)) {
      rlang::abort("`western_bean_cutworm` must be a percentage between 0 and 100.")
    }
    if (any(kernel_weight <= 0, na.rm = TRUE)) {
      rlang::abort("`thousand_kernel_weight` must be positive.")
    }
    relative_weight <- kernel_weight / max(kernel_weight, na.rm = TRUE)
    data$kernel_surface_share <- relative_weight^params$weight_amplification
    max_yield <- max(data$x, na.rm = TRUE)
    half_thickness <- params$thickness * resolution(as.numeric(data$y), FALSE) / 2
    left_margin <- if (params$husks) husk_margin_fraction * max_yield else 0
    data$y_numeric <- as.numeric(data$y)
    data$max_yield <- max_yield
    data$xmin <- -left_margin
    show_weight <- params$weight_label && !is.null(data$thousand_kernel_weight)
    margin_fraction <- right_margin_fraction(
      data$x, params$yield_label, show_weight, params$yield_unit, params$decimal_mark
    )
    data$xmax <- data$x + margin_fraction * max_yield
    vertical_margin <- if (is.null(data$smut)) 1.45 else 1.55
    data$ymin <- data$y_numeric - half_thickness * vertical_margin
    data$ymax <- data$y_numeric + half_thickness * vertical_margin
    data$half_thickness <- half_thickness
    data
  },

  draw_panel = function(data, panel_params, coord, kernels_per_cob = 90, husks = TRUE,
                        weight_label = FALSE, weight_unit = "g", yield_label = FALSE,
                        yield_unit = "t/ha", decimal_mark = ".", label_size = 4.2,
                        label_family = "") {
    max_yield <- data$max_yield[1]
    panel_number <- data$PANEL[1]

    cobs <- data |>
      filter(x > 0) |>
      transmute(
        cob_id = row_number(), x, y = y_numeric, half_thickness, fill,
        kernel_surface_share, unpollinated, smut, western_bean_cutworm, cutworm_zone
      )

    draw_polygons <- function(polygons) {
      GeomPolygon$draw_panel(polygons, panel_params, coord)
    }

    cob_layers <- cob_polygon_layers(
      cobs,
      reference_kernel_length = max_yield / kernels_per_cob,
      aspect_ratio = blister_aspect_fraction * max_yield,
      panel_number = panel_number
    )
    cob_grobs <- lapply(cob_layers, function(polygons) {
      if (!is.null(polygons)) draw_polygons(polygons)
    })

    husks_grob <- if (husks) {
      compute_husks(cobs, max_yield) |>
        as_polygons(husk_id, husk_color, panel_number, colour = "#7E6438", linewidth = 0.3) |>
        draw_polygons()
    }

    show_weight <- weight_label && all(is.finite(data$thousand_kernel_weight))
    labels_grob <- if (yield_label || show_weight) {
      labels_data <- data |>
        transmute(
          label = build_tip_labels(
            x, thousand_kernel_weight, yield_label, show_weight,
            yield_unit, weight_unit, decimal_mark
          ),
          x = x + 0.015 * max_yield, y = y_numeric,
          colour = "grey20", size = label_size, angle = 0,
          hjust = if (inherits(coord, "CoordFlip")) 0.5 else 0,
          vjust = if (inherits(coord, "CoordFlip")) 0 else 0.5,
          alpha = NA, family = label_family, fontface = 1, lineheight = 1.2, PANEL
        ) |>
        as.data.frame()
      GeomText$draw_panel(labels_data, panel_params, coord)
    }

    grobTree(
      cob_grobs$cores, cob_grobs$damage_backing, cob_grobs$kernels, cob_grobs$damage,
      cob_grobs$galls, husks_grob, labels_grob
    )
  }
)
