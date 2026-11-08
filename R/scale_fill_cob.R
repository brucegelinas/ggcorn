#' Kernel palettes
#'
#' Named list of colors used by [scale_fill_cob_c()] and [scale_fill_cob_d()]. `maturity`
#' is designed for yield. The others run from a healthy to an affected kernel: `disease`
#' is generic (yellow to brown), `gibberella`, `fusarium`, `diplodia` and `aspergillus`
#' suggest the mold color of these ear rots, and `smut` that of common smut. These colors
#' are schematic. To draw smut galls, see the `smut` aesthetic of [geom_cob()].
#'
#' \if{html}{\figure{README-palettes.png}{options: width="100\%" alt="The seven palettes shown on cobs, from 0 to 100\%"}}
#'
#' @format Named list of hexadecimal color vectors.
#' @seealso [show_cob_palettes()] to draw the colors.
#' @examples
#' # draw all the palettes
#' show_cob_palettes()
#'
#' # draw only some of them
#' show_cob_palettes(c("disease", "smut"))
#'
#' # list the names and hexadecimal codes
#' names(palettes_cob)
#' palettes_cob$disease
#' @export
palettes_cob <- list(
  maturity    = c("#F0D68E", "#F7A91B"),
  disease     = c("#F7C93E", "#C98A2B", "#6B4213"),
  gibberella  = c("#F7C93E", "#EE9CA0", "#B5303F"),
  fusarium   = c("#F7C93E", "#F6E3DC", "#E8A79A"),
  diplodia    = c("#F7C93E", "#F3EBD3", "#FBFAF5"),
  aspergillus = c("#F7C93E", "#B3AE3C", "#5E6B2A"),
  smut     = c("#F7C93E", "#C9C6B4", "#3A3734")
)

#' Draw the cob palettes
#'
#' Shows each palette as a color strip, so the colors can be seen rather than read as
#' hexadecimal codes. The strips use the same color interpolation as [scale_fill_cob_c()].
#'
#' @param palettes Names of the palettes to draw, from [palettes_cob]. Default: all of
#'   them.
#' @return A `ggplot2` plot, which can be modified like any other.
#' @examples
#' show_cob_palettes()
#'
#' show_cob_palettes(c("fusarium", "gibberella"))
#' @export
show_cob_palettes <- function(palettes = names(palettes_cob)) {
  palettes <- rlang::arg_match(palettes, names(palettes_cob), multiple = TRUE)

  color_strips <- expand_grid(
    palette_name = factor(palettes, levels = palettes),
    strip_position = 0:100
  ) |>
    mutate(
      strip_color = gradient_n_pal(palettes_cob[[as.character(palette_name[1])]])(strip_position / 100),
      .by = palette_name
    )

  ggplot(color_strips, aes(strip_position, 0, fill = strip_color)) +
    geom_raster() +
    facet_grid(rows = vars(palette_name), switch = "y") +
    scale_fill_identity() +
    scale_x_continuous(
      name = NULL,
      breaks = c(0, 100),
      labels = c("Healthy or low", "Affected or high"),
      expand = expansion(0)
    ) +
    scale_y_continuous(name = NULL, breaks = NULL, expand = expansion(0)) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      panel.spacing.y = unit(6, "pt"),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      plot.margin = margin(8, 72, 8, 8)
    )
}

cob_palette_colors <- function(palette, direction) {
  colors <- if (length(palette) > 1) {
    palette
  } else {
    palettes_cob[[rlang::arg_match(palette, names(palettes_cob))]]
  }
  if (direction < 0) rev(colors) else colors
}

default_limits <- function(palette, direction) {
  disease_palette <- length(palette) == 1 && palette != "maturity" && direction > 0
  if (!disease_palette) {
    return(NULL)
  }
  function(data_range) {
    if (data_range[1] >= 0 && data_range[2] <= 100) c(0, 100) else data_range
  }
}

#' Color kernels with a cob palette
#'
#' `scale_fill_cob_c()` works with a continuous variable, such as percent severity.
#' `scale_fill_cob_d()` works with classes.
#'
#' With a disease palette, `scale_fill_cob_c()` reads values as percentages from 0 to 100,
#' so a low severity stays yellow even if it is the highest on the plot. For another
#' scale, such as a 1 to 9 rating, set `limits = c(1, 9)`. To stretch the palette across
#' the range of the data, set `limits = NULL`. If values fall outside 0 to 100, the
#' palette follows the range of the data. `scale_fill_cob_d()` spreads classes across the
#' whole palette, unless all possible classes are given in `levels`.
#'
#' @param palette Name of a palette in [palettes_cob], or a vector of at least two colors
#'   for a custom palette. Default: `"maturity"`.
#' @param direction `1` keeps the palette's order and `-1` reverses it. Reversing
#'   `"disease"` makes low values brown, which is useful for coloring by yield. Default:
#'   `1`.
#' @param limits Values matching the two ends of the palette. Values outside them take
#'   the color of the nearest end. Default: `0` to `100` for a disease palette, otherwise
#'   the range of the data.
#' @param levels All possible classes of the scale, such as `1:9`, even those absent from
#'   the data. A class's color then depends on its position on that scale. Default:
#'   `NULL`, which spreads the classes present across the whole palette.
#' @param ... Other arguments passed to [ggplot2::scale_fill_gradientn()],
#'   [ggplot2::discrete_scale()] or [ggplot2::scale_fill_manual()], such as `name`.
#' @return A `ggplot2` scale.
#' @examples
#' library(ggplot2)
#'
#' trial <- data.frame(
#'   hybrid = c("A", "B", "C"),
#'   grain_yield = c(11.2, 10.6, 9.4),
#'   severity = c(5, 30, 70)
#' )
#'
#' ggplot(trial, aes(grain_yield, hybrid, fill = severity)) +
#'   geom_cob(kernels_per_cob = 40) +
#'   scale_fill_cob_c("disease")
#'
#' ggplot(trial, aes(grain_yield, hybrid, fill = factor(c(2, 3, 4)))) +
#'   geom_cob(kernels_per_cob = 40) +
#'   scale_fill_cob_d("fusarium", levels = 1:9)
#'
#' ggplot(trial, aes(grain_yield, hybrid, fill = severity)) +
#'   geom_cob(kernels_per_cob = 40) +
#'   scale_fill_cob_c(c("#F7C93E", "#7A3E9D"))
#' @export
scale_fill_cob_c <- function(palette = "maturity", direction = 1,
                             limits = default_limits(palette, direction), ...) {
  scale_fill_gradientn(
    colours = cob_palette_colors(palette, direction),
    limits = limits, oob = squish, ...
  )
}

#' @rdname scale_fill_cob_c
#' @export
scale_fill_cob_d <- function(palette = "maturity", direction = 1, levels = NULL, ...) {
  interpolate_colors <- colorRampPalette(cob_palette_colors(palette, direction))
  if (is.null(levels)) {
    return(discrete_scale("fill", palette = interpolate_colors, ...))
  }
  level_colors <- setNames(interpolate_colors(length(levels)), as.character(levels))
  scale_fill_manual(values = level_colors, ...)
}

severity_scale <- function(aesthetic, name, breaks, labels, guide, limits, ...) {
  continuous_scale(
    aesthetic,
    palette = function(values_used) values_used,
    rescaler = function(values_used, ...) values_used,
    limits = limits,
    name = name, breaks = breaks, labels = labels, guide = guide, ...
  )
}

percent_labels <- function(values_used) paste0(values_used, "%")

#' Set the smut, cutworm and unpollinated legends
#'
#' These scales control the legends of the `smut`, `western_bean_cutworm` and `unpollinated`
#' aesthetics of [geom_cob()]. Each key is a mini cob, without husks, drawn with the same code
#' as the plot, so it shows the symptom at that severity. The scales apply on their own when an
#' aesthetic is used and `ggcorn` is loaded with `library()`. Call them only to change the
#' title, the breaks, the labels or the guide.
#'
#' By default, the severity scale runs from 0 to 100, so a key means the same thing on every
#' plot. For low severities, narrow it with `limits`, such as `limits = c(0, 20)`: the breaks
#' follow, and a cob whose value falls outside `limits` is removed with a warning. Keys for `western_bean_cutworm` show damage at the tip, unless `cutworm_zone` is a fixed
#' value in [geom_cob()]. When two aesthetics use the same variable, title and breaks, `ggplot2`
#' merges them into one legend, and each key shows both symptoms.
#'
#' @section Hiding the legends:
#' These legends are shown by default. To hide them:
#'
#' ```r
#' # hide one legend
#' plot + guides(smut = "none")
#'
#' # hide several legends
#' plot + guides(smut = "none", western_bean_cutworm = "none", unpollinated = "none")
#'
#' # hide it in the scale
#' plot + scale_smut_continuous(guide = "none")
#'
#' # hide every legend of the cob layer
#' geom_cob(aes(smut = severity), show.legend = FALSE)
#'
#' # hide every legend of the plot
#' plot + theme(legend.position = "none")
#' ```
#'
#' @param name Legend title. Default: the name of the mapped variable.
#' @param limits Range of the severity scale. Default: `c(0, 100)`.
#' @param breaks Severity values to show. Default: five evenly spaced values across
#'   `limits`, which gives `c(0, 25, 50, 75, 100)` for the default limits.
#' @param labels Function or vector giving the labels. Default: `percent_labels`, which writes
#'   `25%`.
#' @param guide `"legend"` shows the legend and `"none"` hides it. Default: `"legend"`.
#' @param ... Other arguments passed to [ggplot2::continuous_scale()].
#' @return A `ggplot2` scale.
#' @examples
#' library(ggplot2)
#'
#' trial <- data.frame(
#'   hybrid = c("A", "B", "C"),
#'   grain_yield = c(11, 10.5, 9.5),
#'   smut_severity = c(5, 30, 60)
#' )
#'
#' cob_plot <- ggplot(trial, aes(grain_yield, hybrid)) +
#'   geom_cob(aes(smut = smut_severity), kernels_per_cob = 40)
#'
#' # the legend appears on its own
#' cob_plot
#'
#' # set the title and breaks
#' cob_plot +
#'   scale_smut_continuous(name = "Smut (%)", breaks = c(0, 50, 100))
#'
#' # zoom in on low severities
#' cob_plot +
#'   scale_smut_continuous(limits = c(0, 60))
#'
#' # hide the legend
#' cob_plot + guides(smut = "none")
#' @export
scale_smut_continuous <- function(name = waiver(),
                                  breaks = seq(limits[1], limits[2], length.out = 5),
                                  labels = percent_labels, guide = "legend",
                                  limits = c(0, 100), ...) {
  severity_scale("smut", name, breaks, labels, guide, limits, ...)
}

#' @rdname scale_smut_continuous
#' @export
scale_western_bean_cutworm_continuous <- function(name = waiver(),
                                                  breaks = seq(limits[1], limits[2], length.out = 5),
                                                  labels = percent_labels, guide = "legend",
                                                  limits = c(0, 100), ...) {
  severity_scale("western_bean_cutworm", name, breaks, labels, guide, limits, ...)
}

#' @rdname scale_smut_continuous
#' @export
scale_unpollinated_continuous <- function(name = waiver(),
                                          breaks = seq(limits[1], limits[2], length.out = 5),
                                          labels = percent_labels, guide = "legend",
                                          limits = c(0, 100), ...) {
  severity_scale("unpollinated", name, breaks, labels, guide, limits, ...)
}

#' Set the thousand-kernel weight legend
#'
#' The legend is hidden by default. To show it, add
#' `guides(thousand_kernel_weight = "legend")` or set `guide = "legend"` here. This scale
#' applies on its own when `thousand_kernel_weight` is used and `ggcorn` is loaded with
#' `library()`. In the legend, kernel area follows weight as it does on the cobs, but the
#' absolute size does not match the kernels on the cobs: adjust `legend_kernel_size` in
#' [geom_cob()] to get closer.
#'
#' @param name Legend title. Default: the name of the mapped variable.
#' @param breaks Values to show. They stay in the legend even if they fall outside the
#'   range of the data. Default: computed from the data.
#' @param guide `"legend"` shows the legend and `"none"` hides it. Default: `"none"`.
#' @param ... Other arguments passed to [ggplot2::continuous_scale()], such as `labels`.
#' @return A `ggplot2` scale.
#' @examples
#' library(ggplot2)
#'
#' trial <- data.frame(hybrid = c("A", "B", "C"), grain_yield = 11, tkw = c(280, 330, 380))
#'
#' ggplot(trial, aes(grain_yield, hybrid)) +
#'   geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 40) +
#'   scale_thousand_kernel_weight_continuous(
#'     name = "1000-kernel weight (g)",
#'     breaks = c(280, 330, 380),
#'     guide = "legend"
#'   )
#' @export
scale_thousand_kernel_weight_continuous <- function(name = waiver(), breaks = waiver(),
                                                    guide = "none", ...) {
  limits_with_breaks <- function(data_limits) {
    if (is.numeric(breaks)) range(data_limits, breaks) else data_limits
  }
  continuous_scale(
    "thousand_kernel_weight",
    palette = function(values_used) values_used,
    rescaler = function(values_used, ...) values_used,
    limits = limits_with_breaks,
    name = name, breaks = breaks, guide = guide, ...
  )
}
