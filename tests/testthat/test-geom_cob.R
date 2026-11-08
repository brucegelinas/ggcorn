library(ggplot2)

trial_tha <- data.frame(
  rotation = factor(c("Monoculture", "Corn / soybean", "Corn / soybean / cereal")),
  grain_yield = c(9.1, 10.2, 10.9)
)

create_cob <- function(x = 12, kernel_surface_share = 1, unpollinated = 0) {
  tibble::tibble(
    cob_id = 1, x = x, y = 1, half_thickness = 0.3,
    fill = "#F7A91B", kernel_surface_share = kernel_surface_share, unpollinated = unpollinated,
    western_bean_cutworm = 0, cutworm_zone = "tip"
  )
}

distinct_kernels <- function(cob, reference_kernel_length = cob$x / 60) {
  ggcorn:::compute_kernels(cob, reference_kernel_length) |>
    dplyr::distinct(kernel_id, .keep_all = TRUE)
}

test_that("the plot builds with a factor or a numeric y", {
  with_factor <- ggplot(trial_tha, aes(grain_yield, rotation)) + geom_cob(kernels_per_cob = 20)
  with_numeric <- ggplot(trial_tha, aes(grain_yield, seq_along(grain_yield))) + geom_cob(kernels_per_cob = 20)
  expect_s3_class(ggplotGrob(with_factor), "gtable")
  expect_s3_class(ggplotGrob(with_numeric), "gtable")
})

test_that("facets and coord_flip work", {
  two_sites <- rbind(transform(trial_tha, site = "A"), transform(trial_tha, site = "B"))
  plot_object <- ggplot(two_sites, aes(grain_yield, rotation)) +
    geom_cob(kernels_per_cob = 20) +
    facet_wrap(vars(site)) +
    coord_flip()
  expect_s3_class(ggplotGrob(plot_object), "gtable")
})

test_that("a missing value removes the cob and warns", {
  with_missing <- transform(trial_tha, grain_yield = replace(grain_yield, 2, NA))
  plot_object <- ggplot(with_missing, aes(grain_yield, rotation)) + geom_cob(kernels_per_cob = 20)
  expect_warning(ggplotGrob(plot_object), "missing")
})

test_that("the left margin follows the husks argument", {
  layer_values_for <- function(...) {
    layer_data(ggplot(trial_tha, aes(grain_yield, rotation)) + geom_cob(...))
  }
  expect_lt(unique(layer_values_for(husks = TRUE)$xmin), 0)
  expect_equal(unique(layer_values_for(husks = FALSE)$xmin), 0)
})

test_that("cob length represents exactly the value of x", {
  cobs <- tibble::tibble(cob_id = 1:2, x = c(9, 12), y = 1:2, half_thickness = 0.3, fill = "#F7A91B")
  outlines <- ggcorn:::compute_cob_outlines(cobs)
  extents <- dplyr::summarise(outlines, start = min(x_contour), end = max(x_contour), .by = cob_id)
  expect_equal(extents$start, c(0, 0))
  expect_equal(extents$end, c(9, 12))
})

test_that("the cob narrows toward the tip and its diameter varies", {
  diameter <- ggcorn:::diameter_profile
  expect_lt(diameter(0), diameter(0.28))
  expect_gt(diameter(0.5), diameter(0.75))
  expect_gt(diameter(0.75), diameter(0.9))
  expect_gt(diameter(0.9), diameter(0.98))
  expect_gt(diameter(0.98), 0)
  expect_equal(diameter(1), 0)
})

test_that("all cobs share one shape, stretched along x", {
  mid_length_radius <- function(x) {
    cob <- tibble::tibble(cob_id = 1, x = x, y = 0, half_thickness = 0.3, fill = "#F7A91B")
    outlines <- ggcorn:::compute_cob_outlines(cob)
    max(outlines$y_contour[outlines$progression == 0.5])
  }
  expect_equal(mid_length_radius(9), mid_length_radius(120))
})

test_that("kernels stay inside the cob", {
  kernels <- ggcorn:::compute_kernels(create_cob(), 12 / 60)
  expect_gte(min(kernels$x_contour), 0)
  expect_lte(max(kernels$x_contour), 12)
  limit <- 0.3 * ggcorn:::relative_radius(kernels$x_contour / 12, 1)
  expect_lte(max(abs(kernels$y_contour - 1) - limit), 0.3 * 0.03)
})

test_that("the drawing does not depend on the yield unit", {
  expect_equal(nrow(distinct_kernels(create_cob(x = 12))), nrow(distinct_kernels(create_cob(x = 190.8))))
})

test_that("without aesthetics, the cob keeps nine rows", {
  expect_equal(dplyr::n_distinct(distinct_kernels(create_cob())$kernel_row), 9)
})

test_that("unpollinated removes the right percentage of kernels", {
  for (percent in c(5, 10, 25, 50)) {
    kernels <- distinct_kernels(create_cob(unpollinated = percent))
    expect_equal(mean(kernels$aborted_kernel) * 100, percent, tolerance = 0.02)
  }
  expect_false(any(distinct_kernels(create_cob(unpollinated = 0))$aborted_kernel))
  expect_true(all(distinct_kernels(create_cob(unpollinated = 100))$aborted_kernel))
})

test_that("unpollinated kernels start from the tip", {
  kernels <- distinct_kernels(create_cob(unpollinated = 20))
  expect_gt(min(kernels$x_grain[kernels$aborted_kernel]), max(kernels$x_grain) * 0.7)
  expect_lt(mean(kernels$x_grain[!kernels$aborted_kernel]), mean(kernels$x_grain[kernels$aborted_kernel]))
})

test_that("kernel area follows weight and the kernel count falls", {
  large_kernels <- distinct_kernels(create_cob(kernel_surface_share = 1))
  small_kernels <- distinct_kernels(create_cob(kernel_surface_share = 0.5))
  central_surface <- function(kernels) {
    center <- kernels$progression > 0.25 & kernels$progression < 0.35 &
      abs(sin(kernels$angle_radians)) < 0.3
    mean(kernels$kernel_width[center] * kernels$kernel_height[center])
  }
  large_surface <- central_surface(large_kernels)
  small_surface <- central_surface(small_kernels)
  expect_equal(small_surface / large_surface, 0.5, tolerance = 0.05)
  expect_equal(nrow(small_kernels) / nrow(large_kernels), 2, tolerance = 0.1)
})

test_that("the highest weight is the reference across facets", {
  two_sites <- data.frame(
    site = c("A", "A", "B"), hybrid = c("x", "y", "x"),
    grain_yield = 10, tkw = c(200, 300, 400)
  )
  layer_values <- layer_data(
    ggplot(two_sites, aes(grain_yield, hybrid)) +
      geom_cob(aes(thousand_kernel_weight = tkw)) +
      facet_wrap(vars(site))
  )
  expect_equal(sort(layer_values$kernel_surface_share), c(0.5, 0.75, 1))
})

test_that("weight_amplification widens the gap", {
  layer_values <- layer_data(
    ggplot(data.frame(h = c("a", "b"), r = 10, p = c(200, 400)), aes(r, h)) +
      geom_cob(aes(thousand_kernel_weight = p), weight_amplification = 2)
  )
  expect_equal(min(layer_values$kernel_surface_share), 0.25)
})

test_that("impossible values give a clear error", {
  trial <- data.frame(h = "a", r = 10, value = 120)
  expect_error(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(unpollinated = value))),
    "between 0 and 100"
  )
  trial$value <- -5
  expect_error(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(thousand_kernel_weight = value))),
    "positive"
  )
})

test_that("a missing percentage removes the cob and warns", {
  trial <- data.frame(h = c("a", "b"), r = 10, value = c(10, NA))
  expect_warning(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(unpollinated = value), kernels_per_cob = 20)),
    "missing"
  )
})

test_that("grooves stay darker than kernels in every palette", {
  colors <- unlist(palettes_cob)
  cores <- ggcorn:::cob_core_color(colors)
  expect_true(all(col2rgb(cores) <= col2rgb(colors)))
})

test_that("the legend shows the thousand-kernel weight breaks", {
  trial <- data.frame(h = c("a", "b"), r = 10, tkw = c(280, 380))
  plot_object <- ggplot(trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20) +
    scale_thousand_kernel_weight_continuous(breaks = c(280, 380), guide = "legend")
  guide <- get_guide_data(plot_object, "thousand_kernel_weight")
  expect_equal(guide$thousand_kernel_weight, c(280, 380))
  expect_s3_class(ggplotGrob(plot_object), "gtable")
})

test_that("the legend scale does not change kernel area", {
  trial <- data.frame(h = c("a", "b"), r = 10, tkw = c(190, 380))
  plot_object <- ggplot(trial, aes(r, h)) + geom_cob(aes(thousand_kernel_weight = tkw))
  with_scale <- plot_object + scale_thousand_kernel_weight_continuous()
  expect_equal(sort(layer_data(with_scale)$kernel_surface_share), c(0.5, 1))
})

test_that("legend kernel area follows weight", {
  expect_equal(ggcorn:::legend_surface_share(190, 380, 1), 0.5)
  expect_equal(ggcorn:::legend_surface_share(190, 380, 2), 0.25)
  expect_equal(ggcorn:::legend_surface_share(Inf, 380, 1), 1)
})

test_that("legend_kernel_size sets the legend key", {
  kernel <- ggcorn:::draw_key_kernel(
    data.frame(fill = "#F7A91B", thousand_kernel_weight = 300),
    list(legend_kernel_size = 12, reference_weight = 300, weight_amplification = 1),
    size = 1
  )
  expect_s3_class(kernel, "grob")
  expect_equal(attr(kernel, "height"), 1.35)
})

test_that("a break outside the data range stays in the legend", {
  trial <- data.frame(h = c("a", "b"), r = 10, tkw = c(285, 380))
  plot_object <- ggplot(trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20) +
    scale_thousand_kernel_weight_continuous(breaks = c(280, 330, 380), guide = "legend")
  guide <- get_guide_data(plot_object, "thousand_kernel_weight")
  expect_equal(guide$thousand_kernel_weight, c(280, 330, 380))
  expect_equal(sort(layer_data(plot_object)$kernel_surface_share), c(285 / 380, 1))
})

weight_trial <- data.frame(
  h = c("a", "b"), r = c(9, 10), tkw = c(328.5, 372), level_label = factor(c(2, 4))
)

n_grobs <- function(plot_object) {
  length(layer_grob(plot_object)[[1]]$children)
}

test_that("the weight label uses the unit", {
  expect_equal(ggcorn:::format_label_value(c(328.5, 372), "g"), c("328.5 g", "372 g"))
  expect_equal(ggcorn:::format_label_value(0.3285, "kg"), "0.3 kg")
  expect_equal(ggcorn:::format_label_value(c(328.5, 372), "g", ","), c("328,5 g", "372 g"))
  expect_equal(ggcorn:::format_label_value(100000, "kg/ha"), "100000 kg/ha")
  expect_equal(ggcorn:::format_label_value(11.6, ""), "11.6")
})

test_that("the weight label appears only when requested and weight is used", {
  without_weight <- ggplot(weight_trial, aes(r, h)) + geom_cob(kernels_per_cob = 20)
  with_label <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20, weight_label = TRUE)
  by_default <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20)
  label_without_weight <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(kernels_per_cob = 20, weight_label = TRUE)
  expect_equal(n_grobs(with_label), n_grobs(without_weight) + 1)
  expect_equal(n_grobs(by_default), n_grobs(without_weight))
  expect_equal(n_grobs(label_without_weight), n_grobs(without_weight))
})

test_that("the axis keeps room for the label only if there is one", {
  without_weight <- layer_data(ggplot(weight_trial, aes(r, h)) + geom_cob())
  with_weight <- layer_data(ggplot(weight_trial, aes(r, h)) + geom_cob(aes(thousand_kernel_weight = tkw)))
  with_label <- layer_data(
    ggplot(weight_trial, aes(r, h)) + geom_cob(aes(thousand_kernel_weight = tkw), weight_label = TRUE)
  )
  expect_equal(without_weight$xmax, without_weight$x)
  expect_equal(with_weight$xmax, with_weight$x)
  expect_gt(min(with_label$xmax - with_label$x), 0)
})

test_that("the weight legend is hidden by default and the fill legend stays", {
  by_default <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw, fill = level_label), kernels_per_cob = 20) +
    scale_fill_cob_d("fusarium")
  expect_null(get_guide_data(by_default, "thousand_kernel_weight"))
  expect_equal(nrow(get_guide_data(by_default, "fill")), 2)
})

test_that("the weight legend appears on request", {
  with_guides <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20) +
    guides(thousand_kernel_weight = "legend")
  with_scale <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20) +
    scale_thousand_kernel_weight_continuous(guide = "legend")
  expect_false(is.null(get_guide_data(with_guides, "thousand_kernel_weight")))
  expect_false(is.null(get_guide_data(with_scale, "thousand_kernel_weight")))
  expect_s3_class(ggplotGrob(with_guides), "gtable")
})

test_that("the label works with coord_flip and facets", {
  plot_object <- ggplot(rbind(transform(weight_trial, site = "A"), transform(weight_trial, site = "B")), aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), kernels_per_cob = 20, weight_label = TRUE) +
    facet_wrap(vars(site)) +
    coord_flip()
  expect_s3_class(ggplotGrob(plot_object), "gtable")
})

label_text <- function(cob_plot) {
  cob_children <- layer_grob(cob_plot)[[1]]$children
  cob_children[[length(cob_children)]]$label
}

test_that("yield_label writes the value of x with its unit", {
  cob_plot <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(yield_label = TRUE, kernels_per_cob = 20)
  expect_equal(label_text(cob_plot), c("9 t/ha", "10 t/ha"))
})

test_that("yield and weight stack on two lines", {
  cob_plot <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(
      aes(thousand_kernel_weight = tkw),
      yield_label = TRUE, weight_label = TRUE, kernels_per_cob = 20
    )
  expect_equal(label_text(cob_plot), c("9 t/ha\n328.5 g", "10 t/ha\n372 g"))
  expect_equal(n_grobs(cob_plot), n_grobs(ggplot(weight_trial, aes(r, h)) + geom_cob(kernels_per_cob = 20)) + 1)
})

test_that("yield_label shows only yield when the weight label is off", {
  cob_plot <- ggplot(weight_trial, aes(r, h)) +
    geom_cob(aes(thousand_kernel_weight = tkw), yield_label = TRUE, kernels_per_cob = 20)
  expect_equal(label_text(cob_plot), c("9 t/ha", "10 t/ha"))
})

test_that("yield_unit and decimal_mark apply to the labels", {
  decimal_trial <- data.frame(h = c("a", "b"), r = c(9.5, 10.2), tkw = c(328.5, 372))
  cob_plot <- ggplot(decimal_trial, aes(r, h)) +
    geom_cob(
      aes(thousand_kernel_weight = tkw),
      yield_label = TRUE, weight_label = TRUE, yield_unit = "bu/ac", decimal_mark = ",",
      kernels_per_cob = 20
    )
  expect_equal(label_text(cob_plot), c("9,5 bu/ac\n328,5 g", "10,2 bu/ac\n372 g"))
})

test_that("yield_label is off by default and keeps room when on", {
  default_data <- layer_data(ggplot(weight_trial, aes(r, h)) + geom_cob())
  labelled_data <- layer_data(ggplot(weight_trial, aes(r, h)) + geom_cob(yield_label = TRUE))
  expect_equal(default_data$xmax, default_data$x)
  expect_gt(min(labelled_data$xmax - labelled_data$x), 0)
})

test_that("a longer label reserves more room", {
  short_margin <- ggcorn:::right_margin_fraction(c(9, 10), TRUE, FALSE, "t", ".")
  long_margin <- ggcorn:::right_margin_fraction(c(9, 10), TRUE, FALSE, "tonnes/ha", ".")
  expect_gt(long_margin, short_margin)
})

test_that("yield labels work with coord_flip, facets and missing weights", {
  two_sites <- rbind(transform(weight_trial, site = "A"), transform(weight_trial, site = "B"))
  flipped_plot <- ggplot(two_sites, aes(r, h)) +
    geom_cob(
      aes(thousand_kernel_weight = tkw),
      yield_label = TRUE, weight_label = TRUE, kernels_per_cob = 20
    ) +
    facet_wrap(vars(site)) +
    coord_flip()
  expect_s3_class(ggplotGrob(flipped_plot), "gtable")
})
