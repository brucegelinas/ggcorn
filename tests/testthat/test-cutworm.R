library(ggplot2)

cutworm_kernels <- function(western_bean_cutworm, zone = "tip", x = 12) {
  cob <- tibble::tibble(
    cob_id = 1, x = x, y = 1, half_thickness = 0.3, fill = "#F7A91B",
    kernel_surface_share = 1, unpollinated = 0, western_bean_cutworm = western_bean_cutworm, cutworm_zone = zone
  )
  ggcorn:::compute_kernels(cob, x / 90) |>
    dplyr::distinct(kernel_id, .keep_all = TRUE)
}

test_that("the share of attacked kernels is exact", {
  for (zone in c("tip", "middle", "base", "tip+base+middle")) {
    for (severity in c(5, 20, 50, 90)) {
      kernels <- cutworm_kernels(severity, zone)
      expect_lte(abs(mean(kernels$attacked_kernel) * 100 - severity), 100 / nrow(kernels))
    }
  }
  expect_false(any(cutworm_kernels(0)$attacked_kernel))
  expect_true(all(cutworm_kernels(100)$attacked_kernel))
})

test_that("damage sits in the requested zone", {
  mean_position <- function(zone) {
    kernels <- cutworm_kernels(15, zone)
    mean(kernels$progression[kernels$attacked_kernel])
  }
  expect_gt(mean_position("tip"), 0.8)
  expect_gt(mean_position("middle"), 0.4)
  expect_lt(mean_position("middle"), 0.6)
  expect_lt(mean_position("base"), 0.25)
})

test_that("a combination reaches every zone", {
  kernels <- cutworm_kernels(30, "tip+base+middle")
  attacked <- kernels$progression[kernels$attacked_kernel]
  expect_true(any(attacked > 0.8))
  expect_true(any(attacked > 0.4 & attacked < 0.6))
  expect_true(any(attacked < 0.25))
})

test_that("zones can be written in several ways", {
  expect_equal(ggcorn:::parse_cutworm_zones("tip"), "tip")
  expect_equal(ggcorn:::parse_cutworm_zones("Tip + Base"), c("tip", "base"))
  expect_equal(ggcorn:::parse_cutworm_zones("tip,middle"), c("tip", "middle"))
  expect_error(ggcorn:::parse_cutworm_zones("top"), "tip")
})

test_that("some kernels are hollowed and the others punctured", {
  kernels <- cutworm_kernels(50)
  attacked <- kernels[kernels$attacked_kernel, ]
  expect_gt(mean(attacked$hollowed_kernel), 0.2)
  expect_lt(mean(attacked$hollowed_kernel), 0.6)
  expect_false(any(kernels$hollowed_kernel[!kernels$attacked_kernel]))
})

test_that("crumbs stay on the cob and the drawing is stable", {
  kernels <- dplyr::filter(cutworm_kernels(40), attacked_kernel)
  state_before <- { set.seed(2); .Random.seed }
  outlines <- ggcorn:::cutworm_outlines(kernels, 0.12 * 12)
  expect_identical(state_before, .Random.seed)
  expect_equal(outlines, ggcorn:::cutworm_outlines(kernels, 0.12 * 12))
  expect_lte(max(outlines$x_contour), 12.02)
  expect_lte(max(abs(outlines$y_contour - 1)), 0.3 * 1.25)
})

test_that("the zone can be fixed or come from a column", {
  trial <- data.frame(h = c("a", "b"), r = 10, damage = c(10, 40), zone = c("tip", "base+middle"))
  fixed_value <- ggplot(trial, aes(r, h)) +
    geom_cob(aes(western_bean_cutworm = damage), cutworm_zone = "middle", kernels_per_cob = 30)
  from_column <- ggplot(trial, aes(r, h)) +
    geom_cob(aes(western_bean_cutworm = damage, cutworm_zone = zone), kernels_per_cob = 30)
  expect_s3_class(ggplotGrob(fixed_value), "gtable")
  expect_s3_class(ggplotGrob(from_column), "gtable")
  expect_equal(unique(layer_data(fixed_value)$cutworm_zone), "middle")
  expect_setequal(layer_data(from_column)$cutworm_zone, c("tip", "base+middle"))
})

test_that("no cutworm drawing is added without damage", {
  trial <- data.frame(h = c("a", "b"), r = 10, damage = c(0, 30))
  without_value <- ggplot(trial, aes(r, h)) + geom_cob(kernels_per_cob = 20)
  with_value <- ggplot(trial, aes(r, h)) + geom_cob(aes(western_bean_cutworm = damage), kernels_per_cob = 20)
  expect_equal(
    length(layer_grob(with_value)[[1]]$children),
    length(layer_grob(without_value)[[1]]$children) + 2
  )
})

test_that("impossible values give a clear error", {
  trial <- data.frame(h = "a", r = 10, damage = 120, zone = "tip")
  expect_error(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(western_bean_cutworm = damage))),
    "between 0 and 100"
  )
  trial$damage <- 10
  trial$zone <- "top"
  expect_error(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(western_bean_cutworm = damage, cutworm_zone = zone))),
    "cutworm_zone"
  )
})

test_that("cutworm damage combines with other aesthetics", {
  trial <- data.frame(
    h = c("a", "b"), r = c(9, 10), damage = c(10, 30), severity = c(5, 20),
    np = c(0, 10), tkw = c(300, 360), level_label = factor(c(2, 3))
  )
  plot_object <- ggplot(trial, aes(r, h)) +
    geom_cob(
      aes(
        western_bean_cutworm = damage, smut = severity, unpollinated = np,
        thousand_kernel_weight = tkw, fill = level_label
      ),
      kernels_per_cob = 30
    ) +
    scale_fill_cob_d("fusarium") +
    facet_wrap(vars(h))
  expect_s3_class(ggplotGrob(plot_object), "gtable")
})

test_that("damage stays pale", {
  luminance <- function(colors) mean(colSums(col2rgb(colors) * c(0.299, 0.587, 0.114)))
  kernels <- dplyr::filter(cutworm_kernels(40), attacked_kernel)
  outlines <- ggcorn:::cutworm_outlines(kernels, 0.12 * 12)
  expect_gt(luminance(ggcorn:::attacked_kernel_color("#F7A91B", FALSE)), 170)
  expect_gt(luminance(ggcorn:::attacked_kernel_color("#F7A91B", TRUE)), 140)
  expect_gt(luminance(ggcorn:::damage_backing_outlines(kernels)$polygon_color), 150)
  expect_gt(luminance(outlines$polygon_color), 170)
})
