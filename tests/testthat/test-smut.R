library(ggplot2)

create_smut_cob <- function(smut, x = 12) {
  tibble::tibble(
    cob_id = 1, x = x, y = 1, half_thickness = 0.3, fill = "#F7A91B",
    kernel_surface_share = 1, unpollinated = 0, smut = smut,
    western_bean_cutworm = 0, cutworm_zone = "tip"
  )
}

cob_blisters <- function(smut, x = 12) {
  cob <- create_smut_cob(smut, x)
  ggcorn:::compute_blisters(cob, ggcorn:::compute_kernels(cob, x / 90), 0.12 * x)
}

covered_kernel_share <- function(smut) {
  cob <- create_smut_cob(smut)
  kernels <- ggcorn:::compute_kernels(cob, 12 / 90)
  centers <- dplyr::distinct(kernels, kernel_id, x_grain, y_grain)
  blisters <- ggcorn:::compute_blisters(cob, kernels, 0.12 * 12)
  covered <- rep(FALSE, nrow(centers))
  for (rank_value in seq_len(nrow(blisters))) {
    covered <- covered |
      ((centers$x_grain - blisters$center_x[rank_value]) / blisters$radius_x[rank_value])^2 +
        ((centers$y_grain - blisters$center_y[rank_value]) / blisters$radius_y[rank_value])^2 < 1
  }
  mean(covered) * 100
}

test_that("the share of covered kernels follows severity", {
  for (severity in c(5, 15, 30, 60, 85)) {
    expect_lt(abs(covered_kernel_share(severity) - severity), 3)
  }
  expect_gt(covered_kernel_share(100), 95)
})

test_that("no gall is drawn without smut", {
  expect_null(ggcorn:::choose_blisters_for_cob(create_smut_cob(0), NULL, 1.44))
  trial <- data.frame(h = c("a", "b"), r = 10, severity = c(0, 40))
  without_value <- ggplot(trial, aes(r, h)) + geom_cob(kernels_per_cob = 20)
  with_value <- ggplot(trial, aes(r, h)) + geom_cob(aes(smut = severity), kernels_per_cob = 20)
  expect_equal(
    length(layer_grob(with_value)[[1]]$children),
    length(layer_grob(without_value)[[1]]$children) + 1
  )
})

test_that("galls never go past the cob's length", {
  for (severity in c(5, 30, 60, 100)) {
    outlines <- ggcorn:::blister_outlines(cob_blisters(severity))
    expect_lte(max(outlines$x_contour), 12)
  }
})

test_that("galls swell the outline without reaching the next cob", {
  light_outlines <- ggcorn:::blister_outlines(cob_blisters(5))
  severe_outlines <- ggcorn:::blister_outlines(cob_blisters(100))
  overhang <- function(outlines) max(abs(outlines$y_contour - 1)) / 0.3
  expect_gt(overhang(severe_outlines), overhang(light_outlines))
  expect_gt(overhang(severe_outlines), 1.2)
  expect_lte(overhang(severe_outlines), 1.55)
})

test_that("galls grow and darken with severity", {
  light <- cob_blisters(10)
  severe <- cob_blisters(80)
  expect_gt(nrow(severe), nrow(light))
  expect_gt(mean(severe$radius_y), mean(light$radius_y))
  expect_gt(mean(severe$ruptured_blister), mean(light$ruptured_blister))
})

test_that("the drawing is stable and does not depend on the unit", {
  state_before <- { set.seed(1); .Random.seed }
  first_draw <- cob_blisters(40)
  expect_identical(state_before, .Random.seed)
  expect_equal(first_draw, cob_blisters(40))
  expect_equal(nrow(first_draw), nrow(cob_blisters(40, x = 10000)))
})

test_that("an impossible severity gives a clear error", {
  trial <- data.frame(h = "a", r = 10, severity = 120)
  expect_error(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(smut = severity))),
    "between 0 and 100"
  )
})

test_that("a missing percentage removes the cob and warns", {
  trial <- data.frame(h = c("a", "b"), r = 10, severity = c(10, NA))
  expect_warning(
    ggplotGrob(ggplot(trial, aes(r, h)) + geom_cob(aes(smut = severity), kernels_per_cob = 20)),
    "missing"
  )
})

test_that("smut works with other aesthetics and the palette", {
  trial <- data.frame(h = c("a", "b"), r = c(9, 10), severity = c(10, 70), np = c(0, 20), tkw = c(300, 360))
  plot_object <- ggplot(trial, aes(r, h)) +
    geom_cob(
      aes(smut = severity, unpollinated = np, thousand_kernel_weight = tkw, fill = severity),
      kernels_per_cob = 30
    ) +
    scale_fill_cob_c("smut", limits = c(0, 100)) +
    coord_flip()
  expect_s3_class(ggplotGrob(plot_object), "gtable")
})
