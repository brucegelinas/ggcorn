library(ggplot2)

symptom_trial <- data.frame(h = c("a", "b"), r = c(10, 9), severity = c(10, 60))

severity_plot <- function(...) {
  ggplot(symptom_trial, aes(r, h)) +
    geom_cob(aes(...), kernels_per_cob = 20)
}

test_that("each symptom has a legend by default, with fixed breaks", {
  plots <- list(
    smut = severity_plot(smut = severity),
    western_bean_cutworm = severity_plot(western_bean_cutworm = severity),
    unpollinated = severity_plot(unpollinated = severity)
  )
  for (symptom in names(plots)) {
    guide <- get_guide_data(plots[[symptom]], symptom)
    expect_equal(guide[[symptom]], c(0, 25, 50, 75, 100))
    expect_equal(guide$.label, c("0%", "25%", "50%", "75%", "100%"))
    expect_s3_class(ggplotGrob(plots[[symptom]]), "gtable")
  }
})

test_that("breaks stay fixed whatever the data range", {
  low_severity <- data.frame(h = c("a", "b"), r = 10, severity = c(1, 3))
  cob_plot <- ggplot(low_severity, aes(r, h)) + geom_cob(aes(smut = severity))
  expect_equal(get_guide_data(cob_plot, "smut")$smut, c(0, 25, 50, 75, 100))
})

test_that("the title, breaks and labels can be changed", {
  cob_plot <- severity_plot(smut = severity) +
    scale_smut_continuous(name = "Smut", breaks = c(0, 50), labels = c("none", "half"))
  guide <- get_guide_data(cob_plot, "smut")
  expect_equal(guide$smut, c(0, 50))
  expect_equal(guide$.label, c("none", "half"))
})

test_that("guides() hides one or several legends", {
  expect_null(get_guide_data(severity_plot(smut = severity) + guides(smut = "none"), "smut"))
  both_symptoms <- severity_plot(smut = severity, unpollinated = severity) +
    guides(smut = "none", unpollinated = "none")
  expect_null(get_guide_data(both_symptoms, "smut"))
  expect_null(get_guide_data(both_symptoms, "unpollinated"))
})

test_that("the scale's guide argument hides a legend", {
  expect_null(get_guide_data(
    severity_plot(western_bean_cutworm = severity) +
      scale_western_bean_cutworm_continuous(guide = "none"),
    "western_bean_cutworm"
  ))
  expect_null(get_guide_data(
    severity_plot(unpollinated = severity) + scale_unpollinated_continuous(guide = "none"),
    "unpollinated"
  ))
})

test_that("show.legend = FALSE hides the legend", {
  cob_plot <- ggplot(symptom_trial, aes(r, h)) +
    geom_cob(aes(smut = severity), show.legend = FALSE)
  expect_null(get_guide_data(cob_plot, "smut"))
})

test_that("legend.position = 'none' removes the legend from the plot", {
  guide_box_widths <- function(cob_plot) {
    cob_table <- ggplotGrob(cob_plot)
    sum(grepl("guide-box", cob_table$layout$name) &
      !vapply(cob_table$grobs, inherits, logical(1), "zeroGrob"))
  }
  shown <- severity_plot(smut = severity)
  hidden <- shown + theme(legend.position = "none")
  expect_gt(guide_box_widths(shown), guide_box_widths(hidden))
})

test_that("symptom keys are mini cobs, including at 0%", {
  key_data <- function(...) {
    data.frame(
      fill = "#F7A91B", thousand_kernel_weight = Inf, unpollinated = -1, smut = -1,
      western_bean_cutworm = -1, cutworm_zone = "tip"
    ) |>
      transform(...)
  }
  zero_key <- ggcorn:::draw_key_cob(key_data(smut = 0), list(), 1)
  full_key <- ggcorn:::draw_key_cob(key_data(smut = 100), list(), 1)
  expect_s3_class(zero_key, "gTree")
  expect_equal(attr(zero_key, "width"), 2.4)
  expect_gt(length(full_key$children), length(zero_key$children))

  cutworm_key <- ggcorn:::draw_key_cob(key_data(western_bean_cutworm = 50), list(), 1)
  expect_gt(length(cutworm_key$children), length(zero_key$children))

  wide_key <- ggcorn:::draw_key_cob(key_data(unpollinated = 50), list(legend_cob_width = 3), 1)
  expect_equal(attr(wide_key, "width"), 3)
})

test_that("keys without symptoms stay kernels", {
  kernel_key <- ggcorn:::draw_key_cob(
    data.frame(fill = "#F7A91B", thousand_kernel_weight = Inf, unpollinated = -1, smut = -1,
               western_bean_cutworm = -1),
    list(), 1
  )
  expect_s3_class(kernel_key, "polygon")
})

test_that("unmapped symptoms draw nothing on the cobs", {
  plain_cob <- ggplot(symptom_trial, aes(r, h)) + geom_cob(kernels_per_cob = 20)
  layer_values <- layer_data(plain_cob)
  expect_true(all(layer_values$smut < 0))
  expect_equal(length(layer_grob(plain_cob)[[1]]$children), 3)
})

test_that("a variable used for two symptoms draws one working legend", {
  cob_plot <- severity_plot(smut = severity, western_bean_cutworm = severity)
  expect_equal(nrow(get_guide_data(cob_plot, "smut")), 5)
  expect_s3_class(ggplotGrob(cob_plot), "gtable")
})

test_that("a fixed cutworm zone reaches the legend keys", {
  cob_plot <- ggplot(symptom_trial, aes(r, h)) +
    geom_cob(aes(western_bean_cutworm = severity), cutworm_zone = "middle", kernels_per_cob = 20)
  expect_s3_class(ggplotGrob(cob_plot), "gtable")
})

test_that("limits narrow each severity scale and breaks follow", {
  narrow_trial <- data.frame(h = c("a", "b"), r = 10, severity = c(5, 15))
  narrow_plot <- function(...) {
    ggplot(narrow_trial, aes(r, h)) + geom_cob(aes(...), kernels_per_cob = 20)
  }
  plots <- list(
    smut = narrow_plot(smut = severity) + scale_smut_continuous(limits = c(0, 20)),
    western_bean_cutworm = narrow_plot(western_bean_cutworm = severity) +
      scale_western_bean_cutworm_continuous(limits = c(0, 20)),
    unpollinated = narrow_plot(unpollinated = severity) +
      scale_unpollinated_continuous(limits = c(0, 20))
  )
  for (symptom in names(plots)) {
    expect_equal(get_guide_data(plots[[symptom]], symptom)[[symptom]], c(0, 5, 10, 15, 20))
    expect_s3_class(ggplotGrob(plots[[symptom]]), "gtable")
  }
})

test_that("explicit breaks still work with limits", {
  cob_plot <- severity_plot(smut = severity) +
    scale_smut_continuous(limits = c(0, 60), breaks = c(0, 60))
  expect_equal(get_guide_data(cob_plot, "smut")$smut, c(0, 60))
})

test_that("a value outside limits removes the cob with a warning", {
  cob_plot <- severity_plot(smut = severity) + scale_smut_continuous(limits = c(0, 20))
  expect_warning(ggplotGrob(cob_plot), "missing|non-finite|outside")
})

test_that("limits don't change the drawn severity", {
  default_values <- layer_data(severity_plot(smut = severity))$smut
  wide_values <- layer_data(severity_plot(smut = severity) + scale_smut_continuous(limits = c(0, 80)))$smut
  expect_equal(default_values, wide_values)
})
