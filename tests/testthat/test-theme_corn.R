library(ggplot2)

test_that("theme_corn sets the title, labels and axis", {
  corn_theme <- theme_corn()
  plot_title <- calc_element("plot.title", corn_theme)
  category_labels <- calc_element("axis.text.y.left", corn_theme)
  expect_equal(plot_title$family, "Georgia")
  expect_equal(plot_title$face, "bold")
  expect_equal(plot_title$size, 18)
  expect_equal(corn_theme$plot.title.position, "plot")
  expect_equal(category_labels$face, "bold")
  expect_equal(category_labels$size, 16)
})

test_that("title and labels scale with base_size", {
  larger_theme <- theme_corn(base_size = 21)
  expect_equal(calc_element("plot.title", larger_theme)$size, 27)
  expect_equal(calc_element("axis.text.y.left", larger_theme)$size, 24)
})

test_that("title_family and base_family are applied", {
  custom_theme <- theme_corn(base_family = "sans", title_family = "serif")
  expect_equal(calc_element("plot.title", custom_theme)$family, "serif")
  expect_equal(calc_element("axis.text.y.left", custom_theme)$family, "sans")
})

test_that("theme_corn works on a plot and can be overridden", {
  yields <- data.frame(rotation = c("a", "b"), grain_yield = c(9, 10))
  cob_plot <- ggplot(yields, aes(grain_yield, rotation)) +
    geom_cob(kernels_per_cob = 20) +
    theme_corn(title_family = "serif", x_values = FALSE) +
    theme(axis.text.x = element_text())
  expect_s3_class(ggplotGrob(cob_plot), "gtable")
  expect_false(inherits(calc_element("axis.text.x.bottom", cob_plot$theme), "ggplot2::element_blank"))
})

test_that("x axis values are shown by default and can be hidden", {
  expect_false(inherits(calc_element("axis.text.x.bottom", theme_corn()), "ggplot2::element_blank"))
  expect_s3_class(calc_element("axis.text.x.bottom", theme_corn(x_values = FALSE)), "ggplot2::element_blank")
})

test_that("x_values must be TRUE or FALSE", {
  expect_error(theme_corn(x_values = 0), "TRUE or FALSE")
  expect_error(theme_corn(x_values = NA), "TRUE or FALSE")
  expect_error(theme_corn(x_values = "yes"), "TRUE or FALSE")
  expect_error(theme_corn(x_values = c(TRUE, FALSE)), "TRUE or FALSE")
})
