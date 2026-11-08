library(ggplot2)

trial <- data.frame(hybrid = c("A", "B", "C"), grain_yield = c(11, 10, 9), severity = c(0, 50, 100))

drawn_colors <- function(plot_object) layer_data(plot_object)$fill

test_that("the continuous scale runs the palette from healthy to affected", {
  plot_object <- ggplot(trial, aes(grain_yield, hybrid, fill = severity)) +
    geom_cob() +
    scale_fill_cob_c("disease")
  end_colors <- toupper(drawn_colors(plot_object)[c(1, 3)])
  expect_equal(end_colors, c("#F7C93E", "#6B4213"))
})

test_that("direction = -1 reverses the palette", {
  plot_object <- ggplot(trial, aes(grain_yield, hybrid, fill = severity)) +
    geom_cob() +
    scale_fill_cob_c("disease", direction = -1)
  expect_equal(toupper(drawn_colors(plot_object)[1]), "#6B4213")
})

test_that("the discrete scale works with classes", {
  plot_object <- ggplot(trial, aes(grain_yield, hybrid, fill = hybrid)) +
    geom_cob() +
    scale_fill_cob_d("gibberella")
  expect_equal(toupper(drawn_colors(plot_object)[c(1, 3)]), c("#F7C93E", "#B5303F"))
})

test_that("a custom palette is accepted", {
  plot_object <- ggplot(trial, aes(grain_yield, hybrid, fill = severity)) +
    geom_cob() +
    scale_fill_cob_c(c("#FFFFFF", "#000000"))
  expect_equal(toupper(drawn_colors(plot_object)[3]), "#000000")
})

test_that("an unknown palette name gives a clear error", {
  expect_error(scale_fill_cob_c("rouille"), "maturity")
})

colors_for <- function(values_used, scale_used) {
  trial <- data.frame(h = letters[seq_along(values_used)], r = 10, value = values_used)
  layer_data(ggplot(trial, aes(r, h, fill = value)) + geom_cob() + scale_used)$fill
}

color_gap <- function(color_a, color_b) {
  max(abs(col2rgb(color_a) - col2rgb(color_b)))
}

test_that("low severity stays close to yellow", {
  colors <- colors_for(c(2, 3, 4), scale_fill_cob_c("fusarium"))
  expect_lt(color_gap(colors[3], "#F7C93E"), 40)
  stretched_colors <- colors_for(c(2, 3, 4), scale_fill_cob_c("fusarium", limits = NULL))
  expect_equal(toupper(stretched_colors[3]), "#E8A79A")
})

test_that("another scale is set with limits", {
  colors <- colors_for(c(1, 9, 12), scale_fill_cob_c("fusarium", limits = c(1, 9)))
  expect_equal(toupper(colors[1]), "#F7C93E")
  expect_equal(toupper(colors[2]), "#E8A79A")
  expect_equal(toupper(colors[3]), "#E8A79A")
})

test_that("values outside 0 to 100 follow the data range", {
  colors <- colors_for(c(8000, 11000), scale_fill_cob_c("disease"))
  expect_equal(toupper(colors), c("#F7C93E", "#6B4213"))
  reversed <- colors_for(c(20, 80), scale_fill_cob_c("disease", direction = -1))
  expect_equal(toupper(reversed), c("#6B4213", "#F7C93E"))
})

test_that("the maturity palette always follows the data range", {
  colors <- colors_for(c(90, 95), scale_fill_cob_c())
  expect_equal(toupper(colors), c("#F0D68E", "#F7A91B"))
})

test_that("levels keeps a class's color whatever the data", {
  class_color <- function(class_labels) {
    trial <- data.frame(h = letters[seq_along(class_labels)], r = 10, class_label = factor(class_labels))
    plot_object <- ggplot(trial, aes(r, h, fill = class_label)) +
      geom_cob() +
      scale_fill_cob_d("fusarium", levels = 1:9)
    layer_values <- layer_data(plot_object)
    layer_values$fill[match("3", as.character(trial$class_label))]
  }
  expect_equal(class_color(3), class_color(c(2, 3, 4)))
  expect_equal(class_color(3), class_color(c(3, 8, 9)))
})

test_that("the legend shows only the classes present", {
  trial <- data.frame(h = c("a", "b"), r = 10, class_label = factor(c(2, 4)))
  plot_object <- ggplot(trial, aes(r, h, fill = class_label)) +
    geom_cob() +
    scale_fill_cob_d("fusarium", levels = 1:9)
  expect_equal(get_guide_data(plot_object, "fill")$.label, c("2", "4"))
})

test_that("show_cob_palettes draws every palette by default", {
  drawn_palettes <- show_cob_palettes()
  expect_s3_class(drawn_palettes, "ggplot")
  expect_s3_class(ggplotGrob(drawn_palettes), "gtable")
  expect_setequal(as.character(unique(drawn_palettes$data$palette_name)), names(palettes_cob))
})

test_that("show_cob_palettes matches the colors of the fill scale", {
  strip <- show_cob_palettes("disease")$data
  expect_equal(toupper(strip$strip_color[strip$strip_position == 0]), "#F7C93E")
  expect_equal(toupper(strip$strip_color[strip$strip_position == 100]), "#6B4213")
  scale_colors <- colors_for(c(0, 50, 100), scale_fill_cob_c("disease"))
  expect_equal(strip$strip_color[strip$strip_position == 50], scale_colors[2])
})

test_that("show_cob_palettes rejects an unknown palette", {
  expect_error(show_cob_palettes("rust"), "maturity")
})
