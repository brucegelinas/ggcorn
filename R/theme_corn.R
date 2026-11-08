#' A theme for cob plots
#'
#' `theme_corn()` starts from [ggplot2::theme_minimal()] and gives cob plots a bold serif
#' title aligned with the left edge of the plot and bold category labels. Add it like any
#' other theme, and override any part with [ggplot2::theme()] afterwards.
#'
#' By default, the `x` axis shows its values. Use `x_values = FALSE` to hide them. With
#' [ggplot2::coord_flip()], categories move to the `x` axis, so `x_values = FALSE` hides
#' them.
#'
#' @param base_size Base font size. The title and the category labels scale with it.
#'   Default: `14`.
#' @param base_family Font family of all text except the title. Default: `""`, the
#'   device's default font.
#' @param title_family Font family of the title. Default: `"Georgia"`, which must be
#'   installed on the computer. Otherwise choose another family, such as `"serif"`.
#' @param x_values `TRUE` shows the values on the `x` axis and `FALSE` hides them. Any
#'   other value gives an error. Default: `TRUE`.
#' @return A `ggplot2` theme.
#' @examples
#' library(ggplot2)
#'
#' yields <- data.frame(
#'   rotation = c("Monoculture", "Corn / soybean", "Corn / soybean / cereal"),
#'   grain_yield = c(100, 110, 120)
#' )
#'
#' ggplot(yields, aes(grain_yield, rotation)) +
#'   geom_cob(kernels_per_cob = 50) +
#'   labs(
#'     title = "Diversifying the rotation increases yield",
#'     x = "Corn yield",
#'     y = NULL
#'   ) +
#'   theme_corn(title_family = "serif")
#'
#' # hide the yield values
#' ggplot(yields, aes(grain_yield, rotation)) +
#'   geom_cob(kernels_per_cob = 50) +
#'   theme_corn(title_family = "serif", x_values = FALSE)
#' @export
theme_corn <- function(base_size = 14, base_family = "", title_family = "Georgia",
                       x_values = TRUE) {
  if (!rlang::is_bool(x_values)) {
    rlang::abort("`x_values` must be TRUE or FALSE.")
  }
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text.y = element_text(face = "bold", size = base_size * 16 / 14),
      axis.title.y = element_blank(),
      axis.text.x = if (x_values) element_text() else element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold", family = title_family, size = base_size * 18 / 14
      )
    )
}
