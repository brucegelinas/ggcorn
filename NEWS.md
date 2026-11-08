# ggcorn 0.13.1

* `scale_smut_continuous()`, `scale_western_bean_cutworm_continuous()` and
  `scale_unpollinated_continuous()` now accept `limits`, which gave an error before. Breaks
  follow the limits by default.

# ggcorn 0.13.0

* `smut`, `western_bean_cutworm` and `unpollinated` now have legends, shown by default. Each
  key is a mini cob without husks, with fixed breaks of 0, 25, 50, 75 and 100%.
* New `scale_smut_continuous()`, `scale_western_bean_cutworm_continuous()` and
  `scale_unpollinated_continuous()`, and a `legend_cob_width` argument in `geom_cob()`.
* Hide these legends with `guides(smut = "none")`, for example. The help of `geom_cob()` and
  `scale_smut_continuous()` shows every way to do it.

# ggcorn 0.12.0

* `weight_label` is now `FALSE` by default, so `thousand_kernel_weight` writes nothing at the
  tip unless asked.
* The kernel size legend is now hidden by default. Show it with
  `guides(thousand_kernel_weight = "legend")`.
* `theme_corn()` now shows the x axis values by default (`x_values = TRUE`), and gives an error
  if `x_values` isn't `TRUE` or `FALSE`.
* Every help page now states the default value of each argument.

# ggcorn 0.11.0

* New `yield_label`, `yield_unit` and `decimal_mark` arguments in `geom_cob()`. With a
  thousand-kernel weight label, the two values stack on two lines.
* New `x_values` argument in `theme_corn()`.

# ggcorn 0.10.0

* New `theme_corn()`: bold Georgia title, bold category labels and no yield axis numbers.

# ggcorn 0.9.1

* New `show_cob_palettes()` draws the palettes as color strips.
* The `palettes_cob` help page now shows the palettes as an image, and its examples draw them.

# ggcorn 0.9.0

* The package is now in English and renamed from `ggmais` to `ggcorn`, with its functions,
  arguments, aesthetics, palettes, help pages and messages translated.
* `geom_epi()` is now `geom_cob()`, and `scale_fill_epi_c()` and `scale_fill_epi_d()` are now
  `scale_fill_cob_c()` and `scale_fill_cob_d()`.
* Weight labels now use a decimal point.

# ggmais 0.8.0

* With a disease palette, `scale_fill_cob_c()` now uses a fixed 0 to 100 scale, so a low
  severity stays yellow. `limits` sets another scale, and `limits = NULL` restores the former
  behavior.
* `scale_fill_cob_d()` accepts `levels` to anchor the colors of a rating scale.

# ggmais 0.7.1

* Western bean cutworm damage is paler, grooves included.

# ggmais 0.7.0

* New `western_bean_cutworm` and `cutworm_zone` aesthetics: attacked kernels are punctured or
  hollowed, with flour and frass, at the tip, middle, base or any combination.

# ggmais 0.6.0

* New `smut` aesthetic: gall clusters cover kernels, swell the cob's outline and darken with
  severity.
* New `smut` palette.

# ggmais 0.5.0

* `thousand_kernel_weight` is now written at the end of each cob, and the axis keeps room for
  it. The weight legend no longer appears by default.
* New `weight_label`, `weight_unit`, `label_size` and `label_family` arguments.

# ggmais 0.4.0

* `thousand_kernel_weight` now has a legend showing one kernel per break.
* New `scale_thousand_kernel_weight_continuous()` and `legend_kernel_size`.
* `fill` legends also show a kernel.

# ggmais 0.3.0

* The cob now has a realistic shape: it narrows gradually toward the tip, its diameter varies
  along its length, and kernels shrink toward the end.

# ggmais 0.2.0

* New aesthetics: `thousand_kernel_weight` sets kernel area and `unpollinated` shows the
  percentage of unpollinated kernels at the tip.
* New `weight_amplification` argument.

# ggmais 0.1.0

* First version: the cob layer, its two fill scales and the palettes.
