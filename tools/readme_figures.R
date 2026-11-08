# Regenerates the README figures. All values are fictional.

library(ggplot2)
library(ggcorn)

text_color <- "black"
background_color <- "#FBF7EC"

rotation_levels <- c("Monoculture", "Corn / soybean", "Corn / soybean / cereal", "+ clover")
fictional_yields <- data.frame(
  rotation = factor(rotation_levels, levels = rotation_levels),
  grain_yield = c(9.1, 10.2, 10.9, 11.6)
)

yield_figure <- ggplot(fictional_yields, aes(grain_yield, rotation)) +
  geom_cob() +
  labs(title = "Yield by rotation (fictional data)", x = "t/ha", y = NULL) +
  theme_corn()

ggsave(
  "man/figures/README-yield.png", yield_figure,
  width = 10, height = 5, dpi = 150, device = ragg::agg_png
)

fictional_components <- data.frame(
  trial = factor(
    c("Control", "Moderate stress", "Stress at flowering"),
    levels = c("Stress at flowering", "Moderate stress", "Control")
  ),
  potential_yield = 11,
  tkw = c(400, 330, 290),
  unpollinated_kernels = c(0, 12, 32)
)

components_figure <- ggplot(fictional_components, aes(potential_yield, trial)) +
  geom_cob(aes(thousand_kernel_weight = tkw, unpollinated = unpollinated_kernels), show.legend = F, weight_label = T, weight_unit = "g / 1000 seeds") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.32))) +
  labs(title = "Thousand-kernel weight and unpollinated kernels (fictional data)", x = NULL, y = NULL) +
theme_corn()

ggsave(
  "man/figures/README-components.png", components_figure,
  width = 10, height = 4, dpi = 150, device = ragg::agg_png
)

smut_levels <- c(0, 5, 15, 30, 60, 100)
smut_severities <- data.frame(
  level = factor(paste0(smut_levels, "%"), levels = paste0(smut_levels, "%")),
  grain_yield = 11,
  severity = smut_levels
)

smut_figure <- ggplot(smut_severities, aes(grain_yield, level)) +
  geom_cob(aes(smut = severity)) +
  scale_smut_continuous(guide = "none") +
  labs(
     title = "Common smut: severity from 0 to 100% (fictional values)", 
     x = NULL, 
     y = NULL,
     smut = "Smut levels"
     ) +
  theme_corn()

ggsave(
  "man/figures/README-smut.png", smut_figure,
  width = 9, height = 5, dpi = 150, device = ragg::agg_png
)

symptom_trial <- data.frame(
  hybrid = factor(c("A", "B", "C", "D")),
  grain_yield = c(11.5, 10.8, 9.9, 8.7),
  severity = c(0, 12, 35, 70)
)

legend_figure <- ggplot(symptom_trial, aes(grain_yield, hybrid)) +
  geom_cob(aes(smut = severity)) +
  scale_smut_continuous(name = "Smut levels",
                        breaks = c(0, 15, 50)) +
  labs(
     title = "Mini cobs in the legend (fictional values)", 
     x = "t/ha", 
     y = NULL
     ) +
theme_corn()

ggsave(
  "man/figures/README-legend.png", legend_figure,
  width = 10, height = 5, dpi = 150, device = ragg::agg_png
)

cutworm_labels <- c("Tip, 8%", "Tip, 40%", "Middle, 12%", "Base, 12%", "Tip+base+middle, 45%")
cutworm_cases <- data.frame(
  case = factor(cutworm_labels, levels = rev(cutworm_labels)),
  grain_yield = 11,
  damage = c(8, 40, 12, 12, 45),
  zone = c("tip", "tip", "middle", "base", "tip+base+middle")
)

cutworm_figure <- ggplot(cutworm_cases, aes(grain_yield, case)) +
  geom_cob(aes(western_bean_cutworm = damage, cutworm_zone = zone), show.legend = F) +
  labs(title = "Western bean cutworm damages (fictional values)", x = NULL, y = NULL) +
   theme_corn(x_values = F)

ggsave(
  "man/figures/README-cutworm.png", cutworm_figure,
  width = 8, height = 6, dpi = 150, device = ragg::agg_png
)

severity_levels <- data.frame(
  severity = c(0, 25, 50, 75, 100),
  level = factor(paste0(c(0, 25, 50, 75, 100), "%"), levels = paste0(c(0, 25, 50, 75, 100), "%")),
  cob_length = 1
)

draw_palette <- function(palette_name) {
  ggplot(severity_levels, aes(cob_length, level, fill = severity)) +
    geom_cob(kernels_per_cob = 42, thickness = 0.66) +
    scale_fill_cob_c(palette_name) +
    labs(title = palette_name, x = NULL, y = NULL) +
    theme_corn(x_values = F)
}

palette_sheet <- gridExtra::arrangeGrob(
  grobs = lapply(lapply(names(palettes_cob), draw_palette), ggplotGrob),
  ncol = 3
)
ragg::agg_png(
  "man/figures/README-palettes.png",
  width = 12, height = 11.4, units = "in", res = 150, background = background_color
)
grid::grid.draw(palette_sheet)
invisible(dev.off())
