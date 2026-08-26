#!/usr/bin/env Rscript

# This script only redraws the published analysis outputs; it does not refit
# models or alter any estimates.

suppressPackageStartupMessages({
  library(ggplot2)
})

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
script_dir <- if (nzchar(script_path) && file.exists(script_path)) {
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  file.path(getwd(), "Submission_Code")
}
root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
out_dir <- file.path(
  root, "Additional_Analyses",
  "21_northern_hemisphere_subtype_pandemic_segmented_models"
)
read_output <- function(filename) {
  path <- file.path(out_dir, filename)
  if (!file.exists(path)) {
    stop("Required analysis output is missing: ", path, call. = FALSE)
  }
  utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
}

input_stem <- "northern"
annual <- read_output(paste0(input_stem, "_segmented_analysis_input.csv"))
curves <- read_output(paste0(input_stem, "_segmented_curve_predictions.csv"))
slopes <- read_output(paste0(input_stem, "_segmented_slope_results.csv"))

subtype_levels <- c("A/H1N1", "A/H3N2", "IBV")
annual$subtype <- factor(annual$subtype, levels = subtype_levels)
curves$subtype <- factor(curves$subtype, levels = subtype_levels)
annual$support <- factor(
  ifelse(annual$n_studies >= 2, "\u22652 studies", "1 study"),
  levels = c("\u22652 studies", "1 study")
)

format_p <- function(x) {
  ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
}
extract_slope <- function(estimand, column) {
  selected <- slopes[slopes$estimand == estimand, c("subtype", column)]
  values <- stats::setNames(selected[[column]], selected$subtype)
  unname(values[subtype_levels])
}
slope_summary <- data.frame(
  subtype = factor(subtype_levels, levels = subtype_levels),
  slope_before = extract_slope(
    "Slope before knot", "estimate_ve_pp_per_year"
  ),
  slope_after = extract_slope(
    "Slope after knot", "estimate_ve_pp_per_year"
  ),
  p_value_fdr_bh = extract_slope(
    "Slope change after minus before", "p_value_fdr_bh"
  )
)
p_annotation <- slope_summary[, c("subtype", "p_value_fdr_bh")]
p_annotation$annotation <- sprintf(
  "BH-adjusted P = %s", format_p(p_annotation$p_value_fdr_bh)
)
p_annotation$x <- 2010.65
p_annotation$y <- 94

slope_labels <- rbind(
  data.frame(
    subtype = factor(subtype_levels, levels = subtype_levels),
    x = 2016.0,
    slope = slope_summary$slope_before
  ),
  data.frame(
    subtype = factor(subtype_levels, levels = subtype_levels),
    x = 2022.0,
    slope = slope_summary$slope_after
  )
)
slope_labels$line_y <- mapply(function(subtype, x) {
  subtype_curve <- curves[as.character(curves$subtype) == subtype, ]
  stats::approx(
    subtype_curve$season_year,
    subtype_curve$fitted_ve_percent,
    xout = x,
    rule = 2
  )$y
}, as.character(slope_labels$subtype), slope_labels$x)
slope_labels$y <- slope_labels$line_y - 8
slope_labels$annotation <- sprintf(
  "%+.2f pp/year", slope_labels$slope
)
# Convert data-space slopes to display-space angles for this figure's panel
# aspect ratio so that each annotation runs parallel to its fitted segment.
slope_labels$angle <- atan(slope_labels$slope * 0.092) * 180 / pi

palette <- c(
  "A/H1N1" = "#008F7A",
  "A/H3N2" = "#3D5A91",
  "IBV" = "#A66A91"
)
pandemic_knot <- 2020.00
y_min <- -30
y_max <- 100

# Mark intervals that continue below the displayed range. The raw interval is
# still drawn and clipped; the arrow prevents the truncation from being hidden.
lower_clipped <- annual[annual$ve_ci_lower < y_min, , drop = FALSE]

panel_labels <- c(
  "A/H1N1" = "A(H1N1)pdm09",
  "A/H3N2" = "A(H3N2)",
  "IBV" = "IBV"
)
x_breaks <- seq(2010.5, 2024.5, by = 2)
x_labels <- sprintf("%d\u2013%02d", 2010:2024, (2011:2025) %% 100)
x_labels <- x_labels[seq(1, length(x_labels), by = 2)]

p <- ggplot(
  annual,
  aes(x = season_year, y = ve_percent, colour = subtype)
) +
  geom_hline(
    yintercept = seq(0, 100, by = 50),
    colour = "#E6E7E9", linewidth = 0.28
  ) +
  geom_vline(
    xintercept = pandemic_knot,
    colour = "#666A70", linewidth = 0.42, linetype = "22"
  ) +
  geom_ribbon(
    data = curves,
    aes(
      x = season_year, ymin = ci_lower_model, ymax = ci_upper_model,
      fill = subtype
    ),
    inherit.aes = FALSE, alpha = 0.14, colour = NA
  ) +
  geom_errorbar(
    aes(ymin = ve_ci_lower, ymax = ve_ci_upper),
    width = 0.10, linewidth = 0.32, alpha = 0.42
  ) +
  geom_line(
    data = curves,
    aes(x = season_year, y = fitted_ve_percent, colour = subtype),
    inherit.aes = FALSE, linewidth = 0.82, lineend = "round"
  ) +
  geom_point(
    aes(shape = support),
    size = 1.95, stroke = 0.62, fill = "white"
  ) +
  geom_segment(
    data = lower_clipped,
    aes(
      x = season_year, xend = season_year,
      y = y_min + 5, yend = y_min + 0.8, colour = subtype
    ),
    inherit.aes = FALSE, linewidth = 0.32,
    arrow = grid::arrow(
      length = grid::unit(1.35, "mm"), type = "closed"
    )
  ) +
  geom_text(
    data = p_annotation,
    aes(x = x, y = y, label = annotation),
    inherit.aes = FALSE, hjust = 0, vjust = 1,
    family = "Arial", colour = "#3D4045",
    size = 3.5
  ) +
  geom_label(
    data = slope_labels,
    aes(
      x = x, y = y, label = annotation,
      angle = angle
    ),
    inherit.aes = FALSE, hjust = 0.5, vjust = 1,
    family = "Arial", fontface = "plain", size = 3.25,
    colour = "#111111",
    fill = grDevices::adjustcolor("white", alpha.f = 0.82),
    linewidth = 0,
    label.padding = grid::unit(0.45, "mm"),
    label.r = grid::unit(0, "mm")
  ) +
  facet_wrap(
    ~ subtype, nrow = 1,
    labeller = as_labeller(panel_labels)
  ) +
  scale_colour_manual(values = palette, guide = "none") +
  scale_fill_manual(values = palette, guide = "none") +
  scale_shape_manual(
    values = c("\u22652 studies" = 16, "1 study" = 1),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = x_breaks, labels = x_labels,
    limits = c(2010.25, 2024.75),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = c(0, 50, 100),
    labels = c("0", "50", "100"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(y_min, y_max), clip = "on") +
  labs(
    x = "Northern Hemisphere influenza season",
    y = "Standardized VE (%)",
    shape = NULL
  ) +
  guides(
    shape = guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(
        colour = "#33363A", size = 2.3, stroke = 0.60
      )
    )
  ) +
  theme_classic(base_size = 10, base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", colour = "#292C31"),
    axis.line = element_line(colour = "#292C31", linewidth = 0.34),
    axis.ticks = element_line(colour = "#292C31", linewidth = 0.30),
    axis.ticks.length = grid::unit(1.4, "mm"),
    axis.text = element_text(colour = "#292C31", size = 10),
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1, margin = margin(t = 1.2)
    ),
    axis.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.title.y = element_text(margin = margin(r = 4)),
    strip.background = element_blank(),
    strip.text = element_text(
      hjust = 0.5, face = "bold", size = 11,
      colour = "#292C31", margin = margin(b = 2.5)
    ),
    panel.spacing.x = grid::unit(4.0, "mm"),
    legend.position = "top",
    legend.justification = "right",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.key.width = grid::unit(4.6, "mm"),
    legend.spacing.x = grid::unit(1.4, "mm"),
    legend.margin = margin(t = 2, r = 0, b = 1.5, l = 0),
    legend.box.spacing = grid::unit(1.0, "mm"),
    plot.margin = margin(1, 3, 2.5, 3, unit = "mm")
  )

output_name <- "Figure 3_northern_subtype_pandemic_segmented"
base <- file.path(out_dir, output_name)

# Widened standard 183 mm two-column width for legibility.
fig_width <- 230
fig_height <- 91
ggsave(
  paste0(base, ".png"), p,
  width = fig_width, height = fig_height, units = "mm",
  dpi = 600, bg = "white"
)
ggsave(
  paste0(base, ".tiff"), p,
  width = fig_width, height = fig_height, units = "mm",
  dpi = 600, compression = "lzw", bg = "white"
)
ggsave(
  paste0(base, ".pdf"), p,
  width = fig_width / 25.4, height = fig_height / 25.4,
  device = grDevices::cairo_pdf, bg = "white"
)

caption <- paste(
  "Marginal standardized vaccine effectiveness in the Northern Hemisphere",
  "by influenza subtype and season. Points show annual estimates and 95%",
  "confidence intervals; filled and open circles indicate estimates supported",
  "by at least two studies and one study, respectively. Solid lines and",
  "shading show continuous segmented generalized meta-regressions using",
  "the full delta-method covariance V_VE plus a REML between-season",
  "variance component, with model-based 95% confidence intervals. The vertical dashed line marks",
  "the prespecified pandemic knot at 2020.00 (January 1, 2020). Downward arrows denote confidence",
  "intervals extending below the plotted range. Displayed P values test the",
  "difference between post- and pre-knot slopes and are Benjamini-Hochberg",
  "adjusted across the three prespecified subtype tests."
)
writeLines(caption, paste0(base, "_caption.txt"), useBytes = TRUE)

# Synchronize the copies consumed by the Figure 3 composition step.
figure3_dir <- file.path(root, "Results", "Figure3")
dir.create(figure3_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(
  paste0(base, ".png"),
  file.path(figure3_dir, paste0(output_name, ".png")),
  overwrite = TRUE
)
file.copy(
  paste0(base, ".tiff"),
  file.path(figure3_dir, paste0(output_name, ".tiff")),
  overwrite = TRUE
)

message("Saved figure to: ", base, ".{png,tiff,pdf}")
