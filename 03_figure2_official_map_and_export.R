# This helper is sourced by 04_generate_figure2_who_region_pathogen.R.

who_code_labels <- c(
  "AFR" = "African Region (AFRO)",
  "AMR" = "Region of the Americas (AMRO)",
  "EMR" = "Eastern Mediterranean Region (EMRO)",
  "EUR" = "European Region (EURO)",
  "SEAR" = "South-East Asia Region (SEARO)",
  "WPR" = "Western Pacific Region (WPRO)"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Read official WHO POLIS polygons without requiring the sf system library.
read_adm0_geojson <- function(path) {
  features <- fromJSON(path, simplifyVector = FALSE)$features

  feature_metadata <- map_dfr(seq_along(features), function(feature_index) {
    properties <- features[[feature_index]]$properties
    tibble(
      feature_id = feature_index,
      ISO_3_CODE = as.character(properties$ISO_3_CODE %||% NA_character_),
      ADM0_NAME = as.character(properties$ADM0_NAME %||% NA_character_),
      ADM0_VIZ_NAME = as.character(properties$ADM0_VIZ_NAME %||% NA_character_),
      ADM0_SOVRN = as.character(properties$ADM0_SOVRN %||% NA_character_),
      WHO_STATUS = as.character(properties$WHO_STATUS %||% NA_character_),
      NATURAL_EARTH_NAME = as.character(properties$ADMIN %||% NA_character_)
    )
  })

  polygon_vertices <- map_dfr(seq_along(features), function(feature_index) {
    geometry <- features[[feature_index]]$geometry
    polygons <- if (geometry$type == "Polygon") {
      list(geometry$coordinates)
    } else if (geometry$type == "MultiPolygon") {
      geometry$coordinates
    } else {
      stop("Unexpected ADM0 geometry type: ", geometry$type)
    }

    map_dfr(seq_along(polygons), function(part_index) {
      rings <- polygons[[part_index]]
      map_dfr(seq_along(rings), function(ring_index) {
        coordinates <- do.call(rbind, rings[[ring_index]])
        tibble(
          feature_id = feature_index,
          part_id = part_index,
          ring_id = ring_index,
          vertex_id = seq_len(nrow(coordinates)),
          longitude = as.numeric(coordinates[, 1]),
          latitude = as.numeric(coordinates[, 2])
        )
      })
    })
  })

  list(metadata = feature_metadata, vertices = polygon_vertices)
}

read_line_geojson <- function(path) {
  features <- fromJSON(path, simplifyVector = FALSE)$features
  map_dfr(seq_along(features), function(feature_index) {
    geometry <- features[[feature_index]]$geometry
    if (geometry$type != "LineString") {
      stop("Unexpected disputed-border geometry type: ", geometry$type)
    }
    coordinates <- do.call(rbind, geometry$coordinates)
    tibble(
      feature_id = feature_index,
      border_name = as.character(
        features[[feature_index]]$properties$NAME %||% "Disputed border"
      ),
      vertex_id = seq_len(nrow(coordinates)),
      longitude = as.numeric(coordinates[, 1]),
      latitude = as.numeric(coordinates[, 2])
    )
  })
}

# Apply the Equal Earth projection to reduce global area distortion.
equal_earth_xy <- function(longitude, latitude) {
  lambda <- longitude * pi / 180
  phi <- latitude * pi / 180
  theta <- asin(sqrt(3) / 2 * sin(phi))
  a1 <- 1.340264
  a2 <- -0.081106
  a3 <- 0.000893
  a4 <- 0.003796
  denominator <- 3 * (
    9 * a4 * theta^8 + 7 * a3 * theta^6 + 3 * a2 * theta^2 + a1
  )
  tibble(
    map_x = 2 * sqrt(3) * lambda * cos(theta) / denominator,
    map_y = theta * (a4 * theta^8 + a3 * theta^6 + a2 * theta^2 + a1)
  )
}

project_map_xy <- function(longitude, latitude) {
  if (isTRUE(rectangular_who_polis)) {
    return(tibble(map_x = as.numeric(longitude), map_y = as.numeric(latitude)))
  }
  equal_earth_xy(longitude, latitude)
}

adm0 <- read_adm0_geojson(who_adm0_file)
disputed_borders <- read_line_geojson(who_disputed_file) %>%
  bind_cols(project_map_xy(.$longitude, .$latitude))

who_reference <- fromJSON(who_reference_file)$value %>%
  as_tibble() %>%
  filter(!is.na(ParentCode)) %>%
  transmute(
    ISO_3_CODE = Code,
    WHO_reference_name = Title,
    WHO_region_code = ParentCode
  )

boundary_audit <- adm0$metadata %>%
  left_join(who_reference, by = "ISO_3_CODE") %>%
  mutate(
    WHO_region = unname(who_code_labels[WHO_region_code]),
    assignment_status = if_else(
      is.na(WHO_region),
      "Not classified in WHO country-region reference",
      "Direct ISO-3 match to WHO country-region reference"
    )
  ) %>%
  arrange(is.na(WHO_region), WHO_region, ADM0_VIZ_NAME)

write.csv(boundary_audit, output_boundary_audit, row.names = FALSE, na = "")

unclassified_label <- "Not classified in WHO reference"
map_legend_levels <- c(
  "African Region (AFRO)",
  "European Region (EURO)",
  "South-East Asia Region (SEARO)",
  "Eastern Mediterranean Region (EMRO)",
  "Region of the Americas (AMRO)",
  "Western Pacific Region (WPRO)"
)
map_region_levels <- c(map_legend_levels, unclassified_label)
map_colors <- c(who_colors, set_names("#E6E6E6", unclassified_label))

# Each country is filled with a single flat color for its WHO region; color
# encodes region membership only and does not vary with literature volume.
boundary_source_map <- boundary_audit %>%
  select(feature_id, ISO_3_CODE, WHO_region) %>%
  mutate(
    WHO_region = coalesce(WHO_region, unclassified_label),
    WHO_region = factor(WHO_region, levels = map_region_levels),
    map_fill = unname(map_colors[as.character(WHO_region)])
  )

world_map <- adm0$vertices %>%
  left_join(boundary_source_map, by = "feature_id") %>%
  bind_cols(project_map_xy(.$longitude, .$latitude))

source_metadata <- fromJSON(who_source_metadata_file)
source_date <- as.Date(substr(source_metadata$retrieved_utc, 1, 10))

map_coordinate_system <- if (isTRUE(rectangular_who_polis)) {
  coord_fixed(
    ratio = 1,
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE,
    clip = "off"
  )
} else {
  coord_equal(expand = TRUE, clip = "off")
}

disputed_border_layers <- if (nrow(disputed_borders) > 0) {
  list(
    geom_path(
      data = disputed_borders,
      aes(x = map_x, y = map_y, group = feature_id),
      color = "white",
      linewidth = 0.7,
      inherit.aes = FALSE
    ),
    geom_path(
      data = disputed_borders,
      aes(x = map_x, y = map_y, group = feature_id),
      color = "gray15",
      linewidth = 0.35,
      linetype = "22",
      inherit.aes = FALSE
    )
  )
} else {
  NULL
}

final_map <- ggplot(world_map) +
  geom_polygon(
    aes(
      x = map_x,
      y = map_y,
      group = interaction(feature_id, part_id),
      subgroup = interaction(feature_id, part_id, ring_id),
      fill = map_fill
    ),
    color = "gray45",
    linewidth = 0.12,
    rule = "evenodd"
  ) +
  disputed_border_layers +
  scale_fill_identity() +
  scale_x_continuous(
    expand = if (isTRUE(rectangular_who_polis)) expansion(mult = 0) else expansion(mult = 0.005)
  ) +
  scale_y_continuous(
    expand = if (isTRUE(rectangular_who_polis)) expansion(mult = 0) else expansion(mult = 0.005)
  ) +
  map_coordinate_system +
  theme_void(base_family = figure_font_family) +
  theme(
    plot.margin = margin(1, 3, 1, 3),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Place each regional chart over its corresponding WHO region.
panel_positions <- if (isTRUE(rectangular_who_polis)) {
  tribble(
    ~who_region, ~longitude, ~latitude, ~panel_width, ~panel_height,
    "African Region (AFRO)", 10, -23, 68, 44,
    "Region of the Americas (AMRO)", -105, 18, 72, 44,
    "Eastern Mediterranean Region (EMRO)", 42, 22, 62, 42,
    "European Region (EURO)", 20, 66, 70, 42,
    "South-East Asia Region (SEARO)", 105, 2, 62, 44,
    "Western Pacific Region (WPRO)", 124, 43, 68, 42
  )
} else {
  tribble(
    ~who_region, ~longitude, ~latitude, ~panel_width, ~panel_height,
    "African Region (AFRO)", 15, -15, 0.98, 0.50,
    "Region of the Americas (AMRO)", -100, 20, 1.10, 0.50,
    "Eastern Mediterranean Region (EMRO)", 35, 21, 0.94, 0.50,
    "European Region (EURO)", 18, 53, 1.04, 0.50,
    "South-East Asia Region (SEARO)", 90, 10, 0.92, 0.50,
    "Western Pacific Region (WPRO)", 129, 38, 1.06, 0.50
  )
}

panel_positions <- panel_positions %>%
  bind_cols(project_map_xy(.$longitude, .$latitude))

for (region_name in region_names) {
  position <- filter(panel_positions, who_region == region_name)
  final_map <- final_map +
    annotation_custom(
      grob = ggplotGrob(region_results[[region_name]]$plot),
      xmin = position$map_x - position$panel_width / 2,
      xmax = position$map_x + position$panel_width / 2,
      ymin = position$map_y - position$panel_height / 2,
      ymax = position$map_y + position$panel_height / 2
    )
}

# Add one shared period legend in the upper-left map margin.
period_legend_plot <- ggplot(
  data.frame(Status = factor(period_levels, levels = period_levels)),
  aes(x = Status, fill = Status)
) +
  geom_bar() +
  scale_fill_manual(
    values = bar_colors,
    name = NULL,
    guide = guide_legend(
      ncol = 1,
      byrow = TRUE,
      keyheight = grid::unit(0.20, "cm")
    )
  ) +
  theme_void(base_family = figure_font_family) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7.0, face = "bold"),
    legend.key.height = grid::unit(0.20, "cm"),
    legend.key.width = grid::unit(0.34, "cm"),
    legend.spacing.y = grid::unit(-0.04, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

period_legend_grob <- cowplot::get_legend(period_legend_plot)
period_legend_bounds <- if (isTRUE(rectangular_who_polis)) {
  c(xmin = -166, xmax = -96, ymin = 60, ymax = 88)
} else {
  c(xmin = -2.66, xmax = -1.45, ymin = 0.93, ymax = 1.18)
}
final_map_with_period_legend <- final_map +
  annotation_custom(
    grob = period_legend_grob,
    xmin = period_legend_bounds[["xmin"]],
    xmax = period_legend_bounds[["xmax"]],
    ymin = period_legend_bounds[["ymin"]],
    ymax = period_legend_bounds[["ymax"]]
  )

# Keep the compact four-column, two-row WHO-region key used in the reference
# figure. The legend is given nearly the full canvas width below so long region
# names remain separated when the TIFF is resized in the manuscript.
region_legend_levels <- c(
  "African Region (AFRO)",
  "Eastern Mediterranean Region (EMRO)",
  "European Region (EURO)",
  "Region of the Americas (AMRO)",
  "South-East Asia Region (SEARO)",
  "Western Pacific Region (WPRO)",
  unclassified_label
)

region_legend_data <- tibble(
  WHO_region = factor(region_legend_levels, levels = region_legend_levels),
  x = seq_along(region_legend_levels),
  y = 1
)

region_legend_labels <- setNames(
  region_legend_levels,
  region_legend_levels
)

region_legend_source <- ggplot(
  region_legend_data,
  aes(x = x, y = y, fill = WHO_region)
) +
  geom_col() +
  scale_fill_manual(
    values = map_colors[region_legend_levels],
    labels = region_legend_labels,
    name = "WHO Regions",
    guide = guide_legend(
      nrow = 2,
      byrow = FALSE,
      title.position = "left",
      title.vjust = 0.5
    )
  ) +
  theme_void(base_family = figure_font_family) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 7.0, face = "bold"),
    legend.text = element_text(size = 5.8),
    legend.key.height = grid::unit(0.34, "cm"),
    legend.key.width = grid::unit(0.34, "cm"),
    legend.spacing.x = grid::unit(0.10, "cm"),
    legend.spacing.y = grid::unit(0.06, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.background = element_rect(fill = "white", color = NA)
  )

region_legend_grob <- cowplot::get_legend(region_legend_source)
region_legend_width <- 0.99
region_legend_x <- if (isTRUE(rectangular_who_polis)) {
  0
} else {
  0.08
}
region_legend_plot <- cowplot::ggdraw() +
  cowplot::draw_grob(
    region_legend_grob,
    x = region_legend_x,
    y = 0.10,
    width = region_legend_width,
    height = 0.86
  )

final_figure <- cowplot::plot_grid(
  final_map_with_period_legend,
  region_legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.17),
  align = "v"
)

n_multi_region <- sum(
  excluded_regions$n[excluded_regions$who_region == "Multi-region"]
)

boundary_caption <- paste(
  "Administrative boundaries are from the WHO POLIS GLOBAL_ADM0 layer and",
  "disputed borders from the WHO POLIS DISPUTED_BORDERS layer",
  paste0("(", format(source_date, "%d %B %Y"), ")"),
  "displayed in a rectangular longitude-latitude layout; Antarctica is not",
  "included in this layer and falls outside the six WHO regional categories",
  "shown. Dashed lines indicate approximate disputed boundaries where",
  "agreement may be incomplete."
)

# Nature-style legend: a bold lead sentence (kept on its own line so it can be
# set in bold when typeset) followed by one dense explanatory paragraph.
figure_caption <- c(
  paste(
    "Figure 2 | Vaccine effectiveness before and during/after the COVID-19",
    "pandemic, by WHO region and influenza virus group."
  ),
  paste(
    "For each of the six WHO regions, bars show median vaccine effectiveness",
    "before (orange) and during/after (green) the COVID-19 pandemic for all",
    "influenza estimates (All) and for influenza A virus (IAV) and influenza B",
    "virus (IBV) separately, with error bars denoting the interquartile",
    "range. Only observations marked Yes for use in logOR synthesis and with",
    "finite, numerically plausible VE values were included;", n_multi_region,
    "otherwise eligible observations classified as",
    "multi-region were excluded from the region-specific panels because they",
    "could not be assigned to a single WHO region. Countries are colored by",
    "WHO region (see legend); coloring is uniform within each region and does",
    "not encode literature volume.", boundary_caption,
    "Areas absent from the WHO country-region",
    "reference are shown in gray. Boundaries, names and designations do not",
    "imply any position regarding the legal status of a country or territory",
    "or the delimitation of its frontiers."
  )
)
writeLines(figure_caption, output_caption, useBytes = TRUE)

ggsave(
  output_tiff,
  plot = final_figure,
  width = 7.2,
  height = 3.7,
  device = "tiff",
  type = "cairo",
  family = figure_font_family,
  dpi = output_dpi,
  bg = "white",
  compression = "lzw",
  limitsize = FALSE
)

ggsave(
  output_pdf,
  plot = final_figure,
  width = 7.2,
  height = 3.7,
  device = cairo_pdf,
  family = figure_font_family,
  bg = "white",
  limitsize = FALSE
)

message("Wrote: ", output_tiff)
message("Wrote: ", output_pdf)
message("Wrote: ", output_statistics)
message("Wrote: ", output_boundary_audit)
message("Wrote: ", output_country_sources)
message("Wrote: ", output_country_unmatched)
message("Wrote: ", output_anomaly_audit)
message("Wrote: ", output_caption)
