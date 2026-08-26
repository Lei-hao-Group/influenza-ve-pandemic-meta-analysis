#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(jsonlite)
  library(purrr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

input_file <- file.path(
  "Submission_Code", "Data",
  "review_data.xlsx"
)
input_sheet <- "Review data"
world_adm0_file <- file.path(
  "Submission_Code", "Data", "geo", "ne_50m_admin_0_countries",
  "GLOBAL_ADM0_WITH_ANTARCTICA.geojson"
)
output_dir <- file.path("Results", "Figure2", "Country_evidence_maps")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_counts <- file.path(output_dir, "Figure2_country_article_estimate_counts.csv")
output_mapping_audit <- file.path(output_dir, "Figure2_country_text_mapping_audit.csv")
output_unmatched <- file.path(output_dir, "Figure2_country_text_unmatched.csv")
output_caption <- file.path(output_dir, "Figure2_country_evidence_maps_caption.txt")
output_dpi <- as.numeric(Sys.getenv("FIGURE_DPI", "600"))

stopifnot(
  file.exists(input_file),
  file.exists(world_adm0_file)
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Country names and common variants occurring in the source-verified workbook.
# A multi-country cell is expanded to every country explicitly named in it.
country_patterns <- tribble(
  ~ISO_3_CODE, ~country_name, ~regex, ~longitude, ~latitude,
  "ARG", "Argentina", "\\bargentina\\b", -64, -35,
  "AUS", "Australia", "\\baustralia\\b|\\baustrilia\\b", 134, -25,
  "AUT", "Austria", "\\baustria\\b", 14.5, 47.5,
  "BEL", "Belgium", "\\bbelgium\\b", 4.7, 50.8,
  "BRA", "Brazil", "\\bbrazil\\b", -52, -10,
  "CAN", "Canada", "\\bcanada\\b", -106, 57,
  "CHL", "Chile", "\\bchile\\b", -71, -30,
  "CHN", "China", "\\bchina\\b|\\bhong kong\\b", 103, 35,
  "CIV", "Cote d'Ivoire", "\\bcote d ivoire\\b|\\bivory coast\\b", -5.5, 7.5,
  "COL", "Colombia", "\\bcolombia\\b", -74, 4,
  "CRI", "Costa Rica", "\\bcosta rica\\b", -84, 10,
  "CZE", "Czechia", "\\bczech republic\\b|\\bczechia\\b", 15.5, 49.8,
  "DEU", "Germany", "\\bgermany\\b", 10.5, 51,
  "DNK", "Denmark", "\\bdenmark\\b", 10, 56,
  "ESP", "Spain", "\\bspain\\b", -4, 40,
  "FIN", "Finland", "\\bfinland\\b", 26, 64,
  "FRA", "France", "\\bfrance\\b", 2, 46,
  "GBR", "United Kingdom", "\\bunited kingdom\\b|\\buk\\b|\\bengland\\b|\\bscotland\\b|\\bwales\\b|\\bnorthern ireland\\b", -3, 55,
  "GRC", "Greece", "\\bgreece\\b", 22, 39,
  "HND", "Honduras", "\\bhonduras\\b", -86.5, 15,
  "HRV", "Croatia", "\\bcroatia\\b", 16, 45,
  "HUN", "Hungary", "\\bhungary\\b", 19, 47,
  "IND", "India", "\\bindia\\b", 79, 22,
  "IRL", "Ireland", "\\bireland\\b|\\brepublic of ireland\\b", -8, 53,
  "ISR", "Israel", "\\bisrael\\b", 35, 31.5,
  "ITA", "Italy", "\\bitaly\\b", 12, 42,
  "JPN", "Japan", "\\bjapan\\b", 138, 37,
  "KOR", "Republic of Korea", "\\brepublic of korea\\b|\\bsouth korea\\b|\\bkorea\\b", 128, 36,
  "LTU", "Lithuania", "\\blithuania\\b", 24, 55,
  "MLT", "Malta", "\\bmalta\\b", 14.4, 35.9,
  "MEX", "Mexico", "\\bmexico\\b", -102, 23,
  "NLD", "Netherlands", "\\bnetherlands\\b|\\bthe netherlands\\b", 5.5, 52,
  "NOR", "Norway", "\\bnorway\\b", 10, 62,
  "NZL", "New Zealand", "\\bnew zealand\\b", 172, -41,
  "PAN", "Panama", "\\bpanama\\b", -80, 8.5,
  "PER", "Peru", "\\bperu\\b", -75, -9,
  "POL", "Poland", "\\bpoland\\b", 19, 52,
  "PRT", "Portugal", "\\bportugal\\b", -8, 39.5,
  "PRY", "Paraguay", "\\bparaguay\\b", -58, -23,
  "ROU", "Romania", "\\bromania\\b", 25, 46,
  "RUS", "Russian Federation", "\\brussian federation\\b|\\brussia\\b", 80, 60,
  "SAU", "Saudi Arabia", "\\bsaudi arabia\\b|\\bkingdom of saudi arabia\\b|\\bksa\\b|\\briyadh\\b", 45, 24,
  "SGP", "Singapore", "\\bsingapore\\b", 104, 1.3,
  "SLV", "El Salvador", "\\bel salvador\\b", -89, 13.8,
  "SWE", "Sweden", "\\bsweden\\b", 16, 62,
  "THA", "Thailand", "\\bthailand\\b", 101, 15,
  "TUN", "Tunisia", "\\btunisia\\b", 9.5, 34,
  "TUR", "Türkiye", "\\bturkey\\b|\\bturkiye\\b|\\btürkiye\\b", 35, 39,
  "URY", "Uruguay", "\\buruguay\\b", -56, -33,
  "USA", "United States", "\\bunited states\\b|\\busa\\b|\\bu s\\b", -100, 38,
  "ZAF", "South Africa", "\\bsouth africa\\b", 24, -29
)

# Compact grid used only for the dense European cluster. Labels remain plain
# text, as in the reference panel, with thin leader lines to the country.
europe_label_positions <- tribble(
  ~ISO_3_CODE, ~label_longitude, ~label_latitude,
  "GBR", -30, 70,
  "NOR",  -5, 70,
  "SWE",  20, 70,
  "FIN",  45, 70,
  "RUS",  70, 70,
  "IRL", -30, 62,
  "NLD",  -5, 62,
  "DNK",  20, 62,
  "LTU",  45, 62,
  "POL",  70, 62,
  "PRT", -30, 54,
  "BEL",  -5, 54,
  "DEU",  20, 54,
  "CZE",  45, 54,
  "ROU",  70, 54,
  "ESP", -30, 46,
  "FRA",  -5, 46,
  "AUT",  20, 46,
  "HUN",  45, 46,
  "HRV",  70, 46,
  "ITA",  -5, 38,
  "GRC",  30, 38,
  "TUR",  65, 38
)

normalize_country_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]\u3001]+", " ") %>%
    str_squish()
}

extract_country_iso3 <- function(country_text) {
  if (is.na(country_text) || !nzchar(str_trim(country_text))) return(character())
  normalized <- normalize_country_text(country_text)
  country_patterns$ISO_3_CODE[
    str_detect(normalized, country_patterns$regex)
  ] %>% unique()
}

# The source workbook's Epi field provides the audited period assignment.
period_levels <- c(
  "Pre-pandemic, 2010–2019",
  "Pandemic/post-pandemic, 2020–2025"
)

raw <- read_excel(input_file, sheet = input_sheet, col_types = "text")
required_columns <- c(
  "Article ID", "Country", "Epi", "Use in logOR synthesis"
)
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

eligible <- raw %>%
  mutate(
    estimate_id = row_number(),
    Article_ID = str_trim(as.character(`Article ID`)),
    Country = str_squish(as.character(Country)),
    period_code = str_to_lower(str_trim(as.character(Epi))),
    include_logor = str_to_lower(
      str_trim(as.character(`Use in logOR synthesis`))
    ) == "yes",
    period = case_when(
      period_code %in% c("0", "pre", "pre-pandemic", "prepandemic") ~
        period_levels[1],
      period_code %in% c(
        "1", "pan/post", "pandemic/post-pandemic",
        "pandemic and postpandemic", "pandemic and post-pandemic"
      ) ~ period_levels[2],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    include_logor,
    !is.na(Article_ID), nzchar(Article_ID),
    !is.na(Country), nzchar(Country),
    !is.na(period)
  ) %>%
  select(estimate_id, Article_ID, Country, period)

if (nrow(eligible) != 8440) {
  warning(
    "Expected 8,440 eligible estimates but found ", nrow(eligible),
    ". Check the source workbook and inclusion coding."
  )
}

mapping_audit <- eligible %>%
  distinct(Country) %>%
  mutate(
    ISO_3_CODE = map(Country, extract_country_iso3),
    n_explicit_countries = lengths(ISO_3_CODE),
    mapped_iso3 = map_chr(ISO_3_CODE, ~ paste(.x, collapse = ";")),
    mapping_status = if_else(
      n_explicit_countries > 0,
      "Mapped to explicitly named country/countries",
      "No explicitly named country matched"
    )
  ) %>%
  select(Country, n_explicit_countries, mapped_iso3, mapping_status) %>%
  arrange(mapping_status, Country)

write.csv(mapping_audit, output_mapping_audit, row.names = FALSE, na = "")
write.csv(
  filter(mapping_audit, n_explicit_countries == 0),
  output_unmatched,
  row.names = FALSE,
  na = ""
)

country_estimates <- eligible %>%
  mutate(ISO_3_CODE = map(Country, extract_country_iso3)) %>%
  unnest(ISO_3_CODE) %>%
  distinct(estimate_id, Article_ID, period, ISO_3_CODE)

country_counts <- country_estimates %>%
  group_by(period, ISO_3_CODE) %>%
  summarise(
    n_articles = n_distinct(Article_ID),
    n_estimates = n_distinct(estimate_id),
    .groups = "drop"
  ) %>%
  left_join(
    select(country_patterns, ISO_3_CODE, country_name, longitude, latitude),
    by = "ISO_3_CODE"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels),
    label = trimws(format(n_articles, scientific = FALSE))
  ) %>%
  arrange(period, desc(n_estimates), country_name)

write.csv(country_counts, output_counts, row.names = FALSE, na = "")

read_adm0_geojson <- function(path) {
  features <- fromJSON(path, simplifyVector = FALSE)$features
  metadata <- map_dfr(seq_along(features), function(feature_index) {
    properties <- features[[feature_index]]$properties
    tibble(
      feature_id = feature_index,
      ISO_3_CODE = as.character(properties$ISO_3_CODE %||% NA_character_),
      country_boundary_name = as.character(
        properties$ADM0_VIZ_NAME %||% properties$ADM0_NAME %||%
          properties$ADMIN %||% NA_character_
      )
    )
  })

  vertices <- map_dfr(seq_along(features), function(feature_index) {
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

  list(metadata = metadata, vertices = vertices)
}

adm0 <- read_adm0_geojson(world_adm0_file)

world_map <- adm0$vertices %>%
  left_join(select(adm0$metadata, feature_id, ISO_3_CODE), by = "feature_id") %>%
  mutate(map_x = longitude, map_y = latitude)

missing_boundaries <- setdiff(unique(country_counts$ISO_3_CODE), adm0$metadata$ISO_3_CODE)
if (length(missing_boundaries) > 0) {
  stop(
      "Mapped countries absent from Natural Earth boundary layer: ",
    paste(missing_boundaries, collapse = ", ")
  )
}

red_palette <- c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#DE2D26", "#99000D")
blue_palette <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")

make_country_map <- function(period_name, palette) {
  period_counts <- country_counts %>% filter(period == period_name)
  max_articles <- max(period_counts$n_articles, na.rm = TRUE)
  polygon_data <- world_map %>%
    left_join(
      select(period_counts, ISO_3_CODE, n_articles, n_estimates),
      by = "ISO_3_CODE"
    )

  label_data <- period_counts %>%
    left_join(europe_label_positions, by = "ISO_3_CODE") %>%
    mutate(
      manual_label = !is.na(label_longitude) & !is.na(label_latitude),
      target_longitude = coalesce(label_longitude, longitude),
      target_latitude = coalesce(label_latitude, latitude),
      label_color = if_else(
        ISO_3_CODE %in% c("USA", "CHN") & n_articles / max_articles >= 0.50,
        "white",
        "#303030"
      )
    ) %>%
    mutate(
      anchor_x = longitude,
      anchor_y = latitude,
      label_x = target_longitude,
      label_y = target_latitude
    )

  map_core <- ggplot(polygon_data) +
    geom_polygon(
      aes(
        x = map_x,
        y = map_y,
        group = interaction(feature_id, part_id),
        subgroup = interaction(feature_id, part_id, ring_id),
        fill = n_articles
      ),
      color = "gray28",
      linewidth = 0.16,
      rule = "evenodd"
    ) +
    geom_segment(
      data = filter(label_data, manual_label),
      aes(x = anchor_x, y = anchor_y, xend = label_x, yend = label_y),
      inherit.aes = FALSE,
      color = "gray55",
      linewidth = 0.18
    ) +
    geom_text(
      data = filter(label_data, manual_label),
      aes(x = label_x, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 3.6,
      family = "Arial",
      fontface = "bold",
      color = "#303030"
    ) +
    geom_text_repel(
      data = filter(label_data, !manual_label),
      aes(x = anchor_x, y = anchor_y, label = label, color = label_color),
      inherit.aes = FALSE,
      seed = 20260820,
      size = 3.6,
      family = "Arial",
      fontface = "bold",
      box.padding = 0.10,
      point.padding = 0.025,
      min.segment.length = 0,
      segment.color = "gray45",
      segment.size = 0.18,
      max.overlaps = Inf,
      max.time = 10,
      max.iter = 100000,
      force = 2.2,
      force_pull = 0.35,
      direction = "both"
    ) +
    scale_color_identity() +
    scale_fill_gradientn(
      colors = palette,
      limits = c(0, max_articles),
      breaks = unique(pretty(c(0, max_articles), n = 6)),
      labels = scales::label_number(accuracy = 1),
      na.value = "#E8E8E8",
      name = "Number of articles (shading)"
    ) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0)) +
    coord_fixed(
      ratio = 1,
      xlim = c(-180, 180),
      ylim = c(-90, 90),
      expand = FALSE,
      clip = "off"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "vertical",
        title.position = "left",
        title.hjust = 0.5,
        barheight = grid::unit(3.0, "cm"),
        barwidth = grid::unit(0.25, "cm"),
        ticks = FALSE,
        frame.colour = NA
      )
    ) +
    theme_void(base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(5, 5, 4, 5),
      legend.position = "inside",
      legend.position.inside = c(0.035, 0.50),
      legend.justification = c(0, 0.5),
      legend.direction = "vertical",
      legend.title = element_text(
        size = 9.2,
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.text = element_text(size = 8.5, color = "#2C4875"),
      legend.margin = margin(0, 0, 0, 0),
      legend.background = element_rect(fill = "transparent", color = NA)
    )

  map_core
}

pre_map <- make_country_map(period_levels[1], red_palette)
post_map <- make_country_map(period_levels[2], blue_palette)

save_plot_set <- function(plot, stem, width, height) {
  ggsave(
    file.path(output_dir, paste0(stem, ".png")),
    plot = plot, width = width, height = height, dpi = output_dpi,
    bg = "white", limitsize = FALSE
  )
  ggsave(
    file.path(output_dir, paste0(stem, ".tiff")),
    plot = plot, width = width, height = height, dpi = output_dpi,
    bg = "white", compression = "lzw", limitsize = FALSE
  )
  ggsave(
    file.path(output_dir, paste0(stem, ".pdf")),
    plot = plot, width = width, height = height, device = cairo_pdf,
    bg = "white", limitsize = FALSE
  )
}

save_plot_set(
  pre_map,
  "Figure2_country_evidence_prepandemic_2010_2019_red",
  width = 9.2,
  height = 4.8
)
save_plot_set(
  post_map,
  "Figure2_country_evidence_pandemic_postpandemic_2020_2025_blue",
  width = 9.2,
  height = 4.8
)

add_period_caption <- function(plot, label, swatch_color) {
  plot +
    annotate(
      "rect",
      xmin = -176, xmax = -168,
      ymin = 82, ymax = 88,
      fill = swatch_color,
      color = NA
    ) +
    annotate(
      "text",
      x = -165, y = 85,
      label = label,
      hjust = 0,
      family = "Arial",
      size = 4.2,
      color = "black"
    )
}

pre_map_labeled <- add_period_caption(
  pre_map,
  "Prepandemic",
  red_palette[[5]]
)
post_map_labeled <- add_period_caption(
  post_map,
  "Pandemic/post-pandemic",
  blue_palette[[5]]
)

combined_map <- patchwork::wrap_plots(
  list(pre_map_labeled, post_map_labeled),
  ncol = 1,
  heights = c(1, 1)
)
save_plot_set(
  combined_map,
  "Figure2_country_evidence_maps_combined",
  width = 9.2,
  height = 9.5
)

caption <- c(
  paste(
    "Figure 2 | Geographic distribution of the influenza vaccine-effectiveness",
    "evidence base before and during/after the COVID-19 pandemic."
  ),
  paste(
    "Countries are shaded according to the number of distinct articles; darker",
    "red in 2010–2019 or darker blue in 2020–2025 indicates more articles,",
    "and gray indicates no eligible data. The continuous shading scale is",
    "period-specific. Country labels report the number of distinct articles. Counts",
    "include records marked 'Use in logOR synthesis = Yes'. For records that",
    "explicitly named multiple countries, the article was counted for every named",
    "country; country-level counts therefore should not be summed",
    "to obtain overall totals. Locations described only as unknown",
    "international sites were not inferred. Administrative boundaries are from",
    "the cached Natural Earth 1:50m administrative boundary layer, including",
    "Antarctica. Boundaries, names and designations do not imply any position regarding",
    "the legal status of any country or territory or the delimitation of its",
    "frontiers."
  )
)
writeLines(caption, output_caption, useBytes = TRUE)

message("Wrote country evidence maps and source data to: ", output_dir)
