#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
code_dir <- if (sys.nframe() > 0L) {
  dirname(normalizePath(sys.frame(1)$ofile))
} else if (length(script_file)) {
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", script_file[[1]]), fixed = TRUE)))
} else normalizePath(".")
source(file.path(code_dir, "functions.R"), local = TRUE)
root <- code_dir
setwd(root)

# Figure 2 and Figure S1.

regional_ve_map <- function() {
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(patchwork)
  library(purrr)
  library(readxl)
  library(stringr)
  library(tidyr)

  input_file <- "Data/review_data.xlsx"
  input_sheet <- "Review data"
  who_reference_file <- "Data/geo/who_gho_country_dimension.json"
  rectangular_who_polis <- TRUE
  figure_font_family <- "Arial"
  who_adm0_file <- "Data/geo/who_polis/GLOBAL_ADM0.geojson"
  who_disputed_file <- "Data/geo/who_polis/DISPUTED_BORDERS.geojson"
  who_source_metadata_file <- "Data/geo/who_polis/source_metadata.json"
  output_dir <- file.path(results_root(root), "Figure2")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_suffix <- ""
  output_file <- function(stem, extension) {
    file.path(output_dir, paste0(stem, output_suffix, ".", extension))
  }
  output_tiff <- output_file("Figure 2", "tiff")
  output_pdf <- output_file("Figure 2", "pdf")
  output_statistics <- output_file("Figure2_WHO_regions_virus_type_summary", "csv")
  output_boundary_audit <- output_file("Figure2_WHO_boundary_region_assignment_audit", "csv")
  output_anomaly_audit <- output_file("Figure2_excluded_obvious_VE_anomalies", "csv")
  output_caption <- output_file("Figure2_caption_and_map_notes", "txt")
  output_dpi <- as.numeric(Sys.getenv("FIGURE_DPI", "1200"))

  required_columns <- c(
    "Article ID", "VE", "Pathogen", "Country", "WHO Region", "Epi",
    "Use in logOR synthesis"
  )
  period_levels <- c("Prepandemic", "Pandemic and postpandemic")
  group_levels <- c("All", "IAV", "IBV")
  # VE = 100 * (1 - OR) has a theoretical upper bound of 100% but no strict
  # lower bound. Retain plausible negative estimates while excluding only
  # numerical explosions that clearly indicate invalid source values.
  ve_lower_plausibility_bound <- -1000
  ve_upper_plausibility_bound <- 100
  who_region_levels <- c(
    "African Region (AFRO)",
    "Region of the Americas (AMRO)",
    "Eastern Mediterranean Region (EMRO)",
    "European Region (EURO)",
    "South-East Asia Region (SEARO)",
    "Western Pacific Region (WPRO)"
  )

  bar_colors <- c(
    "Prepandemic" = "#D55E00",
    "Pandemic and postpandemic" = "#009E73"
  )

  # Region colors are kept distinct from the two period colors used in the bars.
  who_colors <- c(
    "African Region (AFRO)" = "#FFFAD3",
    "Region of the Americas (AMRO)" = "#E3D2E7",
    "Eastern Mediterranean Region (EMRO)" = "#DBF1D6",
    "European Region (EURO)" = "#C5DFED",
    "South-East Asia Region (SEARO)" = "#D3D4E8",
    "Western Pacific Region (WPRO)" = "#FCCECA"
  )

  stopifnot(
    file.exists(input_file),
    file.exists(who_reference_file),
    file.exists(who_adm0_file),
    file.exists(who_disputed_file),
    file.exists(who_source_metadata_file)
  )

  data_all <- suppressWarnings(read_excel(input_file, sheet = input_sheet))
  missing_columns <- setdiff(required_columns, names(data_all))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  # Convert the new text period codes to the two labels used in the original figure.
  data_clean <- data_all %>%
    transmute(
      Article_ID = as.character(`Article ID`),
      Country = str_squish(as.character(Country)),
      VE = as.numeric(VE) * 100,
      Pathogen = str_trim(as.character(Pathogen)),
      who_region = str_trim(as.character(`WHO Region`)),
      use_in_logor_synthesis = str_to_lower(
        str_trim(as.character(`Use in logOR synthesis`))
      ),
      period_code = str_to_lower(str_trim(as.character(Epi))),
      Status = case_when(
        period_code %in% c("0", "pre", "pre-pandemic", "prepandemic") ~
          "Prepandemic",
        period_code %in% c(
          "1", "pan/post", "pandemic/post-pandemic",
          "pandemic and postpandemic", "pandemic and post-pandemic"
        ) ~ "Pandemic and postpandemic",
        TRUE ~ NA_character_
      )
    )

  unrecognized_periods <- data_clean %>%
    filter(is.na(Status)) %>%
    distinct(period_code) %>%
    pull(period_code)
  if (length(unrecognized_periods) > 0) {
    stop("Unrecognized Epi values: ", paste(unrecognized_periods, collapse = ", "))
  }

  eligible_data <- data_clean %>%
    filter(use_in_logor_synthesis == "yes")

  obvious_ve_anomalies <- eligible_data %>%
    filter(
      !is.finite(VE) |
        VE < ve_lower_plausibility_bound |
        VE > ve_upper_plausibility_bound
    ) %>%
    mutate(
      exclusion_reason = case_when(
        !is.finite(VE) ~ "Missing or non-finite VE",
        VE > ve_upper_plausibility_bound ~ "VE exceeds the theoretical 100% upper bound",
        VE < ve_lower_plausibility_bound ~ "Numerically implausible VE below -1000%",
        TRUE ~ "Excluded by VE plausibility check"
      )
    )
  write.csv(obvious_ve_anomalies, output_anomaly_audit, row.names = FALSE, na = "")

  eligible_data <- eligible_data %>%
    filter(
      is.finite(VE),
      VE >= ve_lower_plausibility_bound,
      VE <= ve_upper_plausibility_bound
    )

  excluded_regions <- eligible_data %>%
    filter(!is.na(who_region), !who_region %in% who_region_levels) %>%
    count(who_region, name = "n")
  if (nrow(excluded_regions) > 0) {
    message(
      "Excluded observations outside the six mapped WHO regions: ",
      paste0(excluded_regions$who_region, " (n=", excluded_regions$n, ")",
             collapse = "; ")
    )
  }

  data_clean <- eligible_data %>%
    filter(
      is.finite(VE),
      !is.na(who_region),
      who_region %in% who_region_levels
    ) %>%
    mutate(
      Status = factor(Status, levels = period_levels),
      who_region = factor(who_region, levels = who_region_levels)
    )

  if (nrow(data_clean) == 0) {
    stop("No usable observations remain after data preparation.")
  }

  summarize_ve <- function(data, group_label) {
    data %>%
      group_by(Status) %>%
      summarise(
        n_obs = sum(!is.na(VE)),
        median_ve = median(VE, na.rm = TRUE),
        q1 = quantile(VE, 0.25, na.rm = TRUE),
        q3 = quantile(VE, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Group_Label = group_label)
  }

  make_region_panel <- function(region_data, region_name) {
    region_abbr <- str_extract(region_name, "(?<=\\().+?(?=\\))")
    if (is.na(region_abbr)) region_abbr <- region_name

    stats_all <- summarize_ve(region_data, "All")
    stats_pathogen <- map_dfr(c("IAV", "IBV"), function(pathogen_name) {
      summarize_ve(filter(region_data, Pathogen == pathogen_name), pathogen_name)
    })

    region_stats <- bind_rows(stats_all, stats_pathogen) %>%
      mutate(
        WHO_Region = region_name,
        Region = region_abbr,
        Group_Label = factor(Group_Label, levels = group_levels)
      ) %>%
      select(WHO_Region, Region, Group_Label, Status, n_obs,
             median_ve, q1, q3)

    plot_data <- region_stats %>%
      complete(
        Group_Label = factor(group_levels, levels = group_levels),
        Status = factor(period_levels, levels = period_levels),
        fill = list(n_obs = 0)
      ) %>%
      mutate(
        median_ve = if_else(n_obs <= 1, NA_real_, median_ve),
        q1 = if_else(n_obs <= 1, NA_real_, q1),
        q3 = if_else(n_obs <= 1, NA_real_, q3)
      )

    dodge_width <- if (rectangular_who_polis) 0.86 else 0.72
    column_width <- if (rectangular_who_polis) 0.90 else 0.74
    dodge <- position_dodge(width = dodge_width)
    panel <- ggplot(
      plot_data,
      aes(x = Group_Label, y = median_ve, fill = Status)
    ) +
      geom_col(position = dodge, width = column_width, na.rm = TRUE) +
      geom_errorbar(
        aes(ymin = q1, ymax = q3),
        position = dodge,
        width = 0.24,
        linewidth = 0.45,
        color = "black",
        na.rm = TRUE
      ) +
      scale_y_continuous(
        limits = c(-5, 115),
        breaks = c(0, 25, 50, 75, 100),
        labels = function(x) paste0(x, "%")
      ) +
      scale_x_discrete(
        drop = FALSE,
        expand = if (rectangular_who_polis) {
          expansion(add = c(0.18, 0.18))
        } else {
          waiver()
        }
      ) +
      scale_fill_manual(
        values = bar_colors,
        name = NULL,
        guide = "none"
      ) +
      labs(title = region_abbr) +
      theme_classic(
        base_family = figure_font_family,
        base_size = if (rectangular_who_polis) 6.8 else 6
      ) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_blank(),
        axis.text.x = element_text(
          size = if (rectangular_who_polis) 6.1 else 5.2,
          color = "black",
          face = "bold",
          lineheight = 1.1,
          margin = margin(t = 1.2)
        ),
        axis.text.y = element_text(
          size = if (rectangular_who_polis) 5.7 else 4.8,
          color = "black",
          face = "bold",
          lineheight = 1.1,
          margin = margin(r = 1.2)
        ),
        axis.line = element_line(linewidth = 0.25, color = "black"),
        axis.ticks = element_line(linewidth = 0.25, color = "black"),
        plot.title = element_text(
          hjust = 0.5,
          size = if (rectangular_who_polis) 7.2 else 6.1,
          face = "bold",
          lineheight = 0.85,
          margin = margin(t = 0, b = -5)
        ),
        legend.text = element_text(size = 5.2),
        axis.ticks.length = grid::unit(0.05, "cm")
      )

    list(
      plot = panel,
      statistics = region_stats
    )
  }

  region_names <- who_region_levels[who_region_levels %in% data_clean$who_region]
  region_results <- set_names(map(region_names, function(region_name) {
    make_region_panel(filter(data_clean, who_region == region_name), region_name)
  }), region_names)

  all_statistics <- map_dfr(region_results, "statistics")
  write.csv(all_statistics, output_statistics, row.names = FALSE, na = "")

  # Use a Cairo device while assembling grobs so Arial metrics are measured with
  # the same font engine used for the final PDF and TIFF exports.
  font_metrics_device_file <- NULL
  previous_cowplot_null_device <- NULL
  if (rectangular_who_polis && grDevices::dev.cur() == 1L) {
    cowplot_cairo_null_device <- function(width, height) {
      grDevices::cairo_pdf(
        filename = tempfile(
          pattern = "figure2-cowplot-null-",
          fileext = ".pdf"
        ),
        width = width,
        height = height,
        family = figure_font_family
      )
      grDevices::dev.control("enable")
    }
    previous_cowplot_null_device <- cowplot::set_null_device(
      cowplot_cairo_null_device
    )
    font_metrics_device_file <- tempfile(fileext = ".pdf")
    grDevices::cairo_pdf(
      filename = font_metrics_device_file,
      width = 7.2,
      height = 3.6,
      family = figure_font_family
    )
  }

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

  # Figure legend with a lead sentence kept on its own line, followed by the
  # explanatory paragraph.
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
  message("Wrote: ", output_anomaly_audit)
  message("Wrote: ", output_caption)

  if (!is.null(font_metrics_device_file)) {
    grDevices::dev.off()
    unlink(font_metrics_device_file)
    unlink(list.files(
      tempdir(),
      pattern = "^figure2-cowplot-null-.*\\.pdf$",
      full.names = TRUE
    ))
    cowplot::set_null_device(previous_cowplot_null_device)
  }
}

study_distribution_map <- function() {
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
    "Data",
    "review_data.xlsx"
  )
  input_sheet <- "Review data"
  world_adm0_file <- file.path(
    "Data", "geo", "ne_50m_admin_0_countries",
    "GLOBAL_ADM0_WITH_ANTARCTICA.geojson"
  )
  output_dir <- file.path(results_root(root), "FigureS1")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  output_counts <- file.path(output_dir, "FigureS1_country_article_estimate_counts.csv")
  output_mapping_audit <- file.path(output_dir, "FigureS1_country_text_mapping_audit.csv")
  output_unmatched <- file.path(output_dir, "FigureS1_country_text_unmatched.csv")
  output_caption <- file.path(output_dir, "FigureS1_country_evidence_maps_caption.txt")
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
    "Figure S1",
    width = 9.2,
    height = 9.5
  )

  caption <- c(
    paste(
      "Figure S1 | Geographic distribution of the influenza vaccine-effectiveness",
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
}

if (sys.nframe() == 0L) {
  regional_ve_map()
  study_distribution_map()
}
