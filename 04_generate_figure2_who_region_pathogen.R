cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
} else {
  file.path(getwd(), "Submission_Code")
}

library(dplyr)
library(ggplot2)
library(jsonlite)
library(patchwork)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)

input_file <- "Submission_Code/Data/review_data.xlsx"
input_sheet <- "Review data"
who_reference_file <- "Submission_Code/Data/geo/who_gho_country_dimension.json"
figure2_layout <- str_to_lower(str_trim(Sys.getenv("FIGURE2_MAP_LAYOUT", "rectangular_who_polis")))
if (!identical(figure2_layout, "rectangular_who_polis")) {
  stop("Figure 2 now supports only the rectangular WHO POLIS layout.")
}
rectangular_who_polis <- TRUE
figure_font_family <- "Arial"
who_adm0_file <- "Submission_Code/Data/geo/who_polis/GLOBAL_ADM0.geojson"
who_disputed_file <- "Submission_Code/Data/geo/who_polis/DISPUTED_BORDERS.geojson"
who_source_metadata_file <- "Submission_Code/Data/geo/who_polis/source_metadata.json"
output_dir <- file.path("Results", "Figure2")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_suffix <- ""
output_file <- function(stem, extension) {
  file.path(output_dir, paste0(stem, output_suffix, ".", extension))
}
output_tiff <- output_file("Figure2_WHO_regions_pathogen_significance_updated", "tiff")
output_pdf <- output_file("Figure2_WHO_regions_pathogen_significance_updated", "pdf")
output_statistics <- output_file("Figure2_WHO_regions_pathogen_summary_updated", "csv")
output_boundary_audit <- output_file("Figure2_WHO_boundary_region_assignment_audit", "csv")
output_country_sources <- output_file("Figure2_country_literature_source_counts", "csv")
output_country_unmatched <- output_file("Figure2_country_literature_source_unmatched", "csv")
output_anomaly_audit <- output_file("Figure2_excluded_obvious_VE_anomalies", "csv")
output_caption <- output_file("Figure2_caption_and_map_notes", "txt")
output_dpi <- as.numeric(Sys.getenv("FIGURE_DPI", "600"))

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

country_patterns <- tribble(
  ~ISO_3_CODE, ~regex,
  "ARG", "\\bargentina\\b",
  "AUS", "\\baustralia\\b|\\baustrilia\\b",
  "AUT", "\\baustria\\b",
  "BEL", "\\bbelgium\\b",
  "BRA", "\\bbrazil\\b",
  "CAN", "\\bcanada\\b",
  "CHL", "\\bchile\\b",
  "CHN", "\\bchina\\b|\\bhong kong\\b",
  "CIV", "\\bcote d ivoire\\b|\\bivory coast\\b",
  "COL", "\\bcolombia\\b",
  "CRI", "\\bcosta rica\\b",
  "CZE", "\\bczech republic\\b|\\bczechia\\b",
  "DEU", "\\bgermany\\b",
  "DNK", "\\bdenmark\\b",
  "ESP", "\\bspain\\b",
  "FIN", "\\bfinland\\b",
  "FRA", "\\bfrance\\b",
  "GBR", "\\bunited kingdom\\b|\\buk\\b|\\bengland\\b|\\bscotland\\b|\\bwales\\b|\\bnorthern ireland\\b",
  "GRC", "\\bgreece\\b",
  "HRV", "\\bcroatia\\b",
  "HUN", "\\bhungary\\b",
  "IND", "\\bindia\\b",
  "IRL", "\\bireland\\b|\\brepublic of ireland\\b",
  "ISR", "\\bisrael\\b",
  "ITA", "\\bitaly\\b",
  "JPN", "\\bjapan\\b",
  "KOR", "\\brepublic of korea\\b|\\bsouth korea\\b|\\bkorea\\b",
  "LTU", "\\blithuania\\b",
  "MLT", "\\bmalta\\b",
  "MEX", "\\bmexico\\b",
  "NLD", "\\bnetherlands\\b|\\bthe netherlands\\b",
  "NOR", "\\bnorway\\b",
  "NZL", "\\bnew zealand\\b",
  "PER", "\\bperu\\b",
  "POL", "\\bpoland\\b",
  "PRT", "\\bportugal\\b",
  "PRY", "\\bparaguay\\b",
  "ROU", "\\bromania\\b",
  "RUS", "\\brussian federation\\b|\\brussia\\b",
  "SAU", "\\bsaudi arabia\\b|\\bkingdom of saudi arabia\\b|\\bksa\\b|\\briyadh\\b",
  "SGP", "\\bsingapore\\b",
  "SLV", "\\bel salvador\\b",
  "SWE", "\\bsweden\\b",
  "THA", "\\bthailand\\b",
  "TUN", "\\btunisia\\b",
  "TUR", "\\bturkey\\b|\\bturkiye\\b|\\btürkiye\\b",
  "URY", "\\buruguay\\b",
  "USA", "\\bunited states\\b|\\busa\\b|\\bu s\\b|\\bus\\b",
  "ZAF", "\\bsouth africa\\b"
)

extract_country_iso3 <- function(country_text) {
  if (is.na(country_text) || !nzchar(str_trim(country_text))) {
    return(character())
  }

  normalized_text <- country_text %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]\u3001]+", " ") %>%
    str_squish()

  country_patterns$ISO_3_CODE[
    str_detect(normalized_text, country_patterns$regex)
  ] %>%
    unique()
}

article_country_raw <- data_clean %>%
  filter(!is.na(Article_ID), nzchar(Article_ID),
         !is.na(Country), nzchar(Country)) %>%
  distinct(Article_ID, Country) %>%
  mutate(ISO_3_CODE = map(Country, extract_country_iso3))

article_country_pairs <- article_country_raw %>%
  unnest(ISO_3_CODE) %>%
  distinct(Article_ID, ISO_3_CODE)

country_source_counts <- article_country_pairs %>%
  count(ISO_3_CODE, name = "n_articles") %>%
  arrange(desc(n_articles), ISO_3_CODE)

country_source_reference <- fromJSON(who_reference_file)$value %>%
  as_tibble() %>%
  transmute(
    ISO_3_CODE = Code,
    country_name = Title,
    WHO_region_code = ParentCode,
    WHO_region_name = ParentTitle
  )

country_source_counts_output <- country_source_counts %>%
  left_join(country_source_reference, by = "ISO_3_CODE") %>%
  group_by(WHO_region_code, WHO_region_name) %>%
  mutate(
    region_source_articles = sum(n_articles),
    country_region_share = n_articles / region_source_articles,
    country_region_share_percent = 100 * country_region_share
  ) %>%
  ungroup() %>%
  select(
    ISO_3_CODE, country_name, WHO_region_code, WHO_region_name, n_articles,
    region_source_articles, country_region_share_percent
  ) %>%
  arrange(desc(n_articles), country_name, ISO_3_CODE)

country_source_unmatched <- article_country_raw %>%
  filter(lengths(ISO_3_CODE) == 0) %>%
  select(Article_ID, Country) %>%
  arrange(Article_ID, Country)

write.csv(country_source_counts_output, output_country_sources, row.names = FALSE, na = "")
write.csv(country_source_unmatched, output_country_unmatched, row.names = FALSE, na = "")

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

source(file.path(script_dir, "03_figure2_official_map_and_export.R"))

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
