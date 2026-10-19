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

# Tables S6-S8, S10 and S19; source counts for Figure S4.

study_characteristics <- function() {
  check_required_packages(c("readxl", "openxlsx"))
  root <- get_project_root()
  data_dir <- file.path(root, "Data")
  out_dir <- file.path(results_root(root), "DescriptiveTables")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dat <- read_source_verified_analysis_data(root)

  # Attach the source Excel row. read_source_verified_analysis_data() returns
  # exactly the rows marked "Use in logOR synthesis" = Yes, in workbook order.
  raw <- readxl::read_excel(
    file.path(data_dir, "review_data.xlsx"), sheet = "Review data", col_types = "text"
  )
  eligible_rows <- which(
    tolower(trimws(as.character(raw$`Use in logOR synthesis`))) == "yes"
  ) + 1L
  stopifnot(length(eligible_rows) == nrow(dat), nrow(dat) == 8440L)

  dat <- dat |>
    dplyr::mutate(
      excel_row = eligible_rows,
      study_id = as.character(.data$study_id),
      season = as.character(.data$season),
      period = factor(
        .data$period_original, levels = c(0, 1),
        labels = c("Pre-pandemic", "Pandemic/post-pandemic")
      )
    )
  stopifnot(dplyr::n_distinct(dat$study_id) == 349L)

  count_by <- function(data, group, levels) {
    data |>
      dplyr::mutate(Category = factor(.data[[group]], levels = levels)) |>
      dplyr::group_by(.data$Category, .drop = FALSE) |>
      dplyr::summarise(
        VE_estimates = dplyr::n(),
        Studies = dplyr::n_distinct(.data$study_id),
        .groups = "drop"
      )
  }

  # ---- Tables S6 and S7: season and subtype by hemisphere ---------------------
  # Northern Hemisphere seasons are consecutive two-year labels (YYYY-YYYY);
  # Southern Hemisphere seasons are four-digit calendar-year labels (YYYY).
  # Estimates covering several seasons are not assigned to a single season.
  subtype_levels <- c("A(H1N1)pdm09", "A(H3N2)", "B/Victoria", "B/Yamagata")
  cross_year <- stringr::str_match(dat$season, "^(\\d{4})-(\\d{4})$")
  is_northern <- !is.na(cross_year[, 1]) &
    as.integer(cross_year[, 3]) == as.integer(cross_year[, 2]) + 1L
  is_southern <- stringr::str_detect(dat$season, "^\\d{4}$")
  season_dat <- dat |>
    dplyr::mutate(
      hemisphere = dplyr::case_when(
        is_northern ~ "Northern", is_southern ~ "Southern", TRUE ~ NA_character_
      ),
      subtype = dplyr::case_when(
        .data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)") ~ "A(H1N1)pdm09",
        .data$pathogen_specific == "A(H3N2)" ~ "A(H3N2)",
        .data$pathogen_specific == "B(Victoria)" ~ "B/Victoria",
        .data$pathogen_specific == "B(Yamagata)" ~ "B/Yamagata",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$hemisphere), !is.na(.data$subtype))

  season_table <- function(hemisphere_name, seasons) {
    sub <- season_dat[season_dat$hemisphere == hemisphere_name, , drop = FALSE]
    stopifnot(all(sub$season %in% seasons))
    cell <- function(rows) {
      unlist(lapply(subtype_levels, function(subtype) {
        x <- rows[rows$subtype == subtype, , drop = FALSE]
        stats::setNames(
          c(dplyr::n_distinct(x$study_id), nrow(x)),
          paste(subtype, c("Publications", "Estimates"), sep = " | ")
        )
      }))
    }
    body <- lapply(seasons, function(season) cell(sub[sub$season == season, , drop = FALSE]))
    # Publications in the Total row are unique publications across seasons.
    out <- tibble::as_tibble(do.call(rbind, c(body, list(cell(sub)))))
    tibble::add_column(out, Season = c(seasons, "Total"), .before = 1)
  }
  table_s6 <- season_table("Northern", sprintf("%d-%d", 2010:2024, 2011:2025))
  table_s7 <- season_table("Southern", as.character(2010:2025))

  # ---- Table S8: characteristics of the included articles ---------------------
  characteristics <- readxl::read_excel(
    file.path(data_dir, "Supplementary Data_Characteristics and Quality score.xlsx"),
    sheet = "characteristic", skip = 2
  ) |>
    dplyr::transmute(
      study_id = trimws(as.character(.data$`Article ID`)),
      year_published = as.numeric(.data$`Year published`),
      sample_size = as.numeric(.data$`Sample size`)
    )
  stopifnot(
    nrow(characteristics) == 349L,
    all(dat$study_id %in% characteristics$study_id),
    !anyNA(characteristics$year_published), !anyNA(characteristics$sample_size)
  )
  article_dat <- dat |>
    dplyr::left_join(characteristics, by = "study_id") |>
    dplyr::mutate(
      year_group = as.character(cut(
        .data$year_published, breaks = c(2010, 2015, 2020, 2025),
        labels = c("2011-2015", "2016-2020", "2021-2025")
      )),
      sample_group = as.character(cut(
        .data$sample_size, breaks = c(0, 1000, 2000, 5000, 10000, 99999, Inf),
        labels = c(
          "≤1,000", "1,001–2,000", "2,001–5,000",
          "5,001–10,000", "10,001–99,999", "≥100,000"
        )
      ))
    )
  stopifnot(!anyNA(article_dat$year_group), !anyNA(article_dat$sample_group))
  characteristic_block <- function(variable, column, levels) {
    article_dat |>
      count_by(column, levels) |>
      dplyr::transmute(
        Variable = variable,
        Category = as.character(.data$Category),
        Articles = .data$Studies,
        Articles_percent = 100 * .data$Studies / 349,
        Estimates = .data$VE_estimates,
        Estimates_percent = 100 * .data$VE_estimates / nrow(dat)
      )
  }
  # Articles contributing estimates to more than one WHO region are counted once
  # within each region, so article percentages across regions may exceed 100%.
  table_s8 <- dplyr::bind_rows(
    characteristic_block("Year published", "year_group", c("2011-2015", "2016-2020", "2021-2025")),
    characteristic_block("Sample size", "sample_group", c(
      "≤1,000", "1,001–2,000", "2,001–5,000",
      "5,001–10,000", "10,001–99,999", "≥100,000"
    )),
    characteristic_block("Study design", "study_design", c("Case-control study", "Cohort study")),
    characteristic_block("WHO region", "who_label", c(
      "AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO", "Multi-region"
    ))
  )

  # ---- Table S10: study design by study period --------------------------------
  tnd <- readr::read_csv(
    file.path(data_dir, "case_control_test_negative_classification.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      study_id = as.character(.data$article_id),
      control_sampling_design = .data$control_sampling_design
    )
  # Estimate-level design for the studies reporting both test-negative and
  # conventional case-control analyses.
  mixed <- readr::read_csv(
    file.path(data_dir, "mixed_design_estimate_classification.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      excel_row = as.integer(.data$excel_row),
      mixed_study_id = as.character(.data$`Article ID`),
      estimate_design = .data$estimate_design
    )
  design_dat <- dat |>
    dplyr::left_join(tnd, by = "study_id") |>
    dplyr::left_join(mixed, by = "excel_row") |>
    dplyr::mutate(
      study_design_detail = dplyr::case_when(
        .data$study_design == "Cohort study" ~ "Cohort study",
        .data$control_sampling_design == "TND" ~ "Case-control study (TND)",
        .data$control_sampling_design == "conventional CC" ~ "Case-control study (conventional CC)",
        .data$control_sampling_design == "TND+conventional CC" ~ "Case-control study (TND+conventional CC)",
        TRUE ~ NA_character_
      ),
      estimate_from_tnd = dplyr::case_when(
        .data$control_sampling_design == "TND" ~ TRUE,
        .data$control_sampling_design == "TND+conventional CC" ~ .data$estimate_design == "TND",
        TRUE ~ FALSE
      )
    )
  is_mixed <- design_dat$study_design_detail == "Case-control study (TND+conventional CC)"
  stopifnot(
    !anyNA(design_dat$study_design_detail),
    !anyNA(design_dat$estimate_from_tnd),
    identical(is_mixed, !is.na(design_dat$estimate_design)),
    all(design_dat$mixed_study_id[is_mixed] == design_dat$study_id[is_mixed])
  )
  design_levels <- c(
    "Case-control study (TND)", "Case-control study (conventional CC)",
    "Case-control study (TND+conventional CC)", "Cohort study"
  )
  period_totals <- design_dat |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(
      total_studies = dplyr::n_distinct(.data$study_id),
      total_estimates = dplyr::n(), .groups = "drop"
    )
  table_s10 <- design_dat |>
    dplyr::mutate(study_design_detail = factor(.data$study_design_detail, levels = design_levels)) |>
    dplyr::group_by(.data$study_design_detail, .data$period, .drop = FALSE) |>
    dplyr::summarise(
      Studies = dplyr::n_distinct(.data$study_id),
      Estimates = dplyr::n(),
      Estimates_TND_analyses = sum(.data$estimate_from_tnd),
      Estimates_conventional_CC_analyses = .data$Estimates - .data$Estimates_TND_analyses,
      .groups = "drop"
    ) |>
    dplyr::mutate(study_design_detail = as.character(.data$study_design_detail)) |>
    dplyr::bind_rows(
      design_dat |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(
          study_design_detail = "All estimates from TND analyses",
          Studies = NA_integer_,
          Estimates = sum(.data$estimate_from_tnd),
          .groups = "drop"
        ),
      design_dat |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(
          study_design_detail = "Total",
          Studies = dplyr::n_distinct(.data$study_id),
          Estimates = dplyr::n(),
          .groups = "drop"
        )
    ) |>
    dplyr::left_join(period_totals, by = "period") |>
    dplyr::mutate(
      # The split by analysis design applies only to studies reporting both.
      dplyr::across(
        c("Estimates_TND_analyses", "Estimates_conventional_CC_analyses"),
        ~ dplyr::if_else(
          .data$study_design_detail == "Case-control study (TND+conventional CC)",
          .x, NA_integer_
        )
      ),
      Studies_percent = 100 * .data$Studies / .data$total_studies,
      Estimates_percent = 100 * .data$Estimates / .data$total_estimates,
      Estimates_TND_analyses_percent = 100 * .data$Estimates_TND_analyses / .data$total_estimates,
      Estimates_conventional_CC_analyses_percent =
        100 * .data$Estimates_conventional_CC_analyses / .data$total_estimates
    ) |>
    dplyr::select(
      Study_design = "study_design_detail", Period = "period",
      "Studies", "Studies_percent", "Estimates", "Estimates_percent",
      "Estimates_TND_analyses", "Estimates_TND_analyses_percent",
      "Estimates_conventional_CC_analyses", "Estimates_conventional_CC_analyses_percent"
    )

  tables <- list(
    TableS6_northern_season_subtype_counts = table_s6,
    TableS7_southern_season_subtype_counts = table_s7,
    TableS8_article_characteristics = table_s8,
    TableS10_study_design_by_period = table_s10
  )
  for (name in names(tables)) {
    readr::write_excel_csv(tables[[name]], file.path(out_dir, paste0(name, ".csv")), na = "")
    message("Wrote: ", file.path(out_dir, paste0(name, ".csv")))
  }
  workbook <- stats::setNames(tables, sub("_.*$", "", names(tables)))
  openxlsx::write.xlsx(
    workbook, file.path(out_dir, "Descriptive_tables.xlsx"), overwrite = TRUE
  )
  for (name in names(tables)) {
    cat("\n==", name, "==\n")
    print(tables[[name]], n = Inf, width = Inf)
  }
}

covariate_counts <- function() {
  check_required_packages(c("readxl"))
  root <- get_project_root()
  out <- file.path(results_root(root), "TableS19")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  dat <- prepare_period_data(read_source_verified_analysis_data(root))

  covariates <- c(
    age_display = "Age group", vaccine_type_display = "Vaccine type",
    pathogen_display = "Influenza type", region_short = "WHO region",
    outcome_display = "Clinical outcome"
  )
  stopifnot(nrow(dat) == 8440L, sum(dat$period_original == 0) == 6346L,
            sum(dat$period_original == 1) == 2094L, !anyNA(dat[names(covariates)]))
  cross <- dplyr::bind_rows(lapply(names(covariates), function(v) {
    dplyr::bind_rows(lapply(levels(dat[[v]]), function(level) {
        dplyr::bind_rows(lapply(0:1, function(period_index) {
          x <- dat[dat[[v]] == level & dat$period_original == period_index, ]
        tibble::tibble(
          covariate = covariates[[v]], category = level,
            period = c("Pre-pandemic", "Pandemic/post-pandemic")[period_index + 1L],
          studies = dplyr::n_distinct(x$study_id), estimates = nrow(x),
            percent = 100 * nrow(x) / sum(dat$period_original == period_index)
        )
      }))
    }))
  }))
  out_csv <- file.path(out, "TableS19_covariate_period_crosstab.csv")
  readr::write_excel_csv(cross, out_csv)
  print(cross, n = Inf)
  message("Wrote: ", out_csv)

  readr::write_csv(dat |> dplyr::select(study_id, period, period_original, dplyr::all_of(names(covariates))),
    file.path(out, "covariate_data.csv"))
  writeLines(unname(tools::md5sum(file.path(root, "Data", "review_data.xlsx"))), file.path(out, "review_data.md5"))
}

if (sys.nframe() == 0L) {
  study_characteristics()
  covariate_counts()
}
