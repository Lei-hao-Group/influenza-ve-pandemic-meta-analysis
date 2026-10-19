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

# Tables S11-S18 and the analysis underlying Figure S3.

restricted_analysis <- function(sensitivity_mode) {
  check_required_packages(extra = c("openxlsx", "readxl"))

  source_verified_file <- file.path(
    "Data",
    "review_data.xlsx"
  )
  root <- get_project_root(input_csv = source_verified_file)
  dat_all <- read_source_verified_analysis_data(root)
  source_audit <- attr(dat_all, "source_audit")

  nos_modes <- c("s13_nos_ge7", "s14_nos_ge7_tnd_adapted")
  table_label <- dplyr::case_when(
    sensitivity_mode == "s12_case_control" ~ "Table S12",
    sensitivity_mode == "s13_nos_ge7" ~ "Table S13",
    sensitivity_mode == "s14_nos_ge7_tnd_adapted" ~ "Table S14",
    TRUE ~ "Table S11"
  )
  sheet_label <- table_label

  sensitivity_pre_seasons <- c(
    "2016", "2016-2017", "2016-2017, 2016",
    "2017", "2017-2018", "2017-2018, 2017",
    "2018", "2018-2019", "2018-2019, 2018",
    "2019", "2019-2020"
  )

  if (sensitivity_mode %in% nos_modes) {
    quality_file <- file.path(
      root,
      "Data",
      "Supplementary Data_Characteristics and Quality score.xlsx"
    )
    read_quality_sheet <- function(sheet) {
      # Some Total Scores cells are uncached Excel formulas, so calculate every
      # score directly from the eight NOS item columns (columns C:J). Numeric
      # stored totals, when present, are retained for an independent cross-check.
      raw_quality <- readxl::read_excel(
        quality_file,
        sheet = sheet,
        col_names = FALSE,
        skip = 2
      )
      item_matrix <- vapply(
        raw_quality[3:10],
        as_num,
        numeric(nrow(raw_quality))
      )
      result <- tibble::tibble(
        study_id_numeric = as_num(raw_quality[[1]]),
        quality_score = rowSums(item_matrix),
        selection_item3 = item_matrix[, 3],
        reported_total_score = as_num(raw_quality[[11]]),
        quality_design = sheet,
        complete_nos_items = rowSums(is.finite(item_matrix)) == 8
      ) |>
        dplyr::filter(is.finite(.data$study_id_numeric))
      if (any(!result$complete_nos_items)) {
        stop("Incomplete NOS item scores in sheet ", sheet, call. = FALSE)
      }
      result
    }
    quality <- dplyr::bind_rows(
      read_quality_sheet("NOS case-control"),
      read_quality_sheet("NOS cohort")
    )
    duplicated_quality_ids <- quality$study_id_numeric[duplicated(quality$study_id_numeric)]
    if (length(duplicated_quality_ids) > 0) {
      stop(
        "Duplicated Article ID values in NOS sheets: ",
        paste(unique(duplicated_quality_ids), collapse = ", "),
        call. = FALSE
      )
    }

    analysis_study_ids <- unique(as_num(as.character(dat_all$study_id)))
    missing_quality_ids <- setdiff(analysis_study_ids, quality$study_id_numeric)
    if (length(missing_quality_ids) > 0) {
      stop(
        "Eligible synthesis studies missing NOS scores: ",
        paste(missing_quality_ids, collapse = ", "),
        call. = FALSE
      )
    }
    reported_score_discrepancies <- quality |>
      dplyr::filter(
        is.finite(.data$reported_total_score),
        .data$reported_total_score != .data$quality_score
      )
    if (nrow(reported_score_discrepancies) > 0) {
      stop(
        "Recalculated NOS item scores disagree with stored numeric totals for Article ID: ",
        paste(reported_score_discrepancies$study_id_numeric, collapse = ", "),
        call. = FALSE
      )
    }

    quality_threshold <- 7
    if (sensitivity_mode == "s14_nos_ge7_tnd_adapted") {
      # Test-negative adaptation: case-control Selection item 3 ("community
      # controls") penalises the defining feature of the test-negative design,
      # so it is treated as met for studies whose extracted estimates all use
      # test-negative controls. Selection item 4 is unchanged (already awarded
      # to every case-control study). Cohort scores are unchanged.
      tnd_file <- file.path(
        root, "Data",
        "case_control_test_negative_classification.csv"
      )
      tnd <- readr::read_csv(tnd_file, show_col_types = FALSE)
      tnd_ids <- as_num(tnd$article_id[tnd$test_negative_design == "yes"])
      case_control_ids <- quality$study_id_numeric[quality$quality_design == "NOS case-control"]
      unclassified_ids <- setdiff(case_control_ids, as_num(tnd$article_id))
      if (length(unclassified_ids) > 0) {
        stop(
          "Case-control studies missing test-negative classification: ",
          paste(unclassified_ids, collapse = ", "),
          call. = FALSE
        )
      }
      quality <- quality |>
        dplyr::mutate(
          tnd_adapted = .data$quality_design == "NOS case-control" &
            .data$study_id_numeric %in% tnd_ids,
          original_quality_score = .data$quality_score,
          quality_score = dplyr::if_else(
            .data$tnd_adapted,
            .data$quality_score - .data$selection_item3 + 1,
            .data$quality_score
          )
        )
    }
    high_quality_ids <- quality$study_id_numeric[quality$quality_score >= quality_threshold]
    dat <- dat_all |>
      dplyr::filter(as_num(as.character(.data$study_id)) %in% high_quality_ids)
    if (sensitivity_mode == "s14_nos_ge7_tnd_adapted") {
      prefix <- "TableS14_TND_adapted_NOS_ge7"
      out_dir <- file.path(results_root(root), "TableS14")
      audit_analysis <- paste(
        "Table S14 sensitivity: restricted to studies with test-negative-adapted",
        "Newcastle-Ottawa Scale score >=7 (case-control Selection item 3 treated as met for test-negative studies)"
      )
      audit_rows_metric <- "rows_used_tnd_adapted_nos_ge7"
    } else {
      prefix <- "TableS13_NOS_ge7"
      out_dir <- file.path(results_root(root), "TableS13")
      audit_analysis <- "Table S13 sensitivity: restricted to studies with Newcastle-Ottawa Scale score >=7"
      audit_rows_metric <- "rows_used_nos_ge7"
    }
  } else if (sensitivity_mode == "s11_2016_2025") {
    dat <- dat_all |>
      dplyr::filter(
        (.data$period_original == 0 & as.character(.data$season) %in% sensitivity_pre_seasons) |
          .data$period_original == 1
      )
    prefix <- "TableS11_2016_2025"
    out_dir <- file.path(results_root(root), "TableS11")
    audit_analysis <- "Table S11 sensitivity: pre-pandemic restricted to 2016-2019 seasons plus pandemic/post-pandemic through 2025"
    audit_rows_metric <- "rows_used_2016_2025"
  } else if (sensitivity_mode == "s12_case_control") {
    dat <- dat_all |>
      dplyr::filter(.data$study_design == "Case-control study")
    stopifnot(nrow(dat) == 8091L, dplyr::n_distinct(dat$study_id) == 323L)
    prefix <- "TableS12_case_control"
    out_dir <- file.path(results_root(root), "TableS12")
    audit_analysis <- "Table S12: case-control studies reporting adjusted VE"
    audit_rows_metric <- "rows_used_case_control"
  } else {
    stop("Unsupported sensitivity mode: ", sensitivity_mode, call. = FALSE)
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_csv <- file.path(out_dir, paste0(prefix, ".csv"))
  out_xlsx <- file.path(out_dir, paste0(prefix, ".xlsx"))
  audit_csv <- file.path(out_dir, paste0(prefix, "_audit.csv"))
  adjusted_period_csv <- file.path(out_dir, paste0(prefix, "_adjusted_period_effects.csv"))
  adjusted_failed_csv <- file.path(out_dir, paste0(prefix, "_adjusted_failed_or_sparse.csv"))
  partial_csv <- file.path(out_dir, paste0(prefix, "_partial.csv"))
  progress_log <- file.path(out_dir, paste0(prefix, "_progress.log"))

  log_progress <- function(...) {
    msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
    message(msg)
    cat(msg, "\n", file = progress_log, append = TRUE)
  }

  write_partial <- function(rows) {
    if (length(rows) > 0) readr::write_excel_csv(dplyr::bind_rows(rows), partial_csv, na = "")
  }

  if (file.exists(progress_log)) file.remove(progress_log)
  if (file.exists(partial_csv)) file.remove(partial_csv)

  section_row <- function(section) {
    tibble::tibble(
      Variable = section, Period = section, `No. of studies` = section,
      `No. of datasets` = section, `No. of seasons` = section,
      `Pooled VE (%)` = section, `I^2` = section,
      `p value for heterogeneity` = section, `p value for effect` = section,
      `tau^2 study` = section, `tau^2 season` = section, `Model status` = section
    )
  }

  format_result_row <- function(variable, period, fit_obj) {
    s <- summarize_multilevel_fit(fit_obj)
    tibble::tibble(
      Variable = variable,
      Period = period,
      `No. of studies` = as.character(s$n_studies),
      `No. of datasets` = as.character(s$n_estimates),
      `No. of seasons` = as.character(s$n_seasons),
      `Pooled VE (%)` = fmt_ci(s$pooled_ve, s$pooled_ve_lower, s$pooled_ve_upper),
      `I^2` = ifelse(
        is.finite(s$i2_multilevel),
        paste0(fmt_num(s$i2_multilevel, 1), "%"),
        no_data
      ),
      `p value for heterogeneity` = fmt_p(s$p_heterogeneity),
      `p value for effect` = fmt_p(s$p_effect),
      `tau^2 study` = fmt_num(s$tau2_study, 6),
      `tau^2 season` = fmt_num(s$tau2_season, 6),
      `Model status` = s$status
    )
  }

  add_pair <- function(rows, section_name, variable, mask_fun) {
    for (period_value in c(0, 1)) {
      period_label <- ifelse(period_value == 0, "Pre", "Pan/post")
      sub <- dat[dat$period_original == period_value, , drop = FALSE]
      subgroup_mask <- mask_fun(sub)
      subgroup_mask[is.na(subgroup_mask)] <- FALSE
      sub <- sub[subgroup_mask, , drop = FALSE]
      log_progress(
        "Fitting ", table_label, " | section=", section_name, " | subgroup=", variable,
        " | period=", period_label, " | estimates=", nrow(sub),
        " | studies=", dplyr::n_distinct(sub$study_id),
        " | seasons=", dplyr::n_distinct(sub$season)
      )
      rows[[length(rows) + 1]] <- format_result_row(
        variable,
        period_label,
        fit_multilevel_meta(sub, aggregate_random_effect_cells = TRUE)
      )
      log_progress(
        "Finished ", table_label, " | section=", section_name, " | subgroup=", variable,
        " | period=", period_label, " | status=", rows[[length(rows)]]$`Model status`
      )
      write_partial(rows)
    }
    rows
  }

  rows <- list()
  rows[[length(rows) + 1]] <- section_row("Overall"); write_partial(rows)
  rows <- add_pair(rows, "Overall", "Overall", function(x) rep(TRUE, nrow(x)))

  rows[[length(rows) + 1]] <- section_row("Age"); write_partial(rows)
  rows <- add_pair(rows, "Age", "All ages", function(x) x$age_label == "All ages")
  rows <- add_pair(rows, "Age", "0-17 years", function(x) x$age_label == "0-17 years")
  rows <- add_pair(rows, "Age", "18-64 years", function(x) x$age_label == "18-64 years")
  rows <- add_pair(rows, "Age", "\u226565 years", function(x) x$age_label == "\u226565 years")

  rows[[length(rows) + 1]] <- section_row("Vaccine type"); write_partial(rows)
  rows <- add_pair(rows, "Vaccine type", "Trivalent", function(x) x$vaccine_label == "Trivalent")
  rows <- add_pair(rows, "Vaccine type", "Quadrivalent", function(x) x$vaccine_label == "Quadrivalent")
  rows <- add_pair(rows, "Vaccine type", "Other", function(x) x$vaccine_label == "Other")

  rows[[length(rows) + 1]] <- section_row("Influenza type"); write_partial(rows)
  rows <- add_pair(rows, "Influenza type", "IAV", function(x) x$pathogen == "IAV")
  rows <- add_pair(rows, "Influenza type", "A/H1N1", function(x) x$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)"))
  rows <- add_pair(rows, "Influenza type", "A/H3N2", function(x) x$pathogen_specific == "A(H3N2)")
  rows <- add_pair(rows, "Influenza type", "IBV", function(x) x$pathogen == "IBV")
  rows <- add_pair(rows, "Influenza type", "B/Yamagata", function(x) x$pathogen_specific == "B(Yamagata)")
  rows <- add_pair(rows, "Influenza type", "B/Victoria", function(x) x$pathogen_specific == "B(Victoria)")

  rows[[length(rows) + 1]] <- section_row("WHO region"); write_partial(rows)
  rows <- add_pair(rows, "WHO region", "EURO", function(x) x$who_label == "EURO")
  rows <- add_pair(rows, "WHO region", "AMRO", function(x) x$who_label == "AMRO")
  rows <- add_pair(rows, "WHO region", "WPRO", function(x) x$who_label == "WPRO")
  rows <- add_pair(rows, "WHO region", "SEARO", function(x) x$who_label == "SEARO")
  rows <- add_pair(rows, "WHO region", "AFRO", function(x) x$who_label == "AFRO")
  rows <- add_pair(rows, "WHO region", "EMRO", function(x) x$who_label == "EMRO")

  rows[[length(rows) + 1]] <- section_row("Clinical outcome"); write_partial(rows)
  rows <- add_pair(rows, "Clinical outcome", "Outpatient", function(x) x$outcome_label == "Outpatient")
  rows <- add_pair(rows, "Clinical outcome", "Inpatient", function(x) x$outcome_label == "Inpatient")
  rows <- add_pair(rows, "Clinical outcome", "Severe and Death", function(x) x$outcome_label %in% c("Severe", "Death", "Severe and Death"))

  table_out <- dplyr::bind_rows(rows)
  readr::write_excel_csv(table_out, out_csv, na = "")

  # Each table reports the intercept-only pooled estimates above together with
  # subgroup-specific adjusted period effects. The adjusted model follows the
  # primary specification but uses the restricted analysis set.
  model_dat <- prepare_period_data(dat)

  fit_adjusted_period_model <- fit_period_model

  adjusted_period_effect_row <- function(model, panel, subgroup, sub) {
    term <- "periodPandemic/Post-Pandemic"
    idx <- match(term, rownames(model$b))
    if (is.na(idx)) stop("Period coefficient missing for ", panel, " - ", subgroup)
    beta <- as.numeric(model$b[idx, 1])
    ci_lb <- model$ci.lb[idx]
    ci_ub <- model$ci.ub[idx]
    tibble::tibble(
      Panel = panel,
      Subgroup = subgroup,
      No_of_studies = dplyr::n_distinct(sub$study_id),
      No_of_estimates = nrow(sub),
      No_of_seasons = dplyr::n_distinct(sub$season),
      Coefficient_logOR = fmt_num(beta),
      SE = fmt_num(model$se[idx]),
      t_statistic = fmt_num(beta / model$se[idx]),
      p_value = fmt_p(model$pval[idx]),
      CI_95_logOR = paste0("(", fmt_num(ci_lb), ", ", fmt_num(ci_ub), ")"),
      VE_change_percent = (1 - exp(beta)) * 100,
      VE_change_lower_percent = (1 - exp(ci_ub)) * 100,
      VE_change_upper_percent = (1 - exp(ci_lb)) * 100,
      Converted_VE_change_percent = fmt_ci(
        (1 - exp(beta)) * 100,
        (1 - exp(ci_ub)) * 100,
        (1 - exp(ci_lb)) * 100
      )
    )
  }

  adjusted_specs <- lapply(period_model_specs(), function(spec) {
    spec$cats <- setdiff(spec$cats, "Unknown")
    spec
  })

  adjusted_influenza_mask <- influenza_type_mask

  adjusted_rows <- list()
  adjusted_failed <- list()
  for (spec in adjusted_specs) {
    for (cat in spec$cats) {
      sub <- if (is.na(spec$col)) {
        model_dat
      } else if (spec$panel == "Influenza type") {
        model_dat[adjusted_influenza_mask(model_dat, cat), , drop = FALSE]
      } else {
        subgroup_mask <- !is.na(model_dat[[spec$col]]) & as.character(model_dat[[spec$col]]) == cat
        model_dat[subgroup_mask, , drop = FALSE]
      }
      model_name <- paste(spec$panel, cat, sep = " - ")
      log_progress(
        "Fitting ", table_label, " adjusted period model | model=", model_name,
        " | estimates=", nrow(sub),
        " | studies=", dplyr::n_distinct(sub$study_id),
        " | seasons=", dplyr::n_distinct(sub$season)
      )
      sparse <- nrow(sub) <= 2 || dplyr::n_distinct(sub$period) < 2 ||
        dplyr::n_distinct(sub$study_id) < 2 || dplyr::n_distinct(sub$season) < 2
      if (sparse) {
        adjusted_failed[[length(adjusted_failed) + 1]] <- tibble::tibble(
          Panel = spec$panel, Subgroup = cat,
          No_of_estimates = nrow(sub),
          No_of_studies = dplyr::n_distinct(sub$study_id),
          No_of_seasons = dplyr::n_distinct(sub$season),
          Note = "Need both periods, >2 estimates, >=2 study IDs, and >=2 seasons"
        )
        next
      }
      fit <- tryCatch(fit_adjusted_period_model(sub, spec$covars), error = function(e) e)
      if (inherits(fit, "error")) {
        adjusted_failed[[length(adjusted_failed) + 1]] <- tibble::tibble(
          Panel = spec$panel, Subgroup = cat,
          No_of_estimates = nrow(sub),
          No_of_studies = dplyr::n_distinct(sub$study_id),
          No_of_seasons = dplyr::n_distinct(sub$season),
          Note = conditionMessage(fit)
        )
        log_progress("Failed ", table_label, " adjusted period model | model=", model_name, " | reason=", conditionMessage(fit))
        next
      }
      adjusted_rows[[length(adjusted_rows) + 1]] <- adjusted_period_effect_row(
        fit$model, spec$panel, cat, sub
      )
      log_progress(
        "Finished ", table_label, " adjusted period model | model=", model_name,
        " | sufficient-statistic rows=", fit$aggregated_rows,
        " | residual df=", fit$original_df
      )
    }
  }

  adjusted_period_effects <- dplyr::bind_rows(adjusted_rows)
  adjusted_failed_models <- dplyr::bind_rows(adjusted_failed)
  readr::write_excel_csv(adjusted_period_effects, adjusted_period_csv, na = "")
  if (nrow(adjusted_failed_models) > 0) {
    readr::write_excel_csv(adjusted_failed_models, adjusted_failed_csv, na = "")
  } else if (file.exists(adjusted_failed_csv)) {
    file.remove(adjusted_failed_csv)
  }

  audit <- tibble::tibble(
    metric = c(
      "analysis", "input_file", "source_rows", "source_logor_eligible_rows",
      "source_logor_excluded_rows", audit_rows_metric, "effect_measure",
      "pooled_model", "adjusted_period_model", "random_effects", "estimation",
      "computational_equivalence", "output_rows", "adjusted_period_output_rows"
    ),
    value = c(
      audit_analysis,
      source_audit$input_file, source_audit$source_rows,
      source_audit$source_logor_eligible_rows, source_audit$source_logor_excluded_rows,
      nrow(dat), paste(
        "adjusted VE and 95% CI converted to the log of the VE-defining",
        "effect ratio (OR, RR, HR, or IRR as applicable)"
      ),
      "separate intercept-only metafor::rma.mv multilevel random-effects meta-analysis by period",
      "subgroup-specific multivariable metafor::rma.mv meta-regression",
      "random intercepts for study_id and season",
      "restricted maximum likelihood (REML), test='t'; model-based pre-aggregation residual df",
      "inverse-variance sufficient-statistic aggregation within identical random-effect/model-moderator cells",
      nrow(table_out), nrow(adjusted_period_effects)
    )
  )
  if (sensitivity_mode %in% nos_modes) {
    audit <- dplyr::bind_rows(
      audit,
      tibble::tibble(
        metric = c(
          "quality_file", "quality_instrument", "quality_score_method",
          "numeric_total_scores_cross_checked", "quality_threshold",
          "studies_with_quality_score", "studies_nos_ge7",
          "eligible_studies_excluded_below_threshold",
          "eligible_rows_excluded_below_threshold"
        ),
        value = c(
          normalizePath(quality_file, winslash = "/", mustWork = TRUE),
          "design-specific Newcastle-Ottawa Scale sheets for case-control and cohort studies",
          "sum of the eight NOS item scores in columns C:J; formula-based Total Scores cells were not assumed to have cached values",
          sum(is.finite(quality$reported_total_score)),
          ">=7 of 9",
          nrow(quality),
          sum(quality$quality_score >= quality_threshold),
          dplyr::n_distinct(dat_all$study_id) - dplyr::n_distinct(dat$study_id),
          nrow(dat_all) - nrow(dat)
        )
      )
    )
    if (sensitivity_mode == "s14_nos_ge7_tnd_adapted") {
      audit <- dplyr::bind_rows(
        audit,
        tibble::tibble(
          metric = c(
            "tnd_classification_file", "case_control_studies_test_negative",
            "case_control_studies_not_test_negative",
            "studies_original_nos_ge7", "studies_moved_to_ge7_by_tnd_adaptation"
          ),
          value = c(
            normalizePath(tnd_file, winslash = "/", mustWork = TRUE),
            sum(quality$tnd_adapted),
            sum(quality$quality_design == "NOS case-control" & !quality$tnd_adapted),
            sum(quality$original_quality_score >= quality_threshold),
            sum(quality$original_quality_score < quality_threshold &
              quality$quality_score >= quality_threshold)
          )
        )
      )
    }
  }
  readr::write_excel_csv(audit, audit_csv, na = "")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_label, gridLines = FALSE)
  openxlsx::addWorksheet(wb, "Adjusted period effects", gridLines = FALSE)
  openxlsx::addWorksheet(wb, "Audit", gridLines = FALSE)
  openxlsx::writeData(wb, sheet_label, table_out)
  openxlsx::writeData(wb, "Adjusted period effects", adjusted_period_effects)
  openxlsx::writeData(wb, "Audit", audit)
  openxlsx::setColWidths(wb, sheet_label, cols = 1:ncol(table_out), widths = "auto")
  openxlsx::setColWidths(
    wb, "Adjusted period effects", cols = 1:ncol(adjusted_period_effects), widths = "auto"
  )
  openxlsx::setColWidths(wb, "Audit", cols = 1:2, widths = "auto")
  openxlsx::freezePane(wb, sheet_label, firstRow = TRUE)
  openxlsx::freezePane(wb, "Adjusted period effects", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  message("Wrote: ", out_csv)
  message("Wrote: ", out_xlsx)
  message("Wrote: ", audit_csv)
  message("Wrote: ", adjusted_period_csv)
  if (file.exists(adjusted_failed_csv)) message("Wrote: ", adjusted_failed_csv)
  message("Progress log: ", progress_log)
}

single_season <- function() {
  check_required_packages(extra = c("openxlsx", "readxl", "tidyr"))

  root <- get_project_root()
  source_dat <- read_source_verified_analysis_data(root)
  source_audit <- attr(source_dat, "source_audit")
  season_info <- classify_single_season(source_dat$season)

  dat <- dplyr::bind_cols(source_dat, season_info) |>
    dplyr::filter(.data$is_single_season) |>
    dplyr::mutate(
      # Northern cross-year YYYY-YYYY+1 and corresponding Southern/calendar
      # YYYY+1 observations share a harmonized global influenza-cycle level.
      influenza_cycle_year = dplyr::if_else(
        .data$season_convention == "cross-year season",
        .data$season_end_year,
        .data$season_start_year
      ),
      season = factor(sprintf("influenza-cycle:%04d", .data$influenza_cycle_year)),
      yi_model = .data$yi_log_or,
      vi_model = .data$vi_log_or,
      period = factor(
        .data$period_original,
        levels = c(0, 1),
        labels = c("Pre-pandemic", "Pandemic/Post-Pandemic")
      ),
      age_display = factor(
        .data$age_label,
        levels = c(
          "All ages", "0-17 years", "18-64 years", "≥65 years",
          "Mixed age groups"
        )
      ),
      vaccine_type_display = factor(
        .data$vaccine_label,
        levels = c("Unknown", "Trivalent", "Quadrivalent", "Other")
      ),
      pathogen_display = factor(
        .data$pathogen_label,
        levels = c("Unknown", "IAV", "IBV")
      ),
      region_short = factor(
        .data$who_label,
        levels = c("Multi-region", "AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")
      ),
      outcome_display = factor(
        dplyr::case_when(
          .data$outcome_label %in% c("Severe", "Death", "Severe and Death") ~
            "Severe and Death",
          TRUE ~ .data$outcome_label
        ),
        levels = c("Unknown", "Outpatient", "Inpatient", "Severe and Death")
      )
    ) |>
    dplyr::filter(
      is.finite(.data$yi_model), is.finite(.data$vi_model), .data$vi_model > 0
    )

  out_dir <- file.path(results_root(root), "TableS15")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_csv <- file.path(out_dir, "TableS15_harmonized_single_season_results.csv")
  out_xlsx <- file.path(out_dir, "TableS15_harmonized_single_season_results.xlsx")
  out_audit <- file.path(out_dir, "TableS15_harmonized_single_season_audit.csv")
  out_label_audit <- file.path(out_dir, "TableS15_harmonized_cycle_label_audit.csv")

  fmt_signed_ci <- function(est, lo, hi) {
    if (any(!is.finite(c(est, lo, hi)))) return(no_data)
    sprintf("%+.1f (%+.1f, %+.1f)", est, lo, hi)
  }

  fmt_ve_ci <- function(est, lo, hi) {
    if (any(!is.finite(c(est, lo, hi)))) return(no_data)
    sprintf("%.1f (%.1f, %.1f)", est, lo, hi)
  }

  fit_adjusted_period <- function(sub, candidate_covariates) {
    sub <- sub |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
    if (
      nrow(sub) <= 2L || dplyr::n_distinct(sub$period) < 2L ||
        dplyr::n_distinct(sub$study_id) < 2L || dplyr::n_distinct(sub$season) < 2L
    ) {
      return(list(status = "insufficient_data", fit = NULL))
    }

    active <- candidate_covariates[vapply(candidate_covariates, function(variable) {
      variable %in% names(sub) && dplyr::n_distinct(sub[[variable]], na.rm = TRUE) > 1L
    }, logical(1))]
    # Drop empty levels so that, when the preferred reference level is absent
    # from a subgroup, the first available level becomes the reference.
    for (variable in active) sub[[variable]] <- droplevels(sub[[variable]])
    formula <- stats::reformulate(c("period", active))
    aggregation_columns <- c("study_id", "season", "period", active)
    model_data <- sub |>
      dplyr::group_by(dplyr::across(dplyr::all_of(aggregation_columns))) |>
      dplyr::summarise(
        yi_model = sum(.data$yi_model / .data$vi_model) / sum(1 / .data$vi_model),
        vi_model = 1 / sum(1 / .data$vi_model),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )

    fit <- tryCatch(
      metafor::rma.mv(
        yi = yi_model,
        V = vi_model,
        mods = formula,
        random = list(~ 1 | study_id, ~ 1 | season),
        data = model_data,
        method = "REML",
        test = "t",
        sparse = TRUE
      ),
      error = function(error) error
    )
    if (inherits(fit, "error")) {
      return(list(status = conditionMessage(fit), fit = NULL))
    }

    original_df <- nrow(sub) - ncol(fit$X)
    term <- "periodPandemic/Post-Pandemic"
    if (!is.finite(original_df) || original_df <= 0 || !term %in% rownames(fit$b)) {
      return(list(status = "period_coefficient_not_estimable", fit = fit))
    }
    index <- match(term, rownames(fit$b))
    beta <- as.numeric(fit$b[index, 1])
    se <- fit$se[index]
    critical <- stats::qt(0.975, original_df)
    lower_log <- beta - critical * se
    upper_log <- beta + critical * se
    p_value <- 2 * stats::pt(-abs(beta / se), df = original_df)

    list(
      status = "ok",
      fit = fit,
      beta = beta,
      se = se,
      lower_log = lower_log,
      upper_log = upper_log,
      effect = (1 - exp(beta)) * 100,
      lower = (1 - exp(upper_log)) * 100,
      upper = (1 - exp(lower_log)) * 100,
      p_value = p_value,
      original_df = original_df,
      active_covariates = paste(active, collapse = "; "),
      aggregated_rows = nrow(model_data)
    )
  }

  mask_all <- function(data) rep(TRUE, nrow(data))
  mask_level <- function(variable, value) {
    force(variable)
    force(value)
    function(data) !is.na(data[[variable]]) & as.character(data[[variable]]) == value
  }
  mask_h1n1 <- function(data) data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)")
  mask_h3n2 <- function(data) data$pathogen_specific == "A(H3N2)"
  mask_yamagata <- function(data) data$pathogen_specific == "B(Yamagata)"
  mask_victoria_combined <- function(data) {
    (data$period_original == 0 & data$pathogen_specific == "B(Victoria)") |
      (data$period_original == 1 & data$pathogen == "IBV")
  }
  mask_severe <- function(data) data$outcome_label %in% c("Severe", "Death", "Severe and Death")

  specs <- list(
    list("Overall", "Overall", mask_all,
         c("age_display", "vaccine_type_display", "pathogen_display", "region_short", "outcome_display")),
    list("Age group", "≥6 mo", mask_level("age_label", "All ages"),
         c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")),
    list("Age group", "6 mo to 17 y", mask_level("age_label", "0-17 years"),
         c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")),
    list("Age group", "18–64 y", mask_level("age_label", "18-64 years"),
         c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")),
    list("Age group", "≥65 y", mask_level("age_label", "≥65 years"),
         c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")),
    list("Vaccine type", "Trivalent", mask_level("vaccine_label", "Trivalent"),
         c("age_display", "pathogen_display", "region_short", "outcome_display")),
    list("Vaccine type", "Quadrivalent", mask_level("vaccine_label", "Quadrivalent"),
         c("age_display", "pathogen_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "IAV", mask_level("pathogen", "IAV"),
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "A(H1N1)pdm09", mask_h1n1,
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "A(H3N2)", mask_h3n2,
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "IBV", mask_level("pathogen", "IBV"),
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "B/Yamagata", mask_yamagata,
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("Influenza type/subtype", "B/Victoria", mask_victoria_combined,
         c("age_display", "vaccine_type_display", "region_short", "outcome_display")),
    list("WHO region", "EURO", mask_level("who_label", "EURO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("WHO region", "AMRO", mask_level("who_label", "AMRO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("WHO region", "WPRO", mask_level("who_label", "WPRO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("WHO region", "SEARO", mask_level("who_label", "SEARO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("WHO region", "AFRO", mask_level("who_label", "AFRO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("WHO region", "EMRO", mask_level("who_label", "EMRO"),
         c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")),
    list("Clinical outcome", "Outpatient", mask_level("outcome_label", "Outpatient"),
         c("age_display", "vaccine_type_display", "pathogen_display", "region_short")),
    list("Clinical outcome", "Inpatient", mask_level("outcome_label", "Inpatient"),
         c("age_display", "vaccine_type_display", "pathogen_display", "region_short")),
    list("Clinical outcome", "Severe illness/death", mask_severe,
         c("age_display", "vaccine_type_display", "pathogen_display", "region_short"))
  )

  result_rows <- list()
  model_audit_rows <- list()

  for (spec in specs) {
    section <- spec[[1]]
    group <- spec[[2]]
    mask <- spec[[3]](dat)
    mask[is.na(mask)] <- FALSE
    sub <- dat[mask, , drop = FALSE]
    message("Fitting harmonized single-season table: ", section, " - ", group,
            " (k=", nrow(sub), ")")

    pooled <- list()
    summaries <- list()
    for (period_value in c(0, 1)) {
      period_sub <- sub[sub$period_original == period_value, , drop = FALSE]
      pooled[[period_value + 1L]] <- fit_multilevel_meta(
        period_sub, aggregate_random_effect_cells = TRUE
      )
      summaries[[period_value + 1L]] <- summarize_multilevel_fit(
        pooled[[period_value + 1L]]
      )
    }
    pre <- summaries[[1]]
    post <- summaries[[2]]
    adjusted <- fit_adjusted_period(sub, spec[[4]])

    result_rows[[length(result_rows) + 1L]] <- tibble::tibble(
      Section = section,
      Group = group,
      Pre_studies = pre$n_studies,
      Post_studies = post$n_studies,
      Pre_estimates = pre$n_estimates,
      Post_estimates = post$n_estimates,
      Pre_cycles = pre$n_seasons,
      Post_cycles = post$n_seasons,
      Pre_I2_percent = pre$i2_multilevel,
      Post_I2_percent = post$i2_multilevel,
      Pre_pooled_VE_percent = pre$pooled_ve,
      Pre_pooled_VE_lower = pre$pooled_ve_lower,
      Pre_pooled_VE_upper = pre$pooled_ve_upper,
      Post_pooled_VE_percent = post$pooled_ve,
      Post_pooled_VE_lower = post$pooled_ve_lower,
      Post_pooled_VE_upper = post$pooled_ve_upper,
      Adjusted_period_effect_percent = ifelse(adjusted$status == "ok", adjusted$effect, NA_real_),
      Adjusted_period_effect_lower = ifelse(adjusted$status == "ok", adjusted$lower, NA_real_),
      Adjusted_period_effect_upper = ifelse(adjusted$status == "ok", adjusted$upper, NA_real_),
      Adjusted_period_p_value = ifelse(adjusted$status == "ok", adjusted$p_value, NA_real_),
      Evidence_studies = paste0(pre$n_studies, " | ", post$n_studies),
      Evidence_estimates = paste0(pre$n_estimates, " | ", post$n_estimates),
      I2 = paste0(
        ifelse(is.finite(pre$i2_multilevel), paste0(sprintf("%.1f", pre$i2_multilevel), "%"), no_data),
        " | ",
        ifelse(is.finite(post$i2_multilevel), paste0(sprintf("%.1f", post$i2_multilevel), "%"), no_data)
      ),
      Pre_pooled_VE = fmt_ve_ci(pre$pooled_ve, pre$pooled_ve_lower, pre$pooled_ve_upper),
      Post_pooled_VE = fmt_ve_ci(post$pooled_ve, post$pooled_ve_lower, post$pooled_ve_upper),
      Adjusted_period_effect = if (adjusted$status == "ok") {
        fmt_signed_ci(adjusted$effect, adjusted$lower, adjusted$upper)
      } else {
        no_data
      },
      Adjusted_model_status = adjusted$status
    )

    model_audit_rows[[length(model_audit_rows) + 1L]] <- tibble::tibble(
      Section = section,
      Group = group,
      Source_estimates = nrow(sub),
      Studies = dplyr::n_distinct(sub$study_id),
      Harmonized_cycles = dplyr::n_distinct(sub$season),
      Active_covariates = ifelse(adjusted$status == "ok", adjusted$active_covariates, no_data),
      Aggregated_rows = ifelse(adjusted$status == "ok", adjusted$aggregated_rows, NA_integer_),
      Residual_df = ifelse(adjusted$status == "ok", adjusted$original_df, NA_real_),
      Model_status = adjusted$status
    )
  }

  results <- dplyr::bind_rows(result_rows)
  model_audit <- dplyr::bind_rows(model_audit_rows)

  label_audit <- dplyr::bind_cols(
    source_dat |>
      dplyr::transmute(
        study_id = as.character(.data$study_id),
        period_original = .data$period_original
      ),
    season_info
  ) |>
    dplyr::mutate(
      influenza_cycle_year = dplyr::if_else(
        .data$is_single_season & .data$season_convention == "cross-year season",
        .data$season_end_year,
        dplyr::if_else(.data$is_single_season, .data$season_start_year, NA_integer_)
      ),
      harmonized_cycle = dplyr::if_else(
        .data$is_single_season,
        sprintf("influenza-cycle:%04d", .data$influenza_cycle_year),
        NA_character_
      )
    ) |>
    dplyr::count(
      .data$reported_study_period, .data$is_single_season,
      .data$season_convention, .data$harmonized_cycle,
      name = "No_of_estimates"
    ) |>
    dplyr::arrange(dplyr::desc(.data$No_of_estimates), .data$reported_study_period)

  analysis_audit <- tibble::tibble(
    Metric = c(
      "input_file", "source_eligible_estimates", "included_single_season_estimates",
      "excluded_multiseason_estimates", "included_studies", "harmonized_cycle_levels",
      "season_mapping", "model", "random_effects", "estimation",
      "B/Victoria_definition"
    ),
    Value = c(
      source_audit$input_file,
      nrow(source_dat),
      nrow(dat),
      nrow(source_dat) - nrow(dat),
      dplyr::n_distinct(dat$study_id),
      dplyr::n_distinct(dat$season),
      paste(
        "Northern cross-year YYYY-YYYY+1 mapped to ending year YYYY+1;",
        "corresponding Southern/calendar-year YYYY+1 mapped to the same global influenza cycle"
      ),
      "period-specific intercept-only models and subgroup-specific adjusted period meta-regressions",
      "crossed random intercepts for study ID and harmonized global influenza cycle",
      "REML; test='t'; pre-aggregation source-estimate residual df",
      "pre-pandemic: explicit B/Victoria; pandemic/post-pandemic: all IBV"
    )
  )

  readr::write_excel_csv(results, out_csv, na = "")
  readr::write_excel_csv(
    dplyr::bind_rows(
      analysis_audit |>
        dplyr::transmute(Section = "Analysis", Item = .data$Metric, Value = as.character(.data$Value)),
      model_audit |>
        dplyr::mutate(dplyr::across(-c("Section", "Group"), as.character)) |>
        tidyr::pivot_longer(-c("Section", "Group"), names_to = "Item", values_to = "Value") |>
        dplyr::mutate(Section = paste(.data$Section, .data$Group, sep = " - ")) |>
        dplyr::select("Section", "Item", Value = "Value")
    ),
    out_audit,
    na = ""
  )
  readr::write_excel_csv(label_audit, out_label_audit, na = "")

  formatted <- results |>
    dplyr::select(
      "Section", "Group", "Evidence_studies", "Evidence_estimates", "I2",
      "Pre_pooled_VE", "Post_pooled_VE", "Adjusted_period_effect",
      "Adjusted_period_p_value"
    )
  workbook_data <- list(
    "Formatted table" = formatted,
    "Numeric results" = results,
    "Model audit" = model_audit,
    "Analysis audit" = analysis_audit,
    "Label audit" = label_audit
  )
  wb <- openxlsx::createWorkbook()
  for (sheet in names(workbook_data)) {
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    openxlsx::writeData(wb, sheet, workbook_data[[sheet]])
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(
      wb, sheet, cols = seq_len(ncol(workbook_data[[sheet]])), widths = "auto"
    )
  }
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  message("Wrote: ", out_csv)
  message("Wrote: ", out_xlsx)
  message("Wrote: ", out_audit)
  message("Wrote: ", out_label_audit)
}

cluster_robust <- function() {
  check_required_packages("clubSandwich")

  root <- get_project_root()
  out_dir <- file.path(results_root(root), "TableS16")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dat <- prepare_analysis_data(root) |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season = droplevels(factor(.data$season)),
      pathogen_specific_display = factor(
        dplyr::case_when(
          .data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)") ~ "A/H1N1",
          .data$pathogen_specific == "A(H3N2)" ~ "A/H3N2",
          .data$pathogen_specific == "B(Victoria)" ~ "B/Victoria",
          TRUE ~ NA_character_
        ),
        levels = c("A/H1N1", "A/H3N2", "B/Victoria")
      ),
      study_period_id = interaction(
        .data$study_id, .data$season, drop = TRUE, lex.order = TRUE
      )
    )

  specs <- list(
    list(
      panel = "Overall", col = NA_character_, cats = "Overall",
      covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
    ),
    list(
      panel = "Age", col = "age_display",
      cats = c("All ages", "0-17 years", "18-64 years", "≥65 years"),
      covars = c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
    ),
    list(
      panel = "Vaccine type", col = "vaccine_type_display",
      cats = c("Trivalent", "Quadrivalent"),
      covars = c("age_display", "pathogen_display", "region_short", "outcome_display")
    ),
    list(
      panel = "Influenza type", col = "pathogen_display",
      cats = c("IAV", "A/H1N1", "A/H3N2", "IBV", "B/Victoria"),
      covars = c("age_display", "vaccine_type_display", "region_short", "outcome_display")
    ),
    list(
      panel = "WHO region", col = "region_short",
      cats = c("EURO", "AMRO", "WPRO", "SEARO"),
      covars = c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")
    ),
    list(
      panel = "Clinical outcome", col = "outcome_display",
      cats = c("Outpatient", "Inpatient", "Severe and Death"),
      covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short")
    )
  )

  influenza_type_mask <- function(data, category) {
    if (category == "B/Victoria") {
      return(
        (
          data$period_original == 0 &
            !is.na(data$pathogen_specific_display) &
            as.character(data$pathogen_specific_display) == "B/Victoria"
        ) |
          (
            data$period_original == 1 &
              !is.na(data$pathogen_display) &
              as.character(data$pathogen_display) == "IBV"
          )
      )
    }
    if (category %in% c("A/H1N1", "A/H3N2")) {
      return(
        !is.na(data$pathogen_specific_display) &
          as.character(data$pathogen_specific_display) == category
      )
    }
    !is.na(data$pathogen_display) & as.character(data$pathogen_display) == category
  }

  subset_spec <- function(data, spec, category) {
    if (is.na(spec$col)) return(data)
    if (spec$panel == "Influenza type") {
      return(data[influenza_type_mask(data, category), , drop = FALSE])
    }
    keep <- !is.na(data[[spec$col]]) & as.character(data[[spec$col]]) == category
    data[keep, , drop = FALSE]
  }

  fit_cr2 <- function(sub, covariates) {
    sub <- sub |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season)),
        study_period_id = droplevels(factor(.data$study_period_id))
      )
    active <- active_covariates(sub, covariates)
    # Drop empty levels so that, when the preferred reference level is absent
    # from a subgroup, the first available level becomes the reference.
    for (x in active) sub[[x]] <- droplevels(sub[[x]])
    mods_formula <- stats::reformulate(c("period", active))

    # These are inverse-variance sufficient statistics for rows with identical
    # fixed- and random-effect design vectors. No source estimate is selected or
    # discarded by this computational aggregation.
    grouping <- c("study_id", "study_period_id", "period", active)
    model_data <- sub |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::summarise(
        yi_model = sum(.data$yi_model / .data$vi_model) / sum(1 / .data$vi_model),
        vi_model = 1 / sum(1 / .data$vi_model),
        source_estimates = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        study_period_id = droplevels(factor(.data$study_period_id))
      )

    fit <- metafor::rma.mv(
      yi = yi_model,
      V = vi_model,
      mods = mods_formula,
      random = list(~ 1 | study_id, ~ 1 | study_period_id),
      data = model_data,
      method = "REML",
      test = "t",
      sparse = TRUE
    )
    vcov_cr2 <- clubSandwich::vcovCR(
      fit, cluster = model_data$study_id, type = "CR2"
    )
    coefficient_tests <- clubSandwich::coef_test(
      fit, vcov = vcov_cr2, test = "Satterthwaite"
    )

    list(
      fit = fit,
      tests = coefficient_tests,
      model_data = model_data,
      active_covariates = active,
      source_data = sub
    )
  }

  extract_coefficients <- function(fitted, panel, subgroup) {
    tests <- fitted$tests
    critical <- stats::qt(0.975, tests$df_Satt)
    ci_lb <- tests$beta - critical * tests$SE
    ci_ub <- tests$beta + critical * tests$SE
    tibble::tibble(
      Panel = panel,
      Subgroup = subgroup,
      Term = rownames(tests),
      Coefficient_logR = as.numeric(tests$beta),
      SE_CR2 = as.numeric(tests$SE),
      t_statistic = as.numeric(tests$tstat),
      Satterthwaite_df = as.numeric(tests$df_Satt),
      p_value_CR2 = as.numeric(tests$p_Satt),
      CI_lower_logR = as.numeric(ci_lb),
      CI_upper_logR = as.numeric(ci_ub),
      No_of_studies = dplyr::n_distinct(fitted$source_data$study_id),
      No_of_estimates = nrow(fitted$source_data),
      No_of_reported_periods = dplyr::n_distinct(fitted$source_data$season),
      No_of_study_periods = dplyr::n_distinct(fitted$source_data$study_period_id),
      Sufficient_statistic_rows = nrow(fitted$model_data),
      Active_covariates = paste(fitted$active_covariates, collapse = "; "),
      Working_random_effects = "study and study-within-reported-period random intercepts",
      Robust_inference = "study-clustered CR2 covariance; Satterthwaite df"
    )
  }

  extract_period_effect <- function(coefficient_table) {
    term <- "periodPandemic/post-pandemic"
    row <- coefficient_table[coefficient_table$Term == term, , drop = FALSE]
    if (nrow(row) != 1L) stop("Expected one estimable period coefficient.")
    row |>
      dplyr::mutate(
        Relative_period_effect_percent = (1 - exp(.data$Coefficient_logR)) * 100,
        Relative_period_effect_lower_percent = (1 - exp(.data$CI_upper_logR)) * 100,
        Relative_period_effect_upper_percent = (1 - exp(.data$CI_lower_logR)) * 100
      )
  }

  period_results <- list()
  failed_results <- list()

  for (spec in specs) {
    for (category in spec$cats) {
      sub <- subset_spec(dat, spec, category)
      descriptor <- paste(spec$panel, category, sep = " | ")
      message("Fitting study-clustered CR2 model: ", descriptor, " (k=", nrow(sub), ")")
      if (
        nrow(sub) <= 2L || dplyr::n_distinct(sub$period) < 2L ||
          dplyr::n_distinct(sub$study_id) < 2L ||
          dplyr::n_distinct(sub$study_period_id) < 2L
      ) {
        failed_results[[length(failed_results) + 1L]] <- tibble::tibble(
          Panel = spec$panel,
          Subgroup = category,
          No_of_estimates = nrow(sub),
          No_of_studies = dplyr::n_distinct(sub$study_id),
          Note = "Need both periods, >2 estimates, >=2 studies, and >=2 study-period cells"
        )
        next
      }

      fitted <- tryCatch(fit_cr2(sub, spec$covars), error = function(e) e)
      if (inherits(fitted, "error")) {
        failed_results[[length(failed_results) + 1L]] <- tibble::tibble(
          Panel = spec$panel,
          Subgroup = category,
          No_of_estimates = nrow(sub),
          No_of_studies = dplyr::n_distinct(sub$study_id),
          Note = conditionMessage(fitted)
        )
        next
      }

      coefficients <- extract_coefficients(fitted, spec$panel, category)
      period_results[[length(period_results) + 1L]] <- extract_period_effect(coefficients)
    }
  }

  cr2_period_effects <- dplyr::bind_rows(period_results)
  failed_models <- dplyr::bind_rows(failed_results)

  audit <- tibble::tibble(
    Metric = c(
      "Eligible effect estimates retained",
      "Independent study clusters",
      "Reported-period levels",
      "Study-period working levels",
      "Effect-estimate selection",
      "Effect scale",
      "Working model",
      "Robust covariance",
      "Degrees of freedom",
      "Reason working period is nested rather than crossed"
    ),
    Value = c(
      nrow(dat),
      dplyr::n_distinct(dat$study_id),
      dplyr::n_distinct(dat$season),
      dplyr::n_distinct(dat$study_period_id),
      "None; all eligible effect estimates retained",
      "log of the reported adjusted relative effect measure",
      "REML meta-regression with study and study-within-reported-period random intercepts",
      "CR2 clustered by study_id",
      "coefficient-specific Satterthwaite approximation",
      paste(
        "clubSandwich CR2 requires all working random effects to be nested within cluster;",
        "the primary crossed reported-period effect spans study clusters"
      )
    )
  )

  period_file <- file.path(out_dir, "TableS16_study_clustered_CR2_period_effects.csv")
  audit_file <- file.path(out_dir, "TableS16_study_clustered_CR2_analysis_audit.csv")
  failed_file <- file.path(out_dir, "TableS16_study_clustered_CR2_failed_models.csv")
  workbook_file <- file.path(out_dir, "TableS16_study_clustered_CR2_outputs.xlsx")

  readr::write_excel_csv(cr2_period_effects, period_file, na = "")
  readr::write_excel_csv(audit, audit_file, na = "")
  if (nrow(failed_models) > 0L) readr::write_excel_csv(failed_models, failed_file, na = "")

  sheets <- list(
    "CR2 period effects" = cr2_period_effects,
    "Analysis audit" = audit
  )
  if (nrow(failed_models) > 0L) sheets[["Failed models"]] <- failed_models
  add_excel_sheets(workbook_file, sheets)

  message("Wrote: ", period_file)
  message("Wrote: ", workbook_file)
}

reported_lineage <- function() {
  root <- get_project_root()
  out_dir <- file.path(results_root(root), "TableS17")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Attach the source Excel row. read_source_verified_analysis_data() returns
  # exactly the rows marked "Use in logOR synthesis" = Yes, in workbook order.
  review_file <- file.path(root, "Data", "review_data.xlsx")
  raw <- readxl::read_excel(review_file, sheet = "Review data", col_types = "text")
  eligible_rows <- which(
    tolower(trimws(as.character(raw$`Use in logOR synthesis`))) == "yes"
  ) + 1L

  dat <- prepare_analysis_data(root)
  if (length(eligible_rows) != nrow(dat)) {
    stop("Excel-row mapping failed: eligible=", length(eligible_rows),
         ", analysed=", nrow(dat), call. = FALSE)
  }
  dat$excel_row <- eligible_rows

  lineage <- readr::read_csv(
    file.path(root, "Data", "b_lineage_reported.csv"),
    show_col_types = FALSE
  )
  check <- merge(
    lineage[, c("excel_row", "Article ID", "VE")],
    data.frame(excel_row = dat$excel_row, study_id = as.character(dat$study_id),
               ve = dat$reported_ve_percent / 100),
    by = "excel_row"
  )
  if (any(as.character(check$`Article ID`) != check$study_id) ||
      any(abs(check$VE - check$ve) > 1e-6)) {
    stop("Reported-lineage file does not match review_data.xlsx rows.", call. = FALSE)
  }

  dat <- dat |>
    dplyr::left_join(
      lineage[, c("excel_row", "Pathogen_specific_reported")], by = "excel_row"
    ) |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season = droplevels(factor(.data$season)),
      is_ibv = !is.na(.data$pathogen_display) & .data$pathogen_display == "IBV",
      is_pre_victoria = .data$period_original == 0 &
        !is.na(.data$pathogen_specific) & .data$pathogen_specific == "B(Victoria)",
      post_reported_victoria = .data$period_original == 1 &
        !is.na(.data$Pathogen_specific_reported) &
        .data$Pathogen_specific_reported == "B(Victoria)"
    )

  post_ibv <- dat$period_original == 1 & dat$is_ibv
  definitions <- list(
    list(
      id = "IBV_current",
      label = "Influenza B, current data (benchmark)",
      mask = dat$is_ibv
    ),
    list(
      id = "BVic_current_rule",
      label = "B/Victoria, current rule: pre explicit vs post all IBV (benchmark)",
      mask = dat$is_pre_victoria | post_ibv
    ),
    list(
      id = "BVic_reported_lineage",
      label = "B/Victoria, source-reported lineage in both periods",
      mask = dat$is_pre_victoria | dat$post_reported_victoria
    )
  )

  covariates <- c("age_display", "vaccine_type_display", "region_short", "outcome_display")

  # Primary adjusted model with crossed study and reported-period effects.
  fit_primary <- function(sub) {
    sub <- sub |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
    active <- active_covariates(sub, covariates)
    for (x in active) sub[[x]] <- droplevels(sub[[x]])
    model_data <- sub |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("study_id", "season", "period", active)))) |>
      dplyr::summarise(
        yi_model = sum(.data$yi_model / .data$vi_model) / sum(1 / .data$vi_model),
        vi_model = 1 / sum(1 / .data$vi_model),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
    model <- metafor::rma.mv(
      yi = yi_model, V = vi_model,
      mods = stats::reformulate(c("period", active)),
      random = list(~ 1 | study_id, ~ 1 | season),
      data = model_data, method = "REML", test = "t", sparse = TRUE
    )
    df <- nrow(sub) - ncol(model$X)
    if (!is.finite(df) || df <= 0) stop("Insufficient residual degrees of freedom.")
    idx <- grep("^period", rownames(model$b))
    beta <- as.numeric(model$b[idx, 1])
    se <- model$se[idx]
    critical <- stats::qt(0.975, df)
    list(beta = beta, se = se, df = df,
         ci_lb = beta - critical * se, ci_ub = beta + critical * se,
         p = 2 * stats::pt(-abs(beta / se), df), active = active)
  }

  effect_row <- function(res, inference) {
    if (inherits(res, "error")) {
      return(tibble::tibble(
        Inference = inference, Relative_period_effect_percent = NA_real_,
        Lower_percent = NA_real_, Upper_percent = NA_real_, p_value = NA_real_,
        df = NA_real_, Coefficient_logOR = NA_real_, SE = NA_real_,
        Active_covariates = NA_character_, Status = conditionMessage(res)
      ))
    }
    tibble::tibble(
      Inference = inference,
      Relative_period_effect_percent = (1 - exp(res$beta)) * 100,
      Lower_percent = (1 - exp(res$ci_ub)) * 100,
      Upper_percent = (1 - exp(res$ci_lb)) * 100,
      p_value = res$p,
      df = res$df,
      Coefficient_logOR = res$beta,
      SE = res$se,
      Active_covariates = paste(res$active, collapse = "; "),
      Status = "ok"
    )
  }

  pooled_row <- function(sub, period_value) {
    part <- sub[sub$period_original == period_value, , drop = FALSE]
    s <- summarize_multilevel_fit(fit_multilevel_meta(part, aggregate_random_effect_cells = TRUE))
    tibble::tibble(
      Period = ifelse(period_value == 0, "Pre-pandemic", "Pandemic/post-pandemic"),
      Studies = s$n_studies,
      Estimates = s$n_estimates,
      Seasons = s$n_seasons,
      Pooled_VE_percent = s$pooled_ve,
      Lower_percent = s$pooled_ve_lower,
      Upper_percent = s$pooled_ve_upper,
      I2_percent = s$i2_multilevel,
      Status = s$status
    )
  }

  effects <- list()
  pooled <- list()
  support <- list()
  for (def in definitions) {
    sub <- dat[def$mask, , drop = FALSE]
    message("Fitting ", def$id, " (k=", nrow(sub), ", studies=", dplyr::n_distinct(sub$study_id), ")")
    counts <- tibble::tibble(
      Definition = def$id,
      Description = def$label,
      Pre_studies = dplyr::n_distinct(sub$study_id[sub$period_original == 0]),
      Pre_estimates = sum(sub$period_original == 0),
      Post_studies = dplyr::n_distinct(sub$study_id[sub$period_original == 1]),
      Post_estimates = sum(sub$period_original == 1)
    )
    support[[length(support) + 1L]] <- counts
    effects[[length(effects) + 1L]] <- dplyr::bind_cols(
      counts,
      effect_row(tryCatch(fit_primary(sub), error = function(e) e),
                 "Primary: crossed study + reported-period RE; model-based t")
    )
    pooled[[length(pooled) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(Definition = def$id, Description = def$label),
      dplyr::bind_rows(pooled_row(sub, 0), pooled_row(sub, 1))
    )
  }

  effects <- dplyr::bind_rows(effects)
  pooled <- dplyr::bind_rows(pooled)
  support <- dplyr::bind_rows(support)

  readr::write_excel_csv(effects, file.path(out_dir, "TableS17_period_effects.csv"), na = "")
  readr::write_excel_csv(pooled, file.path(out_dir, "TableS17_pooled_ve_by_period.csv"), na = "")
  add_excel_sheets(
    file.path(out_dir, "TableS17_b_victoria_reported_lineage_outputs.xlsx"),
    list(
      "Period effects" = effects,
      "Pooled VE by period" = pooled,
      "Support" = support
    )
  )

  message("Wrote outputs to ", out_dir)
}

joint_support <- function() {
  check_required_packages(c("readxl", "openxlsx"))
  root <- get_project_root()
  out_dir <- file.path(results_root(root), "TableS18")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "model_results"), showWarnings = FALSE)

  dat <- prepare_period_data(read_source_verified_analysis_data(root))
  dat$eligible_source_index <- seq_len(nrow(dat))
  specs <- period_model_specs()

  count_support <- function(x, covariates) {
    x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(covariates))) |>
      dplyr::summarise(
        estimates_pre = sum(.data$period_original == 0),
        estimates_post = sum(.data$period_original == 1),
        studies_pre = dplyr::n_distinct(.data$study_id[.data$period_original == 0]),
        studies_post = dplyr::n_distinct(.data$study_id[.data$period_original == 1]),
        study_ids_pre = paste(sort(unique(as.character(
          .data$study_id[.data$period_original == 0]))), collapse = ";"),
        study_ids_post = paste(sort(unique(as.character(
          .data$study_id[.data$period_original == 1]))), collapse = ";"),
        .groups = "drop"
      )
  }

  fit_restricted <- function(sub, covariates) {
    sub <- droplevels(sub)
    active <- covariates[vapply(sub[covariates], dplyr::n_distinct, integer(1)) > 1]
    formula <- stats::reformulate(c("period", active))
    model_data <- sub |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("study_id", "season", "period", active)))) |>
      dplyr::summarise(
        yi_model = sum(.data$yi_model / .data$vi_model) / sum(1 / .data$vi_model),
        vi_model = 1 / sum(1 / .data$vi_model), .groups = "drop"
      ) |>
      droplevels()
    X <- stats::model.matrix(formula, model_data)
    if (qr(X)$rank < ncol(X)) stop("Restricted fixed-effect design is rank deficient.")
    m <- metafor::rma.mv(yi = yi_model, V = vi_model, mods = formula,
      random = list(~ 1 | study_id, ~ 1 | season), data = model_data,
      method = "REML", test = "t", sparse = TRUE)
    i <- match("periodPandemic/Post-Pandemic", rownames(m$b))
    stopifnot(!is.na(i), !any(m$coef.na), nrow(sub) > m$p)
    # Preserve the primary analysis' model-based inference to isolate the
    # change in sample.
    df <- nrow(sub) - m$p
    beta <- as.numeric(m$b[i, 1]); se <- m$se[i]
    critical <- stats::qt(0.975, df)
    lower_log <- beta - critical * se; upper_log <- beta + critical * se
    effect <- tibble::tibble(
      active_covariates = paste(active, collapse = "; "),
      constant_covariates = paste(setdiff(covariates, active), collapse = "; "),
      aggregated_rows = nrow(model_data), parameters = m$p,
      model_based_df = df, beta_log_ratio = beta, standard_error = se,
      relative_period_effect_percent = 100 * (1 - exp(beta)),
      ci_lower_percent = 100 * (1 - exp(upper_log)),
      ci_upper_percent = 100 * (1 - exp(lower_log)),
      p_value = 2 * stats::pt(-abs(beta / se), df),
      study_variance = m$sigma2[1], reported_period_variance = m$sigma2[2]
    )
    list(model = m, effect = effect)
  }

  jobs <- list()
  # Unknown vaccine type, influenza type and clinical outcome subgroups are not
  # displayed in Table S18.
  for (spec in specs) for (category in setdiff(spec$cats, "Unknown")) {
    sub <- if (is.na(spec$col)) dat else if (spec$panel == "Influenza type") {
      dat[influenza_type_mask(dat, category), , drop = FALSE]
    } else dat[!is.na(dat[[spec$col]]) & as.character(dat[[spec$col]]) == category, , drop = FALSE]
    jobs[[length(jobs) + 1L]] <- list(panel = spec$panel, subgroup = category,
      covariates = spec$covars, sub = sub, primary_model = TRUE)
  }
  # Include Table 1 rows without primary adjusted estimates, explicitly showing
  # their absent/sparse support rather than dropping them from the audit.
  virus_covariates <- c("age_display", "vaccine_type_display", "region_short", "outcome_display")
  region_covariates <- c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")
  jobs[[length(jobs) + 1L]] <- list(panel = "Influenza type", subgroup = "B/Yamagata",
    covariates = virus_covariates,
    sub = dat[!is.na(dat$pathogen_specific) & dat$pathogen_specific == "B(Yamagata)", ],
    primary_model = FALSE)
  for (region in c("AFRO", "EMRO")) jobs[[length(jobs) + 1L]] <- list(
    panel = "WHO region", subgroup = region, covariates = region_covariates,
    sub = dat[dat$region_short == region, ], primary_model = FALSE)
  for (i in seq_along(jobs)) jobs[[i]]$model_id <- sprintf("M%02d", i)
  stopifnot(length(jobs) == 22L, !anyNA(dat[c("study_id", "season", "period")]))

  primary_file <- file.path(results_root(root), "Table1",
    "Table1_adjusted_rma_mv_period_effects.csv")
  primary_results <- if (file.exists(primary_file)) readr::read_csv(primary_file, show_col_types = FALSE) else NULL
  thresholds <- c(0L, 2L, 3L, 5L)
  all_counts <- list(); all_support <- list(); all_membership <- list()
  for (i in seq_along(jobs)) {
    job <- jobs[[i]]; sub <- job$sub; cv <- job$covariates
    stopifnot(!anyNA(sub[c("study_id", "season", "period", cv)]))
    support <- count_support(sub, cv) |>
      dplyr::mutate(pattern_id = sprintf(paste0(job$model_id, "-%03d"), dplyr::row_number()),
        min_studies_both_periods = pmin(.data$studies_pre, .data$studies_post))
    # Include fixed subgroup categories in the audit even though they do not
    # form additional dimensions of the within-subgroup pattern.
    all_categories <- c("age_display", "vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
    for (v in setdiff(all_categories, names(support))) {
      vals <- unique(as.character(sub[[v]]))
      support[[v]] <- if (length(vals) == 1L) vals else NA_character_
    }
    membership <- dplyr::left_join(sub,
      dplyr::select(support, dplyr::all_of(cv), "pattern_id", "min_studies_both_periods"), by = cv)
    support <- support |>
      dplyr::mutate(model_id = job$model_id, panel = job$panel, subgroup = job$subgroup,
        pattern_covariates = paste(cv, collapse = "; "), .before = 1)
    job$support <- support; job$membership <- membership
    job$definition_note <- if (job$subgroup == "B/Victoria") {
      "Table 1 definition: explicit B(Victoria) before the pandemic versus all IBV during/after the pandemic."
    } else ""
    job$counts <- dplyr::bind_rows(lapply(thresholds, function(threshold) {
      retained <- membership[membership$min_studies_both_periods >= threshold, ]
      chosen <- support[support$min_studies_both_periods >= threshold, ]
      stopifnot(nrow(retained) == sum(chosen$estimates_pre + chosen$estimates_post))
      if (threshold > 0L) {
        recounted <- count_support(retained, cv)
        stopifnot(all(recounted$studies_pre >= threshold), all(recounted$studies_post >= threshold))
      }
      tibble::tibble(model_id = job$model_id, panel = job$panel, subgroup = job$subgroup,
        primary_model = job$primary_model, threshold = threshold,
        patterns = nrow(chosen), studies_pre = dplyr::n_distinct(retained$study_id[retained$period_original == 0]),
        studies_post = dplyr::n_distinct(retained$study_id[retained$period_original == 1]),
        studies_total = dplyr::n_distinct(retained$study_id),
        estimates_pre = sum(retained$period_original == 0), estimates_post = sum(retained$period_original == 1),
        estimates_total = nrow(retained),
        retained_estimates_pre_percent = if (sum(sub$period_original == 0)) 100 * sum(retained$period_original == 0) / sum(sub$period_original == 0) else NA_real_,
        retained_estimates_post_percent = if (sum(sub$period_original == 1)) 100 * sum(retained$period_original == 1) / sum(sub$period_original == 1) else NA_real_,
        seasons_pre = dplyr::n_distinct(retained$season[retained$period_original == 0]),
        seasons_post = dplyr::n_distinct(retained$season[retained$period_original == 1]),
        vaccine_categories = paste(unique(as.character(retained$vaccine_type_display)), collapse = "; "),
        age_categories = paste(unique(as.character(retained$age_display)), collapse = "; "),
        outcome_categories = paste(unique(as.character(retained$outcome_display)), collapse = "; "),
        region_categories = paste(unique(as.character(retained$region_short)), collapse = "; "),
        pattern_covariates = paste(cv, collapse = "; "), definition_note = job$definition_note)
    }))
    jobs[[i]] <- job
    all_counts[[i]] <- job$counts
    all_support[[i]] <- support
    all_membership[[i]] <- membership |>
      dplyr::select("eligible_source_index", "study_id", "season", "period",
        dplyr::all_of(all_categories), "pattern_id", "min_studies_both_periods") |>
      dplyr::mutate(model_id = job$model_id, panel = job$panel, subgroup = job$subgroup, .before = 1)
  }
  counts <- dplyr::bind_rows(all_counts)
  support <- dplyr::bind_rows(all_support)
  membership <- dplyr::bind_rows(all_membership)
  readr::write_excel_csv(counts, file.path(out_dir, "TableS18_counts.csv"), na = "")
  readr::write_excel_csv(support, file.path(out_dir, "TableS18_joint_pattern_support.csv"), na = "")
  readr::write_excel_csv(membership, file.path(out_dir, "TableS18_record_membership.csv"), na = "")
  message("Saved all counts: ", length(jobs), " groups x ", length(thresholds), " thresholds.")

  signature_paths <- c(file.path(code_dir, c("functions.R", "meta_analysis.R",
    "sensitivity.R", "Data/review_data.xlsx")), primary_file)
  signature <- tools::md5sum(signature_paths[file.exists(signature_paths)])
  fit_job <- function(job) {
    path <- file.path(out_dir, "model_results", paste0(job$model_id, ".rds"))
    if (file.exists(path)) {
      cached <- readRDS(path)
      if (identical(cached$signature, signature)) {
        message("Reusing completed ", job$model_id, " ", job$subgroup)
        return(cached)
      }
    }
    results <- list()
    for (threshold in thresholds) {
      summary <- job$counts[job$counts$threshold == threshold, ]
      sub <- job$membership[job$membership$min_studies_both_periods >= threshold, ]
      result <- summary
      result$status <- "not_fitted"; result$detail <- ""; result$result_source <- "new_fit"
      result$relative_period_effect_percent <- NA_real_
      result$ci_lower_percent <- NA_real_; result$ci_upper_percent <- NA_real_
      ref <- if (!is.null(primary_results)) primary_results[
        primary_results$Panel == job$panel & primary_results$Subgroup == job$subgroup, ] else NULL
      if (threshold == 0L && !is.null(ref) && nrow(ref) == 1L && job$primary_model) {
        stopifnot(summary$estimates_total == ref$No_of_estimates,
                  summary$studies_total == ref$No_of_studies)
        result$status <- "reference_existing_primary"
        result$result_source <- "Results/Table1/Table1_adjusted_rma_mv_period_effects.csv"
        result$relative_period_effect_percent <- ref$VE_change_percent
        result$ci_lower_percent <- ref$VE_change_lower_percent
        result$ci_upper_percent <- ref$VE_change_upper_percent
      } else if (threshold == 0L && !job$primary_model) {
        result$status <- "descriptive_only"
        result$detail <- "No adjusted estimate in the primary table; counts retained for support audit."
      } else if (nrow(sub) == 0L) {
        result$status <- "no_qualifying_patterns"
        result$detail <- "No joint pattern has the required number of study IDs in BOTH periods."
      } else if (dplyr::n_distinct(sub$period) < 2L || dplyr::n_distinct(sub$study_id) < 2L ||
                 dplyr::n_distinct(sub$season) < 2L || nrow(sub) <= 2L) {
        result$status <- "insufficient_data"
        result$detail <- "Both periods, multiple study IDs/reported periods and more than two estimates required."
      } else {
        message(job$model_id, " ", job$panel, " / ", job$subgroup,
          " threshold=", threshold, " k=", nrow(sub))
        warns <- character()
        fitted <- tryCatch(withCallingHandlers(fit_restricted(sub, job$covariates),
          warning = function(w) {warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")}),
          error = function(e) e)
        if (inherits(fitted, "error")) {
          result$status <- if (grepl("rank deficient", conditionMessage(fitted))) "rank_deficient" else "fit_failed"
          result$detail <- conditionMessage(fitted)
        } else {
          result$status <- "ok"; result$detail <- paste(unique(warns), collapse = "; ")
          for (col in names(fitted$effect)) result[[col]] <- fitted$effect[[col]]
        }
      }
      results[[length(results) + 1L]] <- result
    }
    output <- list(signature = signature, results = dplyr::bind_rows(results))
    saveRDS(output, path)
    message("Completed ", job$model_id, " ", job$panel, " / ", job$subgroup)
    output
  }
  cores <- suppressWarnings(as.integer(Sys.getenv("VE_SUPPORT_CORES", unset = "4")))
  if (!is.finite(cores) || cores < 1L) stop("VE_SUPPORT_CORES must be a positive integer.")
  cores <- min(cores, length(jobs))
  fitted_jobs <- if (.Platform$OS.type == "unix" && cores > 1L) {
    parallel::mclapply(jobs, fit_job, mc.cores = cores, mc.preschedule = FALSE)
  } else lapply(jobs, fit_job)
  if (any(vapply(fitted_jobs, inherits, logical(1), "try-error"))) {
    stop("At least one subgroup audit failed; completed jobs are checkpointed in model_results/.")
  }
  results <- dplyr::bind_rows(lapply(fitted_jobs, `[[`, "results")) |>
    dplyr::arrange(.data$model_id, .data$threshold)
  results$effect_ci <- ifelse(is.finite(results$relative_period_effect_percent),
    sprintf("%+.1f (%+.1f, %+.1f)", results$relative_period_effect_percent,
      results$ci_lower_percent, results$ci_upper_percent), "Not estimable / not reported")

  outputs <- list(results = results, counts = counts, joint_pattern_support = support)
  for (name in names(outputs)) {
    readr::write_excel_csv(outputs[[name]], file.path(out_dir, paste0("TableS18_", name, ".csv")), na = "")
  }
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", wrapText = TRUE)
  for (name in names(outputs)) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, outputs[[name]], headerStyle = header_style, withFilter = TRUE)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(outputs[[name]])), widths = 20)
    openxlsx::setRowHeights(wb, name, rows = 1, heights = 45)
  }
  openxlsx::saveWorkbook(wb, file.path(out_dir, "TableS18_joint_support_outputs.xlsx"), overwrite = TRUE)

  writeLines(c(paste("Run:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste(names(signature), signature, sep = " MD5="),
    "Effect: 100*(1-exp(beta_period)); not absolute VE percentage points.",
    "Inference: model SE; t with pre-aggregation source estimate count minus fixed coefficient count."),
    file.path(out_dir, "TableS18_methods_and_provenance.txt"))
  print(results |> dplyr::select("panel", "subgroup", "threshold", "patterns", "studies_pre",
    "studies_post", "estimates_pre", "estimates_post", "effect_ci", "status"), n = Inf, width = Inf)
}

study_influence <- function() {
  root <- get_project_root(file.path(
    "Data",
    "review_data.xlsx"
  ))
  out_dir <- file.path(
    results_root(root), "FigureS3"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  summary_csv <- file.path(out_dir, "FigureS3_study_influence_balance_summary.csv")
  loo_csv <- file.path(out_dir, "FigureS3_leave_one_study_out.csv")
  balance_csv <- file.path(out_dir, "FigureS3_study_contribution_balance_audit.csv")
  audit_csv <- file.path(out_dir, "FigureS3_leave_one_study_audit.csv")
  plot_csv <- file.path(out_dir, "FigureS3_plot_data.csv")
  out_xlsx <- file.path(out_dir, "FigureS3_study_influence_balance_summary.xlsx")
  progress_log <- file.path(out_dir, "progress.log")
  if (file.exists(progress_log)) file.remove(progress_log)

  log_progress <- function(...) {
    msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
    message(msg)
    cat(msg, "\n", file = progress_log, append = TRUE)
  }

  dat <- prepare_analysis_data(root) |>
    dplyr::mutate(
      pathogen_specific_display = dplyr::case_when(
        .data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)") ~ "A/H1N1",
        .data$pathogen_specific == "A(H3N2)" ~ "A/H3N2",
        .data$pathogen_specific == "B(Victoria)" ~ "B/Victoria",
        TRUE ~ NA_character_
      )
    )

  specs <- list(
    list(
      panel = "Overall", subgroup = "Overall",
      mask = function(x) rep(TRUE, nrow(x)),
      covariates = c(
        "age_display", "vaccine_type_display", "pathogen_display",
        "region_short", "outcome_display"
      )
    ),
    list(
      panel = "Age", subgroup = "0-17 years",
      mask = function(x) !is.na(x$age_display) & as.character(x$age_display) == "0-17 years",
      covariates = c(
        "vaccine_type_display", "pathogen_display", "region_short", "outcome_display"
      )
    ),
    list(
      panel = "Influenza type", subgroup = "A/H3N2",
      mask = function(x) !is.na(x$pathogen_specific_display) & x$pathogen_specific_display == "A/H3N2",
      covariates = c(
        "age_display", "vaccine_type_display", "region_short", "outcome_display"
      )
    ),
    list(
      panel = "Influenza type", subgroup = "IBV",
      mask = function(x) !is.na(x$pathogen_display) & as.character(x$pathogen_display) == "IBV",
      covariates = c(
        "age_display", "vaccine_type_display", "region_short", "outcome_display"
      )
    ),
    list(
      panel = "Influenza type", subgroup = "B/Victoria",
      mask = function(x) {
        (
          x$period == "Pre-pandemic" &
            !is.na(x$pathogen_specific_display) &
            x$pathogen_specific_display == "B/Victoria"
        ) |
          (
            x$period == "Pandemic/post-pandemic" &
              !is.na(x$pathogen_display) &
              as.character(x$pathogen_display) == "IBV"
          )
      },
      covariates = c(
        "age_display", "vaccine_type_display", "region_short", "outcome_display"
      )
    )
  )

  prepare_model_input <- function(sub, covariates, fixed_formula = NULL) {
    active <- active_covariates(sub, covariates)
    formula <- if (is.null(fixed_formula)) {
      stats::reformulate(c("period", active))
    } else {
      fixed_formula
    }
    model_variables <- all.vars(formula)
    required <- unique(c(
      "study_id", "season", model_variables, "yi_model", "vi_model"
    ))
    original <- sub |>
      dplyr::select(dplyr::all_of(required)) |>
      tidyr::drop_na(dplyr::all_of(c("study_id", "season", model_variables))) |>
      dplyr::filter(
        is.finite(.data$yi_model), is.finite(.data$vi_model), .data$vi_model > 0
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
    for (variable in model_variables) {
      if (is.factor(original[[variable]])) original[[variable]] <- droplevels(original[[variable]])
    }

    group_columns <- unique(c("study_id", "season", model_variables))
    aggregated <- original |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) |>
      dplyr::summarise(
        yi_model = sum(.data$yi_model / .data$vi_model) / sum(1 / .data$vi_model),
        vi_model = 1 / sum(1 / .data$vi_model),
        source_estimates = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
    for (variable in model_variables) {
      if (is.factor(aggregated[[variable]])) aggregated[[variable]] <- droplevels(aggregated[[variable]])
    }
    list(original = original, aggregated = aggregated, formula = formula)
  }

  fit_model <- function(input, vi_column = "vi_model", fixed_sigma2 = NULL) {
    args <- list(
      yi = input$aggregated$yi_model,
      V = input$aggregated[[vi_column]],
      mods = input$formula,
      random = list(~ 1 | study_id, ~ 1 | season),
      data = input$aggregated,
      method = "REML",
      test = "t",
      sparse = TRUE
    )
    if (!is.null(fixed_sigma2)) args$sigma2 <- fixed_sigma2
    suppressWarnings(do.call(metafor::rma.mv, args))
  }

  extract_period <- function(fit, original_rows) {
    term <- grep("^period.*Pandemic", rownames(fit$b), value = TRUE)
    if (length(term) != 1) stop("Expected exactly one pandemic/post-pandemic period coefficient")
    idx <- match(term, rownames(fit$b))
    beta <- as.numeric(fit$b[idx, 1])
    se <- as.numeric(fit$se[idx])
    # Pre-aggregation residual degrees of freedom, as in the primary analysis.
    df <- original_rows - ncol(fit$X)
    if (!is.finite(df) || df <= 0) stop("Insufficient residual degrees of freedom")
    critical <- stats::qt(0.975, df)
    ci_lower_beta <- beta - critical * se
    ci_upper_beta <- beta + critical * se
    tibble::tibble(
      beta = beta,
      se = se,
      df = df,
      p = 2 * stats::pt(-abs(beta / se), df),
      effect = 100 * (1 - exp(beta)),
      lower = 100 * (1 - exp(ci_upper_beta)),
      upper = 100 * (1 - exp(ci_lower_beta))
    )
  }

  signed <- function(x) sprintf("%+.1f", x)
  effect_ci <- function(effect, lower, upper) {
    paste0(signed(effect), "% (", signed(lower), "%, ", signed(upper), "%)")
  }

  summary_rows <- list()
  loo_rows <- list()
  balance_rows <- list()
  audit_rows <- list()
  plot_rows <- list()

  configured_cores <- suppressWarnings(as.integer(Sys.getenv("ANALYSIS_CORES", unset = "4")))
  if (!is.finite(configured_cores) || configured_cores < 1) configured_cores <- 1L

  for (spec in specs) {
    label <- paste(spec$panel, spec$subgroup, sep = " - ")
    sub <- dat[spec$mask(dat), , drop = FALSE] |>
      droplevels()
    log_progress(
      "Fitting primary and study-balanced models | subgroup=", label,
      " | estimates=", nrow(sub),
      " | studies=", dplyr::n_distinct(sub$study_id)
    )

    input <- prepare_model_input(sub, spec$covariates)
    primary_fit <- fit_model(input)
    primary <- extract_period(primary_fit, nrow(input$original))

    # Normalize each study's total sampling precision to the median total study
    # precision, while preserving relative precision among cells within a study.
    balanced_data <- input$aggregated |>
      dplyr::mutate(cell_precision = 1 / .data$vi_model) |>
      dplyr::group_by(.data$study_id) |>
      dplyr::mutate(study_total_precision = sum(.data$cell_precision)) |>
      dplyr::ungroup()
    target_precision <- stats::median(unique(
      balanced_data[c("study_id", "study_total_precision")]
    )$study_total_precision)
    balanced_data <- balanced_data |>
      dplyr::mutate(
        vi_balanced = .data$vi_model * .data$study_total_precision / target_precision
      )
    balanced_input <- input
    balanced_input$aggregated <- balanced_data
    balanced_fit <- fit_model(balanced_input, vi_column = "vi_balanced")
    balanced <- extract_period(balanced_fit, nrow(input$original))

    all_studies <- sub |>
      dplyr::distinct(.data$study_id) |>
      dplyr::pull(.data$study_id) |>
      as.character() |>
      sort()
    log_progress(
      "Running complete leave-one-study-out | subgroup=", label,
      " | omissions=", length(all_studies)
    )

    omit_one <- function(omitted) {
      tryCatch({
        reduced <- sub[as.character(sub$study_id) != omitted, , drop = FALSE] |>
          droplevels()
        reduced_input <- prepare_model_input(
          reduced, spec$covariates, fixed_formula = input$formula
        )
        reduced_fit <- fit_model(
          reduced_input, fixed_sigma2 = as.numeric(primary_fit$sigma2)
        )
        result <- extract_period(reduced_fit, nrow(reduced_input$original))
        dplyr::bind_cols(
          tibble::tibble(
            panel = spec$panel,
            subgroup = spec$subgroup,
            omitted_study = omitted,
            removed_estimates = sum(as.character(sub$study_id) == omitted),
            removed_pre_estimates = sum(
              as.character(sub$study_id) == omitted &
                sub$period == "Pre-pandemic"
            ),
            removed_post_estimates = sum(
              as.character(sub$study_id) == omitted &
                sub$period == "Pandemic/post-pandemic"
            ),
            status = "ok"
          ),
          result
        )
      }, error = function(e) {
        tibble::tibble(
          panel = spec$panel,
          subgroup = spec$subgroup,
          omitted_study = omitted,
          status = paste0("failed: ", conditionMessage(e))
        )
      })
    }

    cores <- min(configured_cores, max(1L, length(all_studies)))
    omitted_results <- if (.Platform$OS.type == "unix" && cores > 1L) {
      parallel::mclapply(
        all_studies, omit_one, mc.cores = cores, mc.preschedule = FALSE
      )
    } else {
      lapply(all_studies, omit_one)
    }
    omitted <- dplyr::bind_rows(omitted_results)
    successful <- omitted |>
      dplyr::filter(.data$status == "ok")
    if (nrow(successful) != length(all_studies)) {
      stop("One or more leave-one-study fits failed for ", label, call. = FALSE)
    }
    loo_rows[[length(loo_rows) + 1]] <- omitted

    summary_rows[[length(summary_rows) + 1]] <- tibble::tibble(
      Panel = spec$panel,
      Subgroup = spec$subgroup,
      `Primary adjusted period effect, % (95% CI)` = effect_ci(
        primary$effect, primary$lower, primary$upper
      ),
      `Studies omitted in turn, n` = nrow(successful),
      `Leave-one-study effect range, %` = paste0(
        signed(min(successful$effect)), " to ", signed(max(successful$effect))
      ),
      `Positive direction, n/N` = paste0(
        sum(successful$effect > 0), "/", nrow(successful)
      ),
      `95% CI excluded null, n/N` = paste0(
        sum(successful$lower > 0 | successful$upper < 0), "/", nrow(successful)
      ),
      `Study-balanced effect, % (95% CI)` = effect_ci(
        balanced$effect, balanced$lower, balanced$upper
      )
    )

    plot_rows[[length(plot_rows) + 1]] <- tibble::tibble(
      Panel = spec$panel,
      Subgroup = spec$subgroup,
      studies_omitted = nrow(successful),
      primary_effect = primary$effect,
      primary_lower = primary$lower,
      primary_upper = primary$upper,
      loo_effect_min = min(successful$effect),
      loo_effect_max = max(successful$effect),
      loo_ci_lower_min = min(successful$lower),
      loo_ci_upper_max = max(successful$upper),
      loo_ci_excludes_null_n = sum(
        successful$lower > 0 | successful$upper < 0
      ),
      balanced_effect = balanced$effect,
      balanced_lower = balanced$lower,
      balanced_upper = balanced$upper
    )

    balance_rows[[length(balance_rows) + 1]] <- tibble::tibble(
      Panel = spec$panel,
      Subgroup = spec$subgroup,
      source_estimates = nrow(input$original),
      studies = dplyr::n_distinct(input$original$study_id),
      sufficient_statistic_cells = nrow(input$aggregated),
      target_total_precision_per_study = target_precision,
      minimum_original_study_total_precision = min(balanced_data$study_total_precision),
      # `balanced_data` contains repeated study totals (one per sufficient-statistic
      # cell), so the study-level median is the normalization target computed above.
      median_original_study_total_precision = target_precision,
      maximum_original_study_total_precision = max(balanced_data$study_total_precision),
      primary_tau2_study = primary_fit$sigma2[[1]],
      primary_tau2_season = primary_fit$sigma2[[2]],
      balanced_tau2_study = balanced_fit$sigma2[[1]],
      balanced_tau2_season = balanced_fit$sigma2[[2]]
    )

    audit_rows[[length(audit_rows) + 1]] <- tibble::tibble(
      Panel = spec$panel,
      Subgroup = spec$subgroup,
      source_estimates = nrow(input$original),
      studies = dplyr::n_distinct(input$original$study_id),
      seasons = dplyr::n_distinct(input$original$season),
      studies_omitted_in_turn = length(all_studies),
      coefficients = ncol(primary_fit$X),
      primary_status = "ok",
      balanced_status = "ok",
      loo_status = "all leave-one-study deletion fits completed"
    )
    log_progress("Finished subgroup=", label)
  }

  summary_table <- dplyr::bind_rows(summary_rows)
  loo_detail <- dplyr::bind_rows(loo_rows)
  balance_audit <- dplyr::bind_rows(balance_rows)
  analysis_audit <- dplyr::bind_rows(audit_rows)
  plot_data <- dplyr::bind_rows(plot_rows)

  readr::write_excel_csv(summary_table, summary_csv, na = "")
  readr::write_excel_csv(loo_detail, loo_csv, na = "")
  readr::write_excel_csv(balance_audit, balance_csv, na = "")
  readr::write_excel_csv(analysis_audit, audit_csv, na = "")
  readr::write_excel_csv(plot_data, plot_csv, na = "")
  add_excel_sheets(
    out_xlsx,
    list(
      "Summary" = summary_table,
      "Leave-one-study detail" = loo_detail,
      "Study balance audit" = balance_audit,
      "Analysis audit" = analysis_audit,
      "Plot data" = plot_data
    )
  )

  message("Wrote: ", summary_csv)
  message("Wrote: ", loo_csv)
  message("Wrote: ", balance_csv)
  message("Wrote: ", audit_csv)
  message("Wrote: ", plot_csv)
  message("Wrote: ", out_xlsx)
}

if (sys.nframe() == 0L) {
  jobs <- list(
    S11 = function() restricted_analysis("s11_2016_2025"),
    S12 = function() restricted_analysis("s12_case_control"),
    S13 = function() restricted_analysis("s13_nos_ge7"),
    S14 = function() restricted_analysis("s14_nos_ge7_tnd_adapted"),
    S15 = single_season, S16 = cluster_robust, S17 = reported_lineage,
    S18 = joint_support, influence = study_influence
  )
  selected <- commandArgs(TRUE)
  if (!length(selected)) selected <- names(jobs)
  if (any(!selected %in% names(jobs))) stop("Choose S11-S18 or influence.")
  for (name in selected) {
    message("Running ", name)
    jobs[[name]]()
  }
}
