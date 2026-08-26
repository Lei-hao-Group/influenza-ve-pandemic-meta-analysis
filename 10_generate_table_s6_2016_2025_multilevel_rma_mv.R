#!/usr/bin/env Rscript

if (tolower(Sys.getenv("TABLE_S6_WARN_IMMEDIATE", unset = "false")) %in% c("1", "true", "yes")) {
  options(warn = 1)
}

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
} else {
  file.path(getwd(), "Submission_Code")
}
source(file.path(script_dir, "00_meta_analysis_core.R"))

check_required_packages(extra = c("openxlsx", "readxl"))

source_verified_file <- file.path(
  "Submission_Code", "Data",
  "review_data.xlsx"
)
root <- get_project_root(input_csv = source_verified_file)
dat_all <- read_source_verified_analysis_data(root)
source_audit <- attr(dat_all, "source_audit")

sensitivity_mode <- tolower(Sys.getenv("TABLE_SENSITIVITY_MODE", unset = "s6"))
table_label <- ifelse(sensitivity_mode == "s8_nos_ge7", "Table S8", "Table S6")
sheet_label <- table_label

sensitivity_pre_seasons <- c(
  "2016", "2016-2017", "2016-2017, 2016",
  "2017", "2017-2018", "2017-2018, 2017",
  "2018", "2018-2019", "2018-2019, 2018",
  "2019", "2019-2020"
)

if (sensitivity_mode == "s8_nos_ge7") {
  quality_file <- file.path(
    root,
    "Submission_Code", "Data",
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
  high_quality_ids <- quality$study_id_numeric[quality$quality_score >= quality_threshold]
  dat <- dat_all |>
    dplyr::filter(as_num(as.character(.data$study_id)) %in% high_quality_ids)
  prefix <- "TableS8_high_quality_NOS_ge7_multilevel_random_effects"
  out_dir <- file.path(root, "Results", "TablesS6-S8", "TableS8")
  audit_analysis <- "Table S8 sensitivity: restricted to studies with Newcastle-Ottawa Scale score >=7"
  audit_rows_metric <- "rows_used_nos_ge7"
} else if (sensitivity_mode == "s6") {
  dat <- dat_all |>
    dplyr::filter(
      (.data$period_original == 0 & as.character(.data$season) %in% sensitivity_pre_seasons) |
        .data$period_original == 1
    )
  prefix <- "TableS6_2016_2025_multilevel_random_effects"
  out_dir <- file.path(root, "Results", "TablesS6-S8", "TableS6")
  audit_analysis <- "Table S6 sensitivity: pre-pandemic restricted to 2016-2019 seasons plus pandemic/post-pandemic through 2025"
  audit_rows_metric <- "rows_used_2016_2025"
} else {
  stop("Unsupported TABLE_SENSITIVITY_MODE: ", sensitivity_mode, call. = FALSE)
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
rows <- add_pair(rows, "Age", "0-64 years & \u226518 years", function(x) x$age_label == "0-64 years & \u226518 years")

rows[[length(rows) + 1]] <- section_row("Vaccine type"); write_partial(rows)
rows <- add_pair(rows, "Vaccine type", "Trivalent", function(x) x$vaccine_label == "Trivalent")
rows <- add_pair(rows, "Vaccine type", "Quadrivalent", function(x) x$vaccine_label == "Quadrivalent")
rows <- add_pair(rows, "Vaccine type", "Other", function(x) x$vaccine_label == "Other")
rows <- add_pair(rows, "Vaccine type", "Unknown", function(x) x$vaccine_label == "Unknown")

rows[[length(rows) + 1]] <- section_row("Influenza type"); write_partial(rows)
rows <- add_pair(rows, "Influenza type", "IAV", function(x) x$pathogen == "IAV")
rows <- add_pair(rows, "Influenza type", "A/H1N1", function(x) x$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)"))
rows <- add_pair(rows, "Influenza type", "A/H3N2", function(x) x$pathogen_specific == "A(H3N2)")
rows <- add_pair(rows, "Influenza type", "IBV", function(x) x$pathogen == "IBV")
rows <- add_pair(rows, "Influenza type", "B/Yamagata", function(x) x$pathogen_specific == "B(Yamagata)")
rows <- add_pair(rows, "Influenza type", "B/Victoria", function(x) x$pathogen_specific == "B(Victoria)")
rows <- add_pair(rows, "Influenza type", "Unknown", function(x) x$pathogen == "No distinction")

rows[[length(rows) + 1]] <- section_row("WHO region"); write_partial(rows)
rows <- add_pair(rows, "WHO region", "EURO", function(x) x$who_label == "EURO")
rows <- add_pair(rows, "WHO region", "AMRO", function(x) x$who_label == "AMRO")
rows <- add_pair(rows, "WHO region", "WPRO", function(x) x$who_label == "WPRO")
rows <- add_pair(rows, "WHO region", "SEARO", function(x) x$who_label == "SEARO")
rows <- add_pair(rows, "WHO region", "AFRO", function(x) x$who_label == "AFRO")
rows <- add_pair(rows, "WHO region", "EMRO", function(x) x$who_label == "EMRO")
rows <- add_pair(rows, "WHO region", "Multi-region", function(x) x$who_label == "Multi-region")

rows[[length(rows) + 1]] <- section_row("Clinical outcome"); write_partial(rows)
rows <- add_pair(rows, "Clinical outcome", "Outpatient", function(x) x$outcome_label == "Outpatient")
rows <- add_pair(rows, "Clinical outcome", "Inpatient", function(x) x$outcome_label == "Inpatient")
rows <- add_pair(rows, "Clinical outcome", "Severe and Death", function(x) x$outcome_label %in% c("Severe", "Death", "Severe and Death"))
rows <- add_pair(rows, "Clinical outcome", "Unknown", function(x) x$outcome_label == "Unknown")

table_out <- dplyr::bind_rows(rows)
readr::write_excel_csv(table_out, out_csv, na = "")

# Table S6 reports the intercept-only pooled estimates above together with
# subgroup-specific adjusted period effects. The adjusted model follows the
# primary specification but uses the restricted Table S6 time window.
model_dat <- dat |>
  dplyr::mutate(
    adjusted_ve_percent = as_num(.data$reported_ve_percent),
    adjusted_ve_ci_lower_percent = as_num(.data$reported_ve_ci_lower_percent),
    adjusted_ve_ci_upper_percent = as_num(.data$reported_ve_ci_upper_percent),
    adjusted_or_recalc = 1 - .data$adjusted_ve_percent / 100,
    adjusted_or_ci_lower_recalc = 1 - .data$adjusted_ve_ci_upper_percent / 100,
    adjusted_or_ci_upper_recalc = 1 - .data$adjusted_ve_ci_lower_percent / 100,
    yi_model = log(.data$adjusted_or_recalc),
    ci_lb_log_or_model = log(.data$adjusted_or_ci_lower_recalc),
    ci_ub_log_or_model = log(.data$adjusted_or_ci_upper_recalc),
    sei_model = (.data$ci_ub_log_or_model - .data$ci_lb_log_or_model) / (2 * 1.96),
    vi_model = .data$sei_model^2,
    period = factor(
      .data$period_original,
      levels = c(0, 1),
      labels = c("Pre-pandemic", "Pandemic/Post-Pandemic")
    ),
    age_display = factor(
      .data$age_label,
      levels = c("All ages", "0-17 years", "18-64 years", "\u226565 years", "0-64 years & \u226518 years")
    ),
    vaccine_type_display = factor(
      .data$vaccine_label,
      levels = c("Unknown", "Trivalent", "Quadrivalent", "Other")
    ),
    pathogen_display = factor(
      .data$pathogen_label,
      levels = c("Unknown", "IAV", "IBV")
    ),
    pathogen_specific_display = factor(
      dplyr::case_when(
        .data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)") ~ "A/H1N1",
        .data$pathogen_specific == "A(H3N2)" ~ "A/H3N2",
        .data$pathogen_specific == "B(Victoria)" ~ "B/Victoria",
        TRUE ~ NA_character_
      ),
      levels = c("A/H1N1", "A/H3N2", "B/Victoria")
    ),
    region_short = factor(
      .data$who_label,
      levels = c("Multi-region", "AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")
    ),
    outcome_display = factor(
      dplyr::case_when(
        .data$outcome_label %in% c("Severe", "Death", "Severe and Death") ~ "Severe and Death",
        TRUE ~ .data$outcome_label
      ),
      levels = c("Unknown", "Outpatient", "Inpatient", "Severe and Death")
    )
  ) |>
  dplyr::filter(
    is.finite(.data$yi_model),
    is.finite(.data$vi_model),
    .data$vi_model > 0,
    .data$adjusted_or_recalc > 0,
    .data$adjusted_or_ci_lower_recalc > 0,
    .data$adjusted_or_ci_upper_recalc > 0
  )

fit_adjusted_period_model <- function(sub, covariates) {
  sub <- sub |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season = droplevels(factor(.data$season))
    )
  active_covariates <- covariates[vapply(covariates, function(x) {
    x %in% names(sub) && dplyr::n_distinct(sub[[x]], na.rm = TRUE) > 1
  }, logical(1))]
  mods_formula <- stats::reformulate(c("period", active_covariates))
  aggregation_columns <- c("study_id", "season", "period", active_covariates)
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
  model <- metafor::rma.mv(
    yi = yi_model,
    V = vi_model,
    mods = mods_formula,
    random = list(~ 1 | study_id, ~ 1 | season),
    data = model_data,
    method = "REML",
    test = "t",
    sparse = TRUE
  )
  original_df <- nrow(sub) - ncol(model$X)
  if (!is.finite(original_df) || original_df <= 0) {
    stop("Insufficient pre-aggregation residual degrees of freedom.", call. = FALSE)
  }
  critical <- stats::qt(0.975, original_df)
  model$ci.lb <- as.numeric(model$b[, 1]) - critical * model$se
  model$ci.ub <- as.numeric(model$b[, 1]) + critical * model$se
  model$pval <- 2 * stats::pt(
    -abs(as.numeric(model$b[, 1]) / model$se), df = original_df
  )
  list(
    model = model,
    aggregated_rows = nrow(model_data),
    original_df = original_df
  )
}

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

adjusted_specs <- list(
  list(
    panel = "Overall", col = NA_character_, cats = "Overall",
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Age", col = "age_display",
    cats = c("All ages", "0-17 years", "18-64 years", "\u226565 years"),
    covars = c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Vaccine type", col = "vaccine_type_display",
    cats = c("Trivalent", "Quadrivalent", "Unknown"),
    covars = c("age_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Influenza type", col = "pathogen_display",
    cats = c("IAV", "A/H1N1", "A/H3N2", "IBV", "B/Victoria", "Unknown"),
    covars = c("age_display", "vaccine_type_display", "region_short", "outcome_display")
  ),
  list(
    panel = "WHO region", col = "region_short",
    cats = c("EURO", "AMRO", "WPRO", "SEARO"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")
  ),
  list(
    panel = "Clinical outcome", col = "outcome_display",
    cats = c("Outpatient", "Inpatient", "Severe and Death", "Unknown"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short")
  )
)

adjusted_influenza_mask <- function(data, category) {
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
if (sensitivity_mode == "s8_nos_ge7") {
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
