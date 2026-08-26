#!/usr/bin/env Rscript

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
raw <- read_source_verified_analysis_data(root)
source_audit <- attr(raw, "source_audit")

count_cols <- c("cases_vaccinated", "controls_vaccinated", "cases_unvaccinated", "controls_unvaccinated")
raw[count_cols] <- lapply(raw[count_cols], as_num)

case_control <- raw$study_design == "Case-control study"
valid_counts <- case_control &
  stats::complete.cases(raw[, count_cols]) &
  raw$cases_vaccinated >= 0 &
  raw$controls_vaccinated >= 0 &
  raw$cases_unvaccinated >= 0 &
  raw$controls_unvaccinated >= 0

dat <- raw[valid_counts, , drop = FALSE]
zero_cell <- dat$cases_vaccinated == 0 |
  dat$controls_vaccinated == 0 |
  dat$cases_unvaccinated == 0 |
  dat$controls_unvaccinated == 0

cc <- ifelse(zero_cell, 0.5, 0)
a <- dat$cases_vaccinated + cc
b <- dat$controls_vaccinated + cc
c <- dat$cases_unvaccinated + cc
d <- dat$controls_unvaccinated + cc

dat$yi_crude <- log((a * d) / (b * c))
dat$vi_crude <- 1 / a + 1 / b + 1 / c + 1 / d
dat$yi_log_or <- dat$yi_crude
dat$vi_log_or <- dat$vi_crude
dat$used_continuity_correction <- zero_cell
dat <- dat[is.finite(dat$yi_crude) & is.finite(dat$vi_crude) & dat$vi_crude > 0, , drop = FALSE]

prefix <- "TableS7_crude_or_abcd_multilevel_random_effects"
out_dir <- file.path(root, "Results", "TablesS6-S8", "TableS7")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_csv <- file.path(out_dir, paste0(prefix, ".csv"))
out_xlsx <- file.path(out_dir, paste0(prefix, ".xlsx"))
audit_csv <- file.path(out_dir, paste0(prefix, "_audit.csv"))
partial_csv <- file.path(out_dir, paste0(prefix, "_partial.csv"))
progress_log <- file.path(out_dir, paste0(prefix, "_progress.log"))
adjusted_period_csv <- file.path(
  out_dir,
  "TableS7_crude_or_abcd_adjusted_period_effects.csv"
)
adjusted_period_failed_csv <- file.path(
  out_dir,
  "TableS7_crude_or_abcd_adjusted_period_effects_failed_or_sparse.csv"
)

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
      "Fitting Table S7 crude OR | section=", section_name, " | subgroup=", variable,
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
      "Finished Table S7 crude OR | section=", section_name, " | subgroup=", variable,
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

# Refit the Table 1 multivariable period models using the crude log(OR) and
# its sampling variance calculated above. The subgroup definitions,
# covariate sets, random effects, REML estimator, sufficient-statistic
# aggregation, and original-row t inference are the same as in Table 1.
model_dat <- dat |>
  dplyr::mutate(
    period = factor(
      .data$period_original,
      levels = c(0, 1),
      labels = c("Pre-pandemic", "Pandemic/Post-Pandemic")
    ),
    age_display = factor(
      .data$age_label,
      levels = c("All ages", "0-17 years", "18-64 years", "≥65 years", "0-64 years & ≥18 years")
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
        .data$pathogen_specific == "B(Yamagata)" ~ "B/Yamagata",
        .data$pathogen_specific == "B(Victoria)" ~ "B/Victoria",
        TRUE ~ NA_character_
      ),
      levels = c("A/H1N1", "A/H3N2", "B/Yamagata", "B/Victoria")
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
  )

fit_crude_period_model <- function(sub, covariates) {
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
  fit_data <- sub |>
    dplyr::group_by(dplyr::across(dplyr::all_of(aggregation_columns))) |>
    dplyr::summarise(
      yi_log_or = sum(.data$yi_log_or / .data$vi_log_or) / sum(1 / .data$vi_log_or),
      vi_log_or = 1 / sum(1 / .data$vi_log_or),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season = droplevels(factor(.data$season))
    )

  fit <- metafor::rma.mv(
    yi = yi_log_or,
    V = vi_log_or,
    mods = mods_formula,
    random = list(~ 1 | study_id, ~ 1 | season),
    data = fit_data,
    method = "REML",
    test = "t",
    sparse = TRUE
  )
  original_df <- nrow(sub) - ncol(fit$X)
  if (!is.finite(original_df) || original_df <= 0) {
    stop("Insufficient pre-aggregation residual degrees of freedom.", call. = FALSE)
  }
  critical <- stats::qt(0.975, original_df)
  fit$ci.lb <- as.numeric(fit$b[, 1]) - critical * fit$se
  fit$ci.ub <- as.numeric(fit$b[, 1]) + critical * fit$se
  fit$pval <- 2 * stats::pt(
    -abs(as.numeric(fit$b[, 1]) / fit$se), df = original_df
  )

  list(
    fit = fit,
    active_covariates = active_covariates,
    aggregated_rows = nrow(fit_data),
    original_df = original_df
  )
}

crude_period_row <- function(fit, panel, subgroup, sub) {
  term <- "periodPandemic/Post-Pandemic"
  idx <- match(term, rownames(fit$b))
  if (is.na(idx)) stop("Period coefficient was not estimable for ", panel, " - ", subgroup)

  beta <- as.numeric(fit$b[idx, 1])
  ci_lb <- fit$ci.lb[idx]
  ci_ub <- fit$ci.ub[idx]
  tibble::tibble(
    Panel = panel,
    Subgroup = subgroup,
    No_of_studies = dplyr::n_distinct(sub$study_id),
    No_of_estimates = nrow(sub),
    No_of_seasons = dplyr::n_distinct(sub$season),
    Coefficient_logOR = fmt_num(beta),
    SE = fmt_num(fit$se[idx]),
    t_statistic = fmt_num(beta / fit$se[idx]),
    p_value = fmt_p(fit$pval[idx]),
    CI_95_logOR = paste0("(", fmt_num(ci_lb), ", ", fmt_num(ci_ub), ")"),
    VE_change_percent = (1 - exp(beta)) * 100,
    VE_change_lower_percent = (1 - exp(ci_ub)) * 100,
    VE_change_upper_percent = (1 - exp(ci_lb)) * 100,
    Converted_VE_change_percent = fmt_ci(
      (1 - exp(beta)) * 100,
      (1 - exp(ci_ub)) * 100,
      (1 - exp(ci_lb)) * 100
    ),
    Model = paste(panel, subgroup, sep = " - ")
  )
}

period_specs <- list(
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
    cats = c("Trivalent", "Quadrivalent", "Other"),
    covars = c("age_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Influenza type", col = "pathogen_display",
    cats = c("IAV", "A/H1N1", "A/H3N2", "IBV", "B/Yamagata", "B/Victoria"),
    covars = c("age_display", "vaccine_type_display", "region_short", "outcome_display")
  ),
  list(
    panel = "WHO region", col = "region_short",
    # Match the Table 1 adjusted-period specification. AFRO and EMRO remain
    # in the period-specific pooled table but do not receive adjusted effects.
    cats = c("EURO", "AMRO", "WPRO", "SEARO"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")
  ),
  list(
    panel = "Clinical outcome", col = "outcome_display",
    cats = c("Outpatient", "Inpatient", "Severe and Death"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short")
  )
)

crude_influenza_mask <- function(data, category) {
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
  if (category %in% c("A/H1N1", "A/H3N2", "B/Yamagata")) {
    return(
      !is.na(data$pathogen_specific_display) &
        as.character(data$pathogen_specific_display) == category
    )
  }
  !is.na(data$pathogen_display) & as.character(data$pathogen_display) == category
}

adjusted_rows <- list()
adjusted_failed_rows <- list()
for (spec in period_specs) {
  for (category in spec$cats) {
    sub <- if (is.na(spec$col)) {
      model_dat
    } else if (spec$panel == "Influenza type") {
      model_dat[crude_influenza_mask(model_dat, category), , drop = FALSE]
    } else {
      mask <- !is.na(model_dat[[spec$col]]) & as.character(model_dat[[spec$col]]) == category
      model_dat[mask, , drop = FALSE]
    }

    model_name <- paste(spec$panel, category, sep = " - ")
    log_progress(
      "Fitting Table S7 adjusted crude-OR period model | model=", model_name,
      " | estimates=", nrow(sub),
      " | studies=", dplyr::n_distinct(sub$study_id),
      " | seasons=", dplyr::n_distinct(sub$season)
    )

    sparse_reason <- NULL
    if (nrow(sub) <= 2 || dplyr::n_distinct(sub$period) < 2 ||
        dplyr::n_distinct(sub$study_id) < 2 || dplyr::n_distinct(sub$season) < 2) {
      sparse_reason <- "Need both periods, >2 estimates, >=2 study IDs, and >=2 seasons"
      fit <- NULL
    } else {
      fit <- tryCatch(fit_crude_period_model(sub, spec$covars), error = function(e) e)
      if (inherits(fit, "error")) sparse_reason <- conditionMessage(fit)
    }

    if (!is.null(sparse_reason)) {
      adjusted_failed_rows[[length(adjusted_failed_rows) + 1]] <- tibble::tibble(
        Model = model_name,
        Panel = spec$panel,
        Subgroup = category,
        No_of_estimates = nrow(sub),
        No_of_studies = dplyr::n_distinct(sub$study_id),
        No_of_seasons = dplyr::n_distinct(sub$season),
        Note = sparse_reason
      )
      log_progress("Skipped Table S7 adjusted crude-OR period model | model=", model_name, " | reason=", sparse_reason)
      next
    }

    adjusted_rows[[length(adjusted_rows) + 1]] <- crude_period_row(
      fit$fit, spec$panel, category, sub
    )
    log_progress(
      "Finished Table S7 adjusted crude-OR period model | model=", model_name,
      " | sufficient-statistic rows=", fit$aggregated_rows,
      " | residual df=", fit$original_df
    )
  }
}

adjusted_period_effects <- dplyr::bind_rows(adjusted_rows)
adjusted_period_failed <- dplyr::bind_rows(adjusted_failed_rows)
readr::write_excel_csv(adjusted_period_effects, adjusted_period_csv, na = "")
if (nrow(adjusted_period_failed) > 0) {
  readr::write_excel_csv(adjusted_period_failed, adjusted_period_failed_csv, na = "")
}

audit <- tibble::tibble(
  metric = c(
    "analysis", "input_file", "source_rows", "source_logor_eligible_rows",
    "source_logor_excluded_rows", "eligible_case_control_rows",
    "eligible_case_control_studies", "cohort_rows_excluded_by_design",
    "case_control_rows_with_complete_nonnegative_counts",
    "case_control_studies_used", "rows_used_for_crude_or",
    "case_control_rows_excluded_missing_or_invalid_counts",
    "rows_with_zero_cell_continuity_correction", "effect_measure", "model", "random_effects",
    "adjusted_period_model", "adjusted_period_effect", "output_rows"
  ),
  value = c(
    "Table S7 case-control-only sensitivity: crude OR calculated from abcd count cells",
    source_audit$input_file, source_audit$source_rows,
    source_audit$source_logor_eligible_rows, source_audit$source_logor_excluded_rows,
    sum(case_control), dplyr::n_distinct(raw$study_id[case_control]),
    sum(!case_control), sum(valid_counts), dplyr::n_distinct(dat$study_id),
    nrow(dat), sum(case_control) - sum(valid_counts),
    sum(dat$used_continuity_correction),
    "log crude OR = log((cases_vaccinated * controls_unvaccinated) / (controls_vaccinated * cases_unvaccinated)); vi = 1/a + 1/b + 1/c + 1/d",
    "metafor::rma.mv multilevel random-effects meta-analysis",
    "random intercepts for study_id and season",
    "subgroup-specific multivariable metafor::rma.mv with the same covariate sets as Table 1; REML; model-based pre-aggregation residual df",
    "100 * [1 - exp(beta_period)] for pandemic/post-pandemic versus pre-pandemic",
    nrow(table_out)
  )
)
readr::write_excel_csv(audit, audit_csv, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Table S7", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Audit", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Adjusted period effects", gridLines = FALSE)
openxlsx::writeData(wb, "Table S7", table_out)
openxlsx::writeData(wb, "Audit", audit)
openxlsx::writeData(wb, "Adjusted period effects", adjusted_period_effects)
openxlsx::setColWidths(wb, "Table S7", cols = 1:ncol(table_out), widths = "auto")
openxlsx::setColWidths(wb, "Audit", cols = 1:2, widths = "auto")
openxlsx::freezePane(wb, "Table S7", firstRow = TRUE)
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Wrote: ", out_csv)
message("Wrote: ", out_xlsx)
message("Wrote: ", audit_csv)
message("Wrote: ", adjusted_period_csv)
message("Progress log: ", progress_log)
