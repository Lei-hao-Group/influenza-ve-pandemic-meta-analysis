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
source_dat <- read_source_verified_analysis_data(root)
source_audit <- attr(source_dat, "source_audit")
dat <- source_dat |>
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

out_dir <- file.path(root, "Results", "Figure3", "multivariable")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_full_csv <- file.path(out_dir, "Figure3_multivariable_rma_mv_full_parameters.csv")
out_period_csv <- file.path(out_dir, "Figure3_multivariable_rma_mv_period_effects.csv")
out_failed_csv <- file.path(out_dir, "Figure3_multivariable_rma_mv_failed_or_sparse.csv")
out_xlsx <- file.path(out_dir, "Figure3_multivariable_rma_mv_outputs.xlsx")

display_var <- c(
  period = "Period",
  age_display = "Age group",
  vaccine_type_display = "Vaccine type",
  pathogen_display = "Influenza type",
  region_short = "WHO region",
  outcome_display = "Clinical outcome"
)

fit_period_model <- function(sub, covariates) {
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
    active_covariates = active_covariates,
    mods_formula = mods_formula,
    aggregated_rows = nrow(model_data),
    original_df = original_df
  )
}

coef_row <- function(model, term_name, variable, category, is_reference = FALSE) {
  if (is_reference) {
    return(tibble::tibble(
      Variable = variable,
      Category = category,
      Coefficient_logOR = "Ref",
      SE = no_data,
      t_statistic = no_data,
      p_value = no_data,
      CI_95_logOR = no_data,
      OR = no_data,
      CI_95_OR = no_data,
      Converted_VE_change_percent = no_data
    ))
  }

  terms <- rownames(model$b)
  if (!term_name %in% terms) {
    return(tibble::tibble(
      Variable = variable,
      Category = category,
      Coefficient_logOR = no_data,
      SE = no_data,
      t_statistic = no_data,
      p_value = no_data,
      CI_95_logOR = no_data,
      OR = no_data,
      CI_95_OR = no_data,
      Converted_VE_change_percent = no_data
    ))
  }

  idx <- match(term_name, terms)
  beta <- as.numeric(model$b[idx, 1])
  se <- model$se[idx]
  ci_lb <- model$ci.lb[idx]
  ci_ub <- model$ci.ub[idx]
  or <- exp(beta)
  or_lb <- exp(ci_lb)
  or_ub <- exp(ci_ub)

  tibble::tibble(
    Variable = variable,
    Category = category,
    Coefficient_logOR = fmt_num(beta),
    SE = fmt_num(se),
    t_statistic = fmt_num(beta / se),
    p_value = fmt_p(model$pval[idx]),
    CI_95_logOR = paste0("(", fmt_num(ci_lb), ", ", fmt_num(ci_ub), ")"),
    OR = fmt_num(or),
    CI_95_OR = paste0("(", fmt_num(or_lb), ", ", fmt_num(or_ub), ")"),
    Converted_VE_change_percent = fmt_ci((1 - or) * 100, (1 - or_ub) * 100, (1 - or_lb) * 100)
  )
}

extract_full_parameters <- function(model, active_covariates, table_name, panel, subgroup, sub) {
  rows <- list(coef_row(model, "intrcpt", "Intercept", "", FALSE))
  model_vars <- c("period", active_covariates)

  for (var in model_vars) {
    levs <- levels(droplevels(factor(sub[[var]])))
    levs <- levs[!is.na(levs)]
    if (length(levs) == 0) next

    rows[[length(rows) + 1]] <- coef_row(model, NA_character_, display_var[[var]], levs[[1]], TRUE)
    if (length(levs) > 1) {
      for (lvl in levs[-1]) {
        rows[[length(rows) + 1]] <- coef_row(model, paste0(var, lvl), display_var[[var]], lvl, FALSE)
      }
    }
  }

  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      Model = table_name,
      Panel = panel,
      Subgroup = subgroup,
      No_of_estimates = nrow(sub),
      No_of_studies = dplyr::n_distinct(sub$study_id),
      No_of_seasons = dplyr::n_distinct(sub$season),
      .before = 1
    )
}

period_effect_row <- function(model, table_name, panel, subgroup, sub) {
  term <- "periodPandemic/Post-Pandemic"
  if (!term %in% rownames(model$b)) {
    return(tibble::tibble(
      Panel = panel,
      Subgroup = subgroup,
      No_of_studies = dplyr::n_distinct(sub$study_id),
      No_of_estimates = nrow(sub),
      No_of_seasons = dplyr::n_distinct(sub$season),
      Coefficient_logOR = no_data,
      SE = no_data,
      t_statistic = no_data,
      p_value = no_data,
      CI_95_logOR = no_data,
      VE_change_percent = NA_real_,
      VE_change_lower_percent = NA_real_,
      VE_change_upper_percent = NA_real_,
      Converted_VE_change_percent = no_data,
      Model = table_name
    ))
  }
  idx <- match(term, rownames(model$b))
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
    Converted_VE_change_percent = fmt_ci((1 - exp(beta)) * 100, (1 - exp(ci_ub)) * 100, (1 - exp(ci_lb)) * 100),
    Model = table_name
  )
}

specs <- list(
  list(
    panel = "Overall",
    col = NA_character_,
    cats = "Overall",
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Age",
    col = "age_display",
    cats = c("All ages", "0-17 years", "18-64 years", "\u226565 years"),
    covars = c("vaccine_type_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Vaccine type",
    col = "vaccine_type_display",
    cats = c("Trivalent", "Quadrivalent", "Unknown"),
    covars = c("age_display", "pathogen_display", "region_short", "outcome_display")
  ),
  list(
    panel = "Influenza type",
    col = "pathogen_display",
    # IAV and IBV retain the original broad-type definitions. The three
    # subtype rows are additional, nested analyses. The B/Victoria row uses
    # explicit B(Victoria) before the pandemic and
    # all IBV estimates during/after the pandemic.
    cats = c("IAV", "A/H1N1", "A/H3N2", "IBV", "B/Victoria", "Unknown"),
    covars = c("age_display", "vaccine_type_display", "region_short", "outcome_display")
  ),
  list(
    panel = "WHO region",
    col = "region_short",
    cats = c("EURO", "AMRO", "WPRO", "SEARO"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "outcome_display")
  ),
  list(
    panel = "Clinical outcome",
    col = "outcome_display",
    cats = c("Outpatient", "Inpatient", "Severe and Death", "Unknown"),
    covars = c("age_display", "vaccine_type_display", "pathogen_display", "region_short")
  )
)

full_tables <- list()
period_tables <- list()
failed_models <- list()

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
  !is.na(data$pathogen_display) &
    as.character(data$pathogen_display) == category
}

for (spec in specs) {
  for (cat in spec$cats) {
    sub <- if (is.na(spec$col)) {
      dat
    } else if (spec$panel == "Influenza type") {
      dat[influenza_type_mask(dat, cat), , drop = FALSE]
    } else {
      subgroup_mask <- !is.na(dat[[spec$col]]) & as.character(dat[[spec$col]]) == cat
      dat[subgroup_mask, , drop = FALSE]
    }
    table_name <- paste(spec$panel, cat, sep = " - ")
    message("Fitting Figure 3 model: ", table_name, " (k=", nrow(sub), ")")

    if (nrow(sub) <= 2 || dplyr::n_distinct(sub$period) < 2 ||
        dplyr::n_distinct(sub$study_id) < 2 || dplyr::n_distinct(sub$season) < 2) {
      message(
        "Skipped Figure 3 model: ", table_name,
        " | reason=Need both periods, >2 estimates, >=2 study IDs, and >=2 seasons"
      )
      failed_models[[length(failed_models) + 1]] <- tibble::tibble(
        Model = table_name,
        Panel = spec$panel,
        Subgroup = cat,
        No_of_estimates = nrow(sub),
        No_of_studies = dplyr::n_distinct(sub$study_id),
        No_of_seasons = dplyr::n_distinct(sub$season),
        Note = "Need both periods, >2 estimates, >=2 study IDs, and >=2 seasons"
      )
      next
    }

    fit <- tryCatch(fit_period_model(sub, spec$covars), error = function(e) e)
    if (inherits(fit, "error")) {
      message("Failed Figure 3 model: ", table_name, " | reason=", conditionMessage(fit))
      failed_models[[length(failed_models) + 1]] <- tibble::tibble(
        Model = table_name,
        Panel = spec$panel,
        Subgroup = cat,
        No_of_estimates = nrow(sub),
        No_of_studies = dplyr::n_distinct(sub$study_id),
        No_of_seasons = dplyr::n_distinct(sub$season),
        Note = conditionMessage(fit)
      )
      next
    }

    full_tables[[length(full_tables) + 1]] <- extract_full_parameters(
      fit$model, fit$active_covariates, table_name, spec$panel, cat, sub
    )
    period_tables[[length(period_tables) + 1]] <- period_effect_row(
      fit$model, table_name, spec$panel, cat, sub
    )
    message(
      "Finished Figure 3 model: ", table_name,
      " | estimates=", nrow(sub),
      " | sufficient-statistic rows=", fit$aggregated_rows,
      " | studies=", dplyr::n_distinct(sub$study_id),
      " | seasons=", dplyr::n_distinct(sub$season)
    )
  }
}

full_parameters <- dplyr::bind_rows(full_tables)
period_effects <- dplyr::bind_rows(period_tables)
failed_models <- dplyr::bind_rows(failed_models)

readr::write_excel_csv(full_parameters, out_full_csv, na = "")
readr::write_excel_csv(period_effects, out_period_csv, na = "")
if (nrow(failed_models) > 0) readr::write_excel_csv(failed_models, out_failed_csv, na = "")

wb <- openxlsx::createWorkbook()
add_sheet <- function(name, x) {
  openxlsx::addWorksheet(wb, substr(name, 1, 31), gridLines = FALSE)
  openxlsx::writeData(wb, substr(name, 1, 31), x)
  openxlsx::setColWidths(wb, substr(name, 1, 31), cols = 1:ncol(x), widths = "auto")
  openxlsx::freezePane(wb, substr(name, 1, 31), firstRow = TRUE)
}
add_sheet("Period effects", period_effects)
add_sheet("Full parameters", full_parameters)
if (nrow(failed_models) > 0) add_sheet("Failed or sparse", failed_models)
analysis_audit <- tibble::tibble(
  metric = c(
    "input_file", "source_rows", "source_logor_eligible_rows",
    "source_logor_excluded_rows", "rows_used", "unique_studies_used",
    "unique_seasons_used", "included_design", "model", "random_effects", "estimation",
    "computational_equivalence", "influenza_type_subgroups",
    "A/H1N1_definition", "B/Victoria_definition"
  ),
  value = c(
    source_audit$input_file,
    source_audit$source_rows,
    source_audit$source_logor_eligible_rows,
    source_audit$source_logor_excluded_rows,
    nrow(dat),
    dplyr::n_distinct(dat$study_id),
    dplyr::n_distinct(dat$season),
    paste(
      "Case-control and cohort studies retained; VE is reconstructed as the",
      "underlying effect ratio and must not be described uniformly as an OR"
    ),
    "subgroup-specific multivariable metafor::rma.mv meta-regression",
    "random intercepts for study_id and season",
    "restricted maximum likelihood (REML), test='t'; model-based pre-aggregation residual df",
    paste(
      "inverse-variance sufficient-statistic aggregation within identical",
      "study_id-by-season-by-model-moderator cells"
    ),
    paste(
      "IAV and IBV are broad, nested groups from Pathogen; A/H1N1, A/H3N2,",
      "and B/Victoria are additional groups from Pathogen_specific"
    ),
    "Pathogen_specific in {A(H1N1), A(pH1N1)}",
    paste(
      "pre-pandemic: explicit Pathogen_specific B(Victoria);",
      "pandemic/post-pandemic: all Pathogen == IBV"
    )
  )
)
add_sheet("Audit", analysis_audit)
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Wrote: ", out_period_csv)
message("Wrote: ", out_full_csv)
if (file.exists(out_failed_csv)) message("Wrote: ", out_failed_csv)
message("Wrote: ", out_xlsx)
