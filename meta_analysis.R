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

# Table 1, full regression coefficients (Part 10), Tables S20 and S21.

pooled_ve <- function() {
  check_required_packages(extra = c("openxlsx", "readxl"))

  source_verified_file <- file.path(
    "Data",
    "review_data.xlsx"
  )
  root <- get_project_root(input_csv = source_verified_file)
  dat <- read_source_verified_analysis_data(root)
  source_audit <- attr(dat, "source_audit")

  out_dir <- file.path(results_root(root), "Table1")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_csv <- file.path(out_dir, "Table1_pooled_ve.csv")
  out_xlsx <- file.path(out_dir, "Table1_pooled_ve.xlsx")
  audit_csv <- file.path(out_dir, "Table1_pooled_ve_audit.csv")
  partial_csv <- file.path(out_dir, "Table1_pooled_ve_partial.csv")
  progress_log <- file.path(out_dir, "Table1_pooled_ve_progress.log")

  log_progress <- function(...) {
    msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
    message(msg)
    cat(msg, "\n", file = progress_log, append = TRUE)
  }

  write_partial <- function(rows) {
    if (length(rows) == 0) return(invisible(FALSE))
    partial <- dplyr::bind_rows(rows)
    readr::write_excel_csv(partial, partial_csv, na = "")
    invisible(TRUE)
  }

  if (file.exists(progress_log)) file.remove(progress_log)
  if (file.exists(partial_csv)) file.remove(partial_csv)

  section_row <- function(section) {
    tibble::tibble(
      Variable = section,
      Period = section,
      `No. of studies` = section,
      `No. of datasets` = section,
      `No. of seasons` = section,
      `Pooled VE (%)` = section,
      `I^2` = section,
      `p value for heterogeneity` = section,
      `p value for effect` = section,
      `tau^2 study` = section,
      `tau^2 season` = section,
      `Model status` = section
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
        "Fitting Table 1 | section=", section_name,
        " | subgroup=", variable,
        " | period=", period_label,
        " | estimates=", nrow(sub),
        " | studies=", dplyr::n_distinct(sub$study_id),
        " | seasons=", dplyr::n_distinct(sub$season)
      )
      fit_obj <- fit_multilevel_meta(sub, aggregate_random_effect_cells = TRUE)
      rows[[length(rows) + 1]] <- format_result_row(
        variable = variable,
        period = period_label,
        fit_obj = fit_obj
      )
      log_progress(
        "Finished Table 1 | section=", section_name,
        " | subgroup=", variable,
        " | period=", period_label,
        " | status=", rows[[length(rows)]]$`Model status`
      )
      write_partial(rows)
    }
    rows
  }

  rows <- list()

  rows[[length(rows) + 1]] <- section_row("Overall")
  write_partial(rows)
  rows <- add_pair(rows, "Overall", "Overall", function(x) rep(TRUE, nrow(x)))

  rows[[length(rows) + 1]] <- section_row("Age")
  write_partial(rows)
  rows <- add_pair(rows, "Age", "All ages", function(x) x$age_label == "All ages")
  rows <- add_pair(rows, "Age", "0-17 years", function(x) x$age_label == "0-17 years")
  rows <- add_pair(rows, "Age", "18-64 years", function(x) x$age_label == "18-64 years")
  rows <- add_pair(rows, "Age", "\u226565 years", function(x) x$age_label == "\u226565 years")

  rows[[length(rows) + 1]] <- section_row("Vaccine type")
  write_partial(rows)
  rows <- add_pair(rows, "Vaccine type", "Trivalent", function(x) x$vaccine_label == "Trivalent")
  rows <- add_pair(rows, "Vaccine type", "Quadrivalent", function(x) x$vaccine_label == "Quadrivalent")
  rows <- add_pair(rows, "Vaccine type", "Other", function(x) x$vaccine_label == "Other")

  rows[[length(rows) + 1]] <- section_row("Influenza type")
  write_partial(rows)
  rows <- add_pair(rows, "Influenza type", "IAV", function(x) x$pathogen == "IAV")
  rows <- add_pair(rows, "Influenza type", "A/H1N1", function(x) x$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)"))
  rows <- add_pair(rows, "Influenza type", "A/H3N2", function(x) x$pathogen_specific == "A(H3N2)")
  rows <- add_pair(rows, "Influenza type", "IBV", function(x) x$pathogen == "IBV")
  rows <- add_pair(rows, "Influenza type", "B/Yamagata", function(x) x$pathogen_specific == "B(Yamagata)")
  rows <- add_pair(rows, "Influenza type", "B/Victoria", function(x) x$pathogen_specific == "B(Victoria)")

  rows[[length(rows) + 1]] <- section_row("WHO region")
  write_partial(rows)
  rows <- add_pair(rows, "WHO region", "EURO", function(x) x$who_label == "EURO")
  rows <- add_pair(rows, "WHO region", "AMRO", function(x) x$who_label == "AMRO")
  rows <- add_pair(rows, "WHO region", "WPRO", function(x) x$who_label == "WPRO")
  rows <- add_pair(rows, "WHO region", "SEARO", function(x) x$who_label == "SEARO")
  rows <- add_pair(rows, "WHO region", "AFRO", function(x) x$who_label == "AFRO")
  rows <- add_pair(rows, "WHO region", "EMRO", function(x) x$who_label == "EMRO")

  rows[[length(rows) + 1]] <- section_row("Clinical outcome")
  write_partial(rows)
  rows <- add_pair(rows, "Clinical outcome", "Outpatient", function(x) x$outcome_label == "Outpatient")
  rows <- add_pair(rows, "Clinical outcome", "Inpatient", function(x) x$outcome_label == "Inpatient")
  rows <- add_pair(rows, "Clinical outcome", "Severe and Death", function(x) x$outcome_label %in% c("Severe", "Death", "Severe and Death"))

  pooled <- dplyr::bind_rows(rows)
  # The Other vaccine row remains in Table S16 but was removed from Table 1.
  cr2_dir <- file.path(results_root(root), "TableS16")
  dir.create(cr2_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(pooled, file.path(cr2_dir, "TableS16_pooled_ve.csv"), na = "")
  table1 <- dplyr::filter(pooled, .data$Variable != "Other")
  readr::write_excel_csv(table1, out_csv, na = "")

  audit <- tibble::tibble(
    metric = c(
      "input_file",
      "source_rows",
      "source_logor_eligible_rows",
      "source_logor_excluded_rows",
      "inclusion_rule",
      "rows_used",
      "unique_studies_used",
      "unique_seasons_used",
      "effect_measure",
      "model",
      "random_effects",
      "computational_equivalence",
      "estimation",
      "table_rows"
    ),
    value = c(
      source_audit$input_file,
      source_audit$source_rows,
      source_audit$source_logor_eligible_rows,
      source_audit$source_logor_excluded_rows,
      "Use in log-ratio synthesis == Yes; finite transformed log(effect ratio); positive finite variance; Epi in pre or pan/post",
      nrow(dat),
      dplyr::n_distinct(dat$study_id),
      dplyr::n_distinct(dat$season),
      paste(
        "log of the reported VE-defining effect ratio (OR, RR, HR, or IRR as",
        "applicable), converted to VE as (1 - exp[log ratio]) * 100"
      ),
      "metafor::rma.mv intercept-only multilevel random-effects meta-analysis",
      "random intercepts for study_id and season",
      "inverse-variance sufficient-statistic aggregation within identical study_id-by-season random-effect cells",
      "restricted maximum likelihood (REML), test = 't'; model-based pre-aggregation residual df",
      nrow(table1)
    )
  )
  readr::write_excel_csv(audit, audit_csv, na = "")

  add_excel_sheets(out_xlsx, list("Table 1" = table1, "Audit" = audit))

  message("Wrote: ", out_csv)
  message("Wrote: ", out_xlsx)
  message("Wrote: ", audit_csv)
}

align_primary_reference_levels <- function(sub, panel, subgroup) {
  # Part 10 of the 14862 clean appendix uses WPRO as the regional reference
  # in these two models, where the overall Multi-region reference is absent.
  if ((panel == "Vaccine type" && subgroup == "Quadrivalent") ||
      (panel == "Clinical outcome" && subgroup == "Severe and Death")) {
    regional_levels <- c("WPRO", "EMRO", "EURO", "AMRO")
    if (!"WPRO" %in% sub$region_short ||
        !all(as.character(sub$region_short) %in% regional_levels)) {
      stop("Regional levels do not match the manuscript reference specification.")
    }
    sub$region_short <- factor(as.character(sub$region_short), levels = regional_levels)
  }
  sub
}

adjusted_period_effects <- function() {
  check_required_packages(extra = c("openxlsx", "readxl"))

  source_verified_file <- file.path(
    "Data",
    "review_data.xlsx"
  )
  root <- get_project_root(input_csv = source_verified_file)
  source_dat <- read_source_verified_analysis_data(root)
  source_audit <- attr(source_dat, "source_audit")
  dat <- prepare_period_data(source_dat)

  out_dir <- file.path(results_root(root), "Table1")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  output_prefix <- "Table1_adjusted_rma_mv"
  out_full_csv <- file.path(out_dir, paste0(output_prefix, "_full_parameters.csv"))
  out_period_csv <- file.path(out_dir, paste0(output_prefix, "_period_effects.csv"))
  out_failed_csv <- file.path(out_dir, paste0(output_prefix, "_failed_or_sparse.csv"))
  out_xlsx <- file.path(out_dir, paste0(output_prefix, "_outputs.xlsx"))
  # Table S20: generalized variance inflation factors for every adjusted model.
  gvif_dir <- file.path(results_root(root), "TableS20")
  dir.create(gvif_dir, showWarnings = FALSE, recursive = TRUE)
  out_gvif_csv <- file.path(gvif_dir, "TableS20_gvif_all_adjusted_models.csv")

  # GVIFs are computed jointly for the coefficients of each moderator, holding
  # the estimated variance components fixed (reestimate = FALSE).

  specs <- period_model_specs()

  full_tables <- list()
  period_tables <- list()
  gvif_tables <- list()
  failed_models <- list()

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
      sub <- align_primary_reference_levels(sub, spec$panel, cat)
      table_name <- paste(spec$panel, cat, sep = " - ")
      message("Fitting adjusted period model: ", table_name, " (k=", nrow(sub), ")")

      if (nrow(sub) <= 2 || dplyr::n_distinct(sub$period) < 2 ||
          dplyr::n_distinct(sub$study_id) < 2 || dplyr::n_distinct(sub$season) < 2) {
        message(
          "Skipped adjusted period model: ", table_name,
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
        message("Failed adjusted period model: ", table_name, " | reason=", conditionMessage(fit))
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
      gvif_tables[[length(gvif_tables) + 1]] <- gvif_rows(
        fit$model, fit$active_covariates, table_name, spec$panel, cat
      )
      message(
        "Finished adjusted period model: ", table_name,
        " | estimates=", nrow(sub),
        " | sufficient-statistic rows=", fit$aggregated_rows,
        " | studies=", dplyr::n_distinct(sub$study_id),
        " | seasons=", dplyr::n_distinct(sub$season)
      )
    }
  }

  # Unknown-only subgroup coefficients are absent from Table 1 and Part 10.
  # Their GVIFs are still reported in Table S20; Unknown moderator levels stay.
  full_parameters <- dplyr::bind_rows(full_tables) |>
    dplyr::filter(.data$Subgroup != "Unknown")
  period_effects <- dplyr::bind_rows(period_tables) |>
    dplyr::filter(.data$Subgroup != "Unknown")
  gvif <- dplyr::bind_rows(gvif_tables)
  failed_models <- dplyr::bind_rows(failed_models)

  readr::write_excel_csv(full_parameters, out_full_csv, na = "")
  readr::write_excel_csv(period_effects, out_period_csv, na = "")
  readr::write_excel_csv(gvif, out_gvif_csv, na = "")
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
  add_sheet("GVIF", gvif)
  if (nrow(failed_models) > 0) add_sheet("Failed or sparse", failed_models)
  analysis_audit <- tibble::tibble(
    metric = c(
      "input_file", "source_rows", "source_logor_eligible_rows",
      "source_logor_excluded_rows", "rows_used", "unique_studies_used",
      "unique_seasons_used", "season_definition", "included_design", "model", "random_effects", "estimation",
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
      "Original reported study-period text, including single-season and multi-season labels",
      paste(
        "Case-control and cohort studies retained; VE is reconstructed as the",
        "underlying effect ratio and must not be described uniformly as an OR"
      ),
      "subgroup-specific multivariable metafor::rma.mv meta-regression",
      "random intercepts for study_id and reported study period",
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
  message("Wrote: ", out_gvif_csv)
  if (file.exists(out_failed_csv)) message("Wrote: ", out_failed_csv)
  message("Wrote: ", out_xlsx)
}

alternative_references <- function() {
  check_required_packages(c("readxl", "openxlsx"))
  root <- get_project_root()
  out_dir <- file.path(results_root(root), "TableS21")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dat <- prepare_period_data(read_source_verified_analysis_data(root))

  covariates <- c(
    "age_display", "vaccine_type_display", "pathogen_display",
    "region_short", "outcome_display"
  )
  reference_sets <- list(
    list(
      specification = 1L,
      label = "All ages (≥6 mo) / Unknown / Unknown / Multi-region / Unknown",
      refs = c(age_display = "All ages", vaccine_type_display = "Unknown",
               pathogen_display = "Unknown", region_short = "Multi-region",
               outcome_display = "Unknown")
    ),
    list(
      specification = 2L,
      label = "All ages (≥6 mo) / Trivalent / IAV / AMRO / Outpatient",
      refs = c(age_display = "All ages", vaccine_type_display = "Trivalent",
               pathogen_display = "IAV", region_short = "AMRO",
               outcome_display = "Outpatient")
    ),
    list(
      specification = 3L,
      label = "18–64 y / Quadrivalent / IBV / EURO / Inpatient",
      refs = c(age_display = "18-64 years", vaccine_type_display = "Quadrivalent",
               pathogen_display = "IBV", region_short = "EURO",
               outcome_display = "Inpatient")
    ),
    list(
      specification = 4L,
      label = "≥65 y / Trivalent / IAV / WPRO / Severe illness/death",
      refs = c(age_display = "≥65 years", vaccine_type_display = "Trivalent",
               pathogen_display = "IAV", region_short = "WPRO",
               outcome_display = "Severe and Death")
    )
  )

  fit_specification <- function(spec) {
    sub <- dat
    for (variable in names(spec$refs)) {
      sub[[variable]] <- stats::relevel(sub[[variable]], ref = spec$refs[[variable]])
    }
    message("Fitting specification ", spec$specification, ": ", spec$label)
    fit <- fit_period_model(sub, covariates)
    model <- fit$model
    period <- match("periodPandemic/Post-Pandemic", rownames(model$b))
    stopifnot(!is.na(period), rownames(model$b)[[1]] == "intrcpt")
    ve <- function(index) {
      100 * (1 - exp(c(model$b[index, 1], model$ci.ub[index], model$ci.lb[index])))
    }
    intercept <- ve(1L)
    effect <- ve(period)
    summary <- tibble::tibble(
      Specification = spec$specification,
      Reference_categories = spec$label,
      Intercept_percent = intercept[[1]],
      Intercept_lower_percent = intercept[[2]],
      Intercept_upper_percent = intercept[[3]],
      Period_coefficient_logOR = as.numeric(model$b[period, 1]),
      Period_SE = model$se[period],
      Adjusted_period_effect_percent = effect[[1]],
      Adjusted_period_effect_lower_percent = effect[[2]],
      Adjusted_period_effect_upper_percent = effect[[3]],
      Log_likelihood_REML = as.numeric(stats::logLik(model)),
      Sigma2_study = model$sigma2[[1]],
      Sigma2_reported_period = model$sigma2[[2]],
      QM = as.numeric(model$QM),
      QM_df = length(model$b) - 1L,
      No_of_estimates = nrow(sub),
      No_of_studies = dplyr::n_distinct(sub$study_id),
      No_of_seasons = dplyr::n_distinct(sub$season)
    )
    parameters <- extract_full_parameters(
      model, fit$active_covariates,
      paste("Specification", spec$specification), "Overall", "Overall", sub
    ) |>
      dplyr::mutate(
        Specification = spec$specification,
        Reference_categories = spec$label,
        .before = 1
      )
    list(summary = summary, parameters = parameters)
  }

  cores <- suppressWarnings(as.integer(Sys.getenv("VE_REFERENCE_CORES", unset = "4")))
  if (!is.finite(cores) || cores < 1L) stop("VE_REFERENCE_CORES must be a positive integer.")
  cores <- min(cores, length(reference_sets))
  fitted <- if (.Platform$OS.type == "unix" && cores > 1L) {
    parallel::mclapply(reference_sets, fit_specification, mc.cores = cores, mc.preschedule = FALSE)
  } else lapply(reference_sets, fit_specification)
  if (any(vapply(fitted, inherits, logical(1), "try-error"))) {
    stop("At least one reference-category specification failed.", call. = FALSE)
  }

  comparison <- dplyr::bind_rows(lapply(fitted, `[[`, "summary"))
  parameters <- dplyr::bind_rows(lapply(fitted, `[[`, "parameters"))

  # The period estimate and model fit must not depend on the reference categories.
  stopifnot(
    diff(range(comparison$Period_coefficient_logOR)) < 1e-4,
    diff(range(comparison$Period_SE)) < 1e-4,
    diff(range(comparison$Log_likelihood_REML)) < 1e-3
  )

  comparison_csv <- file.path(out_dir, "TableS21_model_comparison.csv")
  parameters_csv <- file.path(out_dir, "TableS21_full_parameters.csv")
  readr::write_excel_csv(comparison, comparison_csv, na = "")
  readr::write_excel_csv(parameters, parameters_csv, na = "")
  openxlsx::write.xlsx(
    list("Panel A comparison" = comparison, "Panels B-E parameters" = parameters),
    file.path(out_dir, "TableS21_alternative_reference_categories.xlsx"),
    overwrite = TRUE
  )

  print(comparison, width = Inf)
  message("Wrote: ", comparison_csv)
  message("Wrote: ", parameters_csv)
}

if (sys.nframe() == 0L) {
  pooled_ve()
  adjusted_period_effects()
  alternative_references()
}
