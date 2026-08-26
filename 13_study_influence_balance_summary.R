#!/usr/bin/env Rscript

# Complete leave-one-study-out and study-contribution-balanced analyses for
# the five prespecified Table 1 findings.

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
} else {
  file.path(getwd(), "Submission_Code")
}
source(file.path(script_dir, "05_robustness_analysis_core.R"))

root <- get_project_root(file.path(
  "Submission_Code", "Data",
  "review_data.xlsx"
))
out_dir <- file.path(
  additional_root(root), "32_study_influence_balance_summary"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summary_csv <- file.path(out_dir, "study_influence_balance_summary_all_studies.csv")
loo_csv <- file.path(out_dir, "leave_one_study_out.csv")
balance_csv <- file.path(out_dir, "study_contribution_balance_audit.csv")
audit_csv <- file.path(out_dir, "full_leave_one_study_audit.csv")
plot_csv <- file.path(out_dir, "study_influence_balance_plot_data.csv")
out_xlsx <- file.path(out_dir, "study_influence_balance_summary.xlsx")
progress_log <- file.path(out_dir, "progress.log")
if (file.exists(progress_log)) file.remove(progress_log)

log_progress <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = progress_log, append = TRUE)
}

dat <- prepare_additional_data(root) |>
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
    dfs = "contain",
    sparse = TRUE
  )
  if (!is.null(fixed_sigma2)) args$sigma2 <- fixed_sigma2
  contain_fit <- suppressWarnings(do.call(metafor::rma.mv, args))
  robust_fit <- cluster_robust_cr1(list(
    fit = contain_fit, model_data = input$aggregated
  ))
  robust_fit$contain_fit <- contain_fit
  robust_fit$sigma2 <- contain_fit$sigma2
  robust_fit
}

extract_period <- function(fit, original_rows) {
  term <- grep("^period.*Pandemic", rownames(fit$b), value = TRUE)
  if (length(term) != 1) stop("Expected exactly one pandemic/post-pandemic period coefficient")
  idx <- match(term, rownames(fit$b))
  beta <- as.numeric(fit$b[idx, 1])
  se <- as.numeric(fit$se[idx])
  df <- as.numeric(fit$ddf[idx])
  ci_lower_beta <- as.numeric(fit$ci.lb[idx])
  ci_upper_beta <- as.numeric(fit$ci.ub[idx])
  tibble::tibble(
    beta = beta,
    se = se,
    df = df,
    p = as.numeric(fit$pval[idx]),
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
