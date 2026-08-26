#!/usr/bin/env Rscript

required_packages <- c(
  "metafor", "readr", "dplyr", "stringr", "tibble"
)

check_required_packages <- function(extra = character()) {
  pkgs <- unique(c(required_packages, extra))
  available <- vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  if (!all(available)) {
    stop(
      "Missing required R packages: ",
      paste(names(available)[!available], collapse = ", "),
      "\nInstall them before running the analysis, for example:\n",
      "install.packages(c(",
      paste(sprintf('\"%s\"', names(available)[!available]), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

get_project_root <- function(input_csv = file.path(
  "Submission_Code", "Data",
  "review_data.xlsx"
)) {
  if (file.exists(file.path(getwd(), input_csv))) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)
    candidate <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(candidate, input_csv))) {
      return(candidate)
    }
  }

  stop(
    "Could not find ", input_csv, ". Run scripts from the project root, or keep the Submission_Code/ ",
    "folder directly under the project root.",
    call. = FALSE
  )
}

no_data <- "--"

as_num <- function(x) suppressWarnings(as.numeric(x))

read_study_design_map <- function(root) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required to read the study-design map.", call. = FALSE)
  }
  quality_file <- file.path(
    root, "Submission_Code", "Data",
    "Supplementary Data_Characteristics and Quality score.xlsx"
  )
  if (!file.exists(quality_file)) {
    stop("Study-characteristics workbook not found: ", quality_file, call. = FALSE)
  }

  readxl::read_excel(
    quality_file, sheet = "characteristic", skip = 2, col_types = "text"
  ) |>
    dplyr::transmute(
      study_id = trimws(as.character(.data$`Article ID`)),
      study_design = trimws(as.character(.data$`Study design`))
    ) |>
    dplyr::filter(nzchar(.data$study_id), nzchar(.data$study_design)) |>
    dplyr::distinct(.data$study_id, .keep_all = TRUE)
}

clean_label <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(
    x,
    stringr::regex("\\b(?:Unspecific|Unspecified)\\b", ignore_case = TRUE),
    "Unknown"
  )
  x <- stringr::str_replace_all(x, "European Region \\(EURO\\)", "EURO")
  x <- stringr::str_replace_all(x, "Region of the Americas \\(AMRO\\)", "AMRO")
  x <- stringr::str_replace_all(x, "Western Pacific Region \\(WPRO\\)", "WPRO")
  x <- stringr::str_replace_all(x, "South-East Asia Region \\(SEARO\\)", "SEARO")
  x <- stringr::str_replace_all(x, "Eastern Mediterranean Region \\(EMRO\\)", "EMRO")
  x <- stringr::str_replace_all(x, "African Region \\(AFRO\\)", "AFRO")
  stringr::str_trim(x)
}

fmt_num <- function(x, digits = 4) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return(no_data)
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_p <- function(x) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return(no_data)
  if (x < 0.0001) return("<0.0001")
  sprintf("%.4f", x)
}

fmt_ci <- function(est, lo, hi, digits = 2) {
  if (any(is.na(c(est, lo, hi))) || any(!is.finite(c(est, lo, hi)))) return(no_data)
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    est, lo, hi
  )
}

read_source_verified_analysis_data <- function(
  root,
  input_file = file.path(
    root,
    "Submission_Code", "Data",
    "review_data.xlsx"
  )
) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required to read the source-verified workbook.", call. = FALSE)
  }
  if (!file.exists(input_file)) {
    stop("Source-verified workbook not found: ", input_file, call. = FALSE)
  }

  # Read as text so audit annotations such as "<5" and "derived range" in
  # count cells do not trigger type-guessing warnings or silent coercion.
  raw <- readxl::read_excel(input_file, sheet = "Review data", col_types = "text")
  design_map <- read_study_design_map(root)
  required <- c(
    "Article ID", "VE", "CI", "Season", "VaccType", "Pathogen",
    "Pathogen_specific", "Country", "WHO Region", "Epi", "Outcome", "Age",
    "Use in logOR synthesis"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Missing required source-verified columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  ve_numeric <- as_num(raw$VE)
  ve_scale <- stats::median(abs(ve_numeric), na.rm = TRUE)
  ve_percent <- if (is.finite(ve_scale) && ve_scale <= 1.5) ve_numeric * 100 else ve_numeric

  parse_ci_bounds <- function(ci_text) {
    normalized <- as.character(ci_text)
    normalized <- stringr::str_replace_all(normalized, "\u2212|\u2013|\u2014", "-")
    normalized <- stringr::str_replace_all(normalized, "(?<=\\d),(?=\\d)", "")
    # A hyphen following a number or percent sign is a range separator, not
    # the sign of the upper VE bound.
    normalized <- stringr::str_replace_all(
      normalized,
      "(?<=[0-9%])\\s*-\\s*(?=[0-9])",
      " to "
    )
    extracted <- stringr::str_extract_all(
      normalized,
      "[-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[Ee][+-]?\\d+)?"
    )
    bounds <- lapply(extracted, function(values) {
      numbers <- as_num(values)
      numbers <- numbers[is.finite(numbers)]
      if (length(numbers) < 3) return(c(NA_real_, NA_real_))
      utils::tail(numbers, 2)
    })
    bounds <- do.call(rbind, bounds)
    tibble::tibble(ci_lower_percent = bounds[, 1], ci_upper_percent = bounds[, 2])
  }

  ci_bounds <- parse_ci_bounds(raw$CI)
  raw_study_id <- trimws(as.character(raw$`Article ID`))
  raw_study_design <- design_map$study_design[
    match(raw_study_id, design_map$study_id)
  ]
  unmapped_studies <- sort(unique(raw_study_id[is.na(raw_study_design)]))
  unmapped_studies <- unmapped_studies[nzchar(unmapped_studies)]
  if (length(unmapped_studies) > 0L) {
    stop(
      "Study-design classification is missing for Article ID: ",
      paste(unmapped_studies, collapse = ", "),
      call. = FALSE
    )
  }
  or_estimate <- 1 - ve_percent / 100
  or_lower <- 1 - ci_bounds$ci_upper_percent / 100
  or_upper <- 1 - ci_bounds$ci_lower_percent / 100
  yi_log_or <- ifelse(or_estimate > 0, suppressWarnings(log(or_estimate)), NA_real_)
  sei_log_or <- ifelse(
    or_upper > 0 & or_lower > 0,
    suppressWarnings(log(or_upper) - log(or_lower)) / (2 * stats::qnorm(0.975)),
    NA_real_
  )

  age_raw <- clean_label(raw$Age)
  vaccine_raw <- clean_label(raw$VaccType)
  period_raw <- tolower(trimws(as.character(raw$Epi)))
  use_logor <- tolower(trimws(as.character(raw$`Use in logOR synthesis`))) == "yes"

  dat <- tibble::tibble(
    study_id = factor(raw_study_id),
    study_design = raw_study_design,
    season = factor(clean_label(raw$Season)),
    period_original = dplyr::case_when(
      period_raw %in% c("pre", "prepandemic", "pre-pandemic", "0") ~ 0,
      period_raw %in% c("pan/post", "pandemic/post-pandemic", "pandemic and post-pandemic", "1") ~ 1,
      TRUE ~ NA_real_
    ),
    age_group = dplyr::case_when(
      age_raw == "All ages" ~ "All ages",
      stringr::str_detect(age_raw, "&|0-64") ~ "0-64 years & ≥18 years",
      stringr::str_detect(age_raw, "^0-17 years$") ~ "0-17 years",
      stringr::str_detect(age_raw, "^18-64 years$") ~ "18-64 years",
      stringr::str_detect(age_raw, "65 years") ~ "≥65 years",
      TRUE ~ age_raw
    ),
    vaccine_type = dplyr::case_when(
      stringr::str_detect(vaccine_raw, stringr::regex("^trivalent", ignore_case = TRUE)) ~ "Trivalent",
      stringr::str_detect(vaccine_raw, stringr::regex("^quadrivalent", ignore_case = TRUE)) ~ "Quadrivalent",
      stringr::str_detect(vaccine_raw, stringr::regex("^other", ignore_case = TRUE)) ~ "Other",
      TRUE ~ "No distinction"
    ),
    pathogen = clean_label(raw$Pathogen),
    pathogen_specific = clean_label(raw$Pathogen_specific),
    country = clean_label(raw$Country),
    who_region = clean_label(raw$`WHO Region`),
    outcome = clean_label(raw$Outcome),
    reported_ve_percent = ve_percent,
    reported_ve_ci_lower_percent = ci_bounds$ci_lower_percent,
    reported_ve_ci_upper_percent = ci_bounds$ci_upper_percent,
    yi_log_or = yi_log_or,
    vi_log_or = sei_log_or^2,
    cases_vaccinated = as_num(raw$`Cases Vaccinated`),
    cases_unvaccinated = as_num(raw$`Cases Unvaccinated`),
    controls_vaccinated = as_num(raw$`Controls Vaccinated`),
    controls_unvaccinated = as_num(raw$`Controls Unvaccinated`),
    source_logor_eligible = use_logor
  ) |>
    dplyr::filter(
      .data$source_logor_eligible,
      .data$period_original %in% c(0, 1),
      is.finite(.data$yi_log_or),
      is.finite(.data$vi_log_or),
      .data$vi_log_or > 0
    ) |>
    dplyr::mutate(
      period_label = dplyr::if_else(.data$period_original == 0, "Pre", "Pan/post"),
      age_label = .data$age_group,
      vaccine_label = dplyr::if_else(.data$vaccine_type == "No distinction", "Unknown", .data$vaccine_type),
      pathogen_label = dplyr::if_else(.data$pathogen == "No distinction", "Unknown", .data$pathogen),
      who_label = clean_label(.data$who_region),
      outcome_label = clean_label(.data$outcome)
    )

  expected_eligible <- sum(use_logor, na.rm = TRUE)
  if (nrow(dat) != expected_eligible) {
    stop(
      "Not all source-audit-eligible rows produced valid log(OR) inputs: eligible=",
      expected_eligible, ", valid=", nrow(dat),
      call. = FALSE
    )
  }

  attr(dat, "source_audit") <- list(
    input_file = normalizePath(input_file, winslash = "/", mustWork = TRUE),
    source_rows = nrow(raw),
    source_logor_eligible_rows = expected_eligible,
    source_logor_excluded_rows = nrow(raw) - expected_eligible,
    included_study_design = paste(
      sort(unique(as.character(dat$study_design))), collapse = "; "
    ),
    included_studies = dplyr::n_distinct(dat$study_id),
    included_case_control_studies = dplyr::n_distinct(
      dat$study_id[dat$study_design == "Case-control study"]
    ),
    included_cohort_studies = dplyr::n_distinct(
      dat$study_id[dat$study_design == "Cohort study"]
    )
  )
  dat
}

typical_sampling_variance <- function(vi, p = 1L) {
  vi <- as.numeric(vi)
  vi <- vi[is.finite(vi) & vi > 0]
  k <- length(vi)
  if (k <= p) return(NA_real_)
  w <- 1 / vi
  sum_w <- sum(w)
  if (!is.finite(sum_w) || sum_w <= 0) return(NA_real_)
  # Higgins-Thompson typical within-study variance for an intercept-only model.
  (k - p) / (sum_w - sum(w^2) / sum_w)
}

fit_multilevel_meta <- function(sub, aggregate_random_effect_cells = FALSE) {
  sub <- sub |>
    dplyr::filter(is.finite(.data$yi_log_or), is.finite(.data$vi_log_or), .data$vi_log_or > 0) |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season = droplevels(factor(.data$season))
    )

  if (nrow(sub) == 0) {
    return(list(status = "no_data", data = sub, fit = NULL))
  }
  if (dplyr::n_distinct(sub$study_id) < 2 || dplyr::n_distinct(sub$season) < 2) {
    return(list(status = "insufficient_random_effect_levels", data = sub, fit = NULL))
  }

  fit_data <- sub
  if (aggregate_random_effect_cells) {
    # For an intercept-only model, observations within the same study-season
    # cell have identical random-effect design rows. Their inverse-variance
    # weighted mean and variance are sufficient statistics for beta and both
    # variance components. This is algebraically equivalent to fitting all
    # rows, while avoiding a very large observation-level covariance matrix.
    fit_data <- sub |>
      dplyr::group_by(.data$study_id, .data$season) |>
      dplyr::summarise(
        yi_log_or = sum(.data$yi_log_or / .data$vi_log_or) /
          sum(1 / .data$vi_log_or),
        vi_log_or = 1 / sum(1 / .data$vi_log_or),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        study_id = droplevels(factor(.data$study_id)),
        season = droplevels(factor(.data$season))
      )
  }

  fit <- tryCatch(
    metafor::rma.mv(
      yi = yi_log_or,
      V = vi_log_or,
      random = list(~ 1 | study_id, ~ 1 | season),
      data = fit_data,
      method = "REML",
      test = "t",
      sparse = TRUE
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(status = paste0("model_failed: ", conditionMessage(fit)), data = sub, fit = NULL))
  }

  list(
    status = "ok",
    data = sub,
    fit_data = fit_data,
    aggregated_random_effect_cells = aggregate_random_effect_cells,
    fit = fit
  )
}

summarize_multilevel_fit <- function(fit_obj) {
  sub <- fit_obj$data
  fit <- fit_obj$fit
  vi_col <- if (!is.null(fit_obj$vi_col)) fit_obj$vi_col else "vi_log_or"
  if (is.null(fit)) {
    return(tibble::tibble(
      n_studies = dplyr::n_distinct(sub$study_id),
      n_estimates = nrow(sub),
      n_seasons = dplyr::n_distinct(sub$season),
      pooled_ve = NA_real_,
      pooled_ve_lower = NA_real_,
      pooled_ve_upper = NA_real_,
      i2_multilevel = NA_real_,
      tau2_study = NA_real_,
      tau2_season = NA_real_,
      p_heterogeneity = NA_real_,
      p_effect = NA_real_,
      status = fit_obj$status
    ))
  }

  beta <- as.numeric(fit$b[1, 1])
  analysis_df <- nrow(sub) - 1L
  critical <- stats::qt(0.975, analysis_df)
  ci_lb <- beta - critical * as.numeric(fit$se[1])
  ci_ub <- beta + critical * as.numeric(fit$se[1])
  p_effect <- 2 * stats::pt(
    -abs(beta / as.numeric(fit$se[1])), df = analysis_df
  )
  if (isTRUE(fit_obj$aggregated_random_effect_cells)) {
    fixed_weights <- 1 / sub[[vi_col]]
    fixed_beta <- sum(fixed_weights * sub$yi_log_or) / sum(fixed_weights)
    qe <- sum(fixed_weights * (sub$yi_log_or - fixed_beta)^2)
    p_heterogeneity <- stats::pchisq(qe, df = analysis_df, lower.tail = FALSE)
  } else {
    p_heterogeneity <- ifelse(!is.null(fit$QEp), fit$QEp, NA_real_)
  }
  ve <- (1 - exp(beta)) * 100
  ve_lower <- (1 - exp(ci_ub)) * 100
  ve_upper <- (1 - exp(ci_lb)) * 100

  total_tau2 <- sum(fit$sigma2, na.rm = TRUE)
  vt <- typical_sampling_variance(sub[[vi_col]], p = 1L)
  i2 <- if (is.finite(vt) && (total_tau2 + vt) > 0) 100 * total_tau2 / (total_tau2 + vt) else NA_real_

  tibble::tibble(
    n_studies = dplyr::n_distinct(sub$study_id),
    n_estimates = nrow(sub),
    n_seasons = dplyr::n_distinct(sub$season),
    pooled_ve = ve,
    pooled_ve_lower = ve_lower,
    pooled_ve_upper = ve_upper,
    i2_multilevel = i2,
    tau2_study = ifelse(length(fit$sigma2) >= 1, fit$sigma2[[1]], NA_real_),
    tau2_season = ifelse(length(fit$sigma2) >= 2, fit$sigma2[[2]], NA_real_),
    p_heterogeneity = p_heterogeneity,
    p_effect = p_effect,
    status = fit_obj$status
  )
}
