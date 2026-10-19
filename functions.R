# Data preparation and multilevel meta-analysis helpers.

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

get_project_root <- function(...) code_dir

no_data <- "--"

as_num <- function(x) suppressWarnings(as.numeric(x))

read_study_design_map <- function(root) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required to read the study-design map.", call. = FALSE)
  }
  quality_file <- file.path(
    root, "Data",
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

# Classify labels that identify exactly one influenza season without assigning
# a multi-season estimate to an arbitrary constituent season. Cross-year labels
# (for example, 2018-2019) and calendar-year labels (for example, 2019) remain
# distinct because they generally represent different seasonal conventions.
classify_single_season <- function(x) {
  reported <- clean_label(x)
  normalized <- reported |>
    stringr::str_replace_all("\u2212|\u2013|\u2014", "-") |>
    stringr::str_replace_all("\\s+", "")

  calendar_match <- stringr::str_match(normalized, "^(\\d{4})$")
  cross_year_match <- stringr::str_match(normalized, "^(\\d{4})-(\\d{2}|\\d{4})$")
  start_year <- suppressWarnings(as.integer(cross_year_match[, 2]))
  raw_end <- cross_year_match[, 3]
  end_year <- suppressWarnings(as.integer(raw_end))
  short_end <- !is.na(raw_end) & nchar(raw_end) == 2L
  end_year[short_end] <- (start_year[short_end] %/% 100L) * 100L + end_year[short_end]
  valid_cross_year <- is.finite(start_year) & is.finite(end_year) & end_year == start_year + 1L
  calendar_year <- suppressWarnings(as.integer(calendar_match[, 2]))
  valid_calendar <- is.finite(calendar_year)

  convention <- dplyr::case_when(
    valid_cross_year ~ "cross-year season",
    valid_calendar ~ "calendar-year season",
    TRUE ~ "multi-season or non-single-season period"
  )
  canonical <- dplyr::case_when(
    valid_cross_year ~ sprintf("cross-year:%04d-%04d", start_year, end_year),
    valid_calendar ~ sprintf("calendar-year:%04d", calendar_year),
    TRUE ~ NA_character_
  )

  tibble::tibble(
    reported_study_period = reported,
    normalized_period_text = normalized,
    is_single_season = valid_cross_year | valid_calendar,
    season_convention = convention,
    normalized_season = canonical,
    season_start_year = dplyr::if_else(valid_cross_year, start_year, calendar_year),
    season_end_year = dplyr::if_else(valid_cross_year, end_year, calendar_year)
  )
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
    "Data",
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
      stringr::str_detect(age_raw, "&|0-64") ~ "Mixed age groups",
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

results_root <- function(root) {
  out <- Sys.getenv("VE_RESULTS_DIR", unset = file.path(root, "Results"))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}

prepare_analysis_data <- function(root) {
  read_source_verified_analysis_data(root) |>
    dplyr::mutate(
      period = factor(
        .data$period_original,
        levels = c(0, 1),
        labels = c("Pre-pandemic", "Pandemic/post-pandemic")
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
          .data$outcome_label %in% c("Severe", "Death", "Severe and Death") ~ "Severe and Death",
          TRUE ~ .data$outcome_label
        ),
        levels = c("Unknown", "Outpatient", "Inpatient", "Severe and Death")
      ),
      yi_model = .data$yi_log_or,
      vi_model = .data$vi_log_or
    ) |>
    dplyr::filter(
      is.finite(.data$yi_model), is.finite(.data$vi_model), .data$vi_model > 0,
      !is.na(.data$period)
    )
}

active_covariates <- function(dat, candidates) {
  candidates[vapply(candidates, function(x) {
    x %in% names(dat) && dplyr::n_distinct(dat[[x]], na.rm = TRUE) > 1
  }, logical(1))]
}

align_design_matrix <- function(formula, newdata, coefficient_names) {
  design <- stats::model.matrix(formula, data = newdata)
  if ("(Intercept)" %in% colnames(design) && "intrcpt" %in% coefficient_names) {
    colnames(design)[colnames(design) == "(Intercept)"] <- "intrcpt"
  }
  output <- matrix(0, nrow = nrow(design), ncol = length(coefficient_names))
  colnames(output) <- coefficient_names
  common <- intersect(colnames(design), coefficient_names)
  output[, common] <- design[, common, drop = FALSE]
  output
}

add_excel_sheets <- function(path, sheets) {
  workbook <- openxlsx::createWorkbook()
  used <- character()
  for (sheet_name in names(sheets)) {
    clean <- substr(gsub("[\\\\/?*\\[\\]:]", "_", sheet_name), 1, 31)
    while (clean %in% used) clean <- substr(paste0(clean, "_2"), 1, 31)
    used <- c(used, clean)
    openxlsx::addWorksheet(workbook, clean)
    openxlsx::writeData(workbook, clean, sheets[[sheet_name]])
    openxlsx::freezePane(workbook, clean, firstRow = TRUE)
    if (ncol(sheets[[sheet_name]]) > 0) {
      openxlsx::setColWidths(workbook, clean, cols = seq_len(ncol(sheets[[sheet_name]])), widths = "auto")
    }
  }
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
}

prepare_period_data <- function(source_dat) {
  source_dat |>
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
        levels = c("All ages", "0-17 years", "18-64 years", "\u226565 years", "Mixed age groups")
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
}

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
  # Drop empty levels so that, when the preferred reference level is absent
  # from a subgroup, the first available level becomes the reference (as
  # labelled in the output) instead of metafor dropping an arbitrary level.
  for (x in active_covariates) sub[[x]] <- droplevels(sub[[x]])

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

gvif_rows <- function(model, active_covariates, table_name, panel, subgroup) {
  terms <- c("period", active_covariates)
  groups <- lapply(terms, function(term) grep(paste0("^", term), rownames(model$b)))
  diagnostic <- metafor::vif(model, btt = groups, reestimate = FALSE)
  tibble::tibble(
    Model = table_name,
    Panel = panel,
    Subgroup = subgroup,
    Moderator = unname(display_var[terms]),
    df = lengths(groups),
    GVIF = unname(diagnostic$vifs),
    GVIF_adjusted = unname(diagnostic$vifs)^(1 / (2 * lengths(groups)))
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

period_model_specs <- function() {
  list(
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
}

