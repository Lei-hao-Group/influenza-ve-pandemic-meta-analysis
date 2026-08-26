#!/usr/bin/env Rscript

# Standardized annual vaccine-effectiveness models and segmented trends for
# A(H1N1)pdm09, A(H3N2), and influenza B in the Northern Hemisphere.

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
script_dir <- if (nzchar(script_path) && file.exists(script_path)) {
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  file.path(getwd(), "Submission_Code")
}
source(file.path(script_dir, "05_robustness_analysis_core.R"))

root <- get_project_root(file.path(
  "Submission_Code", "Data",
  "review_data.xlsx"
))
out_dir <- file.path(
  additional_root(root),
  "21_northern_hemisphere_subtype_pandemic_segmented_models"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

subtype_levels <- c("A/H1N1", "A/H3N2", "IBV")
pandemic_knot <- 2020.00
minimum_nodes_each_side <- 3L
z_975 <- stats::qnorm(0.975)
adjusters <- c(
  "age_display", "vaccine_type_display", "region_short", "outcome_display"
)
common_levels <- list(
  age_display = c("All ages", "0-17 years", "18-64 years", "≥65 years"),
  vaccine_type_display = c("Unknown", "Trivalent", "Quadrivalent"),
  region_short = c("AMRO", "EURO", "WPRO", "SEARO"),
  outcome_display = c(
    "Unknown", "Outpatient", "Inpatient", "Severe and Death"
  )
)

parse_single_season <- function(season) {
  text <- as.character(season)
  matched <- stringr::str_match(text, "^(\\d{4})-(\\d{4})$")
  start <- suppressWarnings(as.numeric(matched[, 2]))
  end <- suppressWarnings(as.numeric(matched[, 3]))
  tibble::tibble(
    season_text = text,
    season_mid_year = ifelse(
      is.finite(start) & is.finite(end) & end == start + 1,
      start + 0.5,
      NA_real_
    )
  )
}

pool_sufficient_statistics <- function(dat, group_columns) {
  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) |>
    dplyr::summarise(
      yi_model = sum(.data$yi_model / .data$vi_model) /
        sum(1 / .data$vi_model),
      vi_model = 1 / sum(1 / .data$vi_model),
      source_estimates = dplyr::n(),
      .groups = "drop"
    )
}

standardized_prediction <- function(fit, formula, newdata, reference_n) {
  design <- align_design_matrix(formula, newdata, rownames(fit$b))
  xbar <- colMeans(design)
  mu <- as.numeric(xbar %*% as.numeric(fit$b[, 1]))
  variance <- as.numeric(t(xbar) %*% fit$vb %*% xbar)
  se <- sqrt(max(variance, 0))
  tibble::tibble(
    target_cells = reference_n,
    beta_log_or = mu,
    se_log_or = se,
    ci_lower_log_or = mu - z_975 * se,
    ci_upper_log_or = mu + z_975 * se,
    ve_percent = 100 * (1 - exp(mu)),
    ve_ci_lower = 100 * (1 - exp(mu + z_975 * se)),
    ve_ci_upper = 100 * (1 - exp(mu - z_975 * se))
  )
}

source_data <- prepare_additional_data(root)

fit_annual_model <- function(group_name) {
  group_match <- if (group_name == "A/H1N1") {
    source_data$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)")
  } else if (group_name == "A/H3N2") {
    source_data$pathogen_specific == "A(H3N2)"
  } else if (group_name == "IBV") {
    as.character(source_data$pathogen_display) == "IBV"
  } else {
    rep(FALSE, nrow(source_data))
  }
  annual_data <- dplyr::bind_cols(
    source_data, parse_single_season(source_data$season)
  ) |>
    dplyr::mutate(group_match = group_match) |>
    dplyr::filter(
      !is.na(.data$season_mid_year),
      .data$group_match,
      as.character(.data$age_display) %in% common_levels$age_display,
      as.character(.data$vaccine_type_display) %in%
        common_levels$vaccine_type_display,
      as.character(.data$region_short) %in% common_levels$region_short,
      as.character(.data$outcome_display) %in% common_levels$outcome_display
    ) |>
    dplyr::select(
      "study_id", "season_text", "season_mid_year",
      dplyr::all_of(adjusters), "yi_model", "vi_model"
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(study_id = droplevels(factor(.data$study_id)))

  for (variable in adjusters) {
    annual_data[[variable]] <- factor(
      as.character(annual_data[[variable]]), levels = common_levels[[variable]]
    )
  }
  active <- adjusters[vapply(adjusters, function(variable) {
    nlevels(droplevels(annual_data[[variable]])) > 1L
  }, logical(1))]

  model_data <- pool_sufficient_statistics(
    annual_data, c("study_id", "season_text", "season_mid_year", active)
  )
  season_levels <- model_data |>
    dplyr::distinct(.data$season_text, .data$season_mid_year) |>
    dplyr::arrange(.data$season_mid_year, .data$season_text) |>
    dplyr::pull(.data$season_text)
  model_data <- model_data |>
    dplyr::mutate(
      study_id = droplevels(factor(.data$study_id)),
      season_factor = factor(.data$season_text, levels = season_levels)
    )
  for (variable in active) {
    model_data[[variable]] <- droplevels(factor(model_data[[variable]]))
  }

  formula <- stats::as.formula(paste(
    "~ 0 + season_factor",
    if (length(active)) paste("+", paste(active, collapse = " + ")) else ""
  ))
  design <- stats::model.matrix(formula, data = model_data)
  if (qr(design)$rank < ncol(design) || nrow(design) <= ncol(design)) {
    stop("Annual model is rank deficient for ", group_name, call. = FALSE)
  }

  fit <- suppressWarnings(metafor::rma.mv(
    yi = yi_model,
    V = vi_model,
    mods = formula,
    random = ~ 1 | study_id,
    data = model_data,
    method = "REML",
    test = "z",
    sparse = TRUE
  ))

  # Each season is standardized to the same set of observed covariate profiles.
  reference <- annual_data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(adjusters)))
  prediction_rows <- vector("list", length(season_levels))
  standardization_rows <- vector("list", length(season_levels))
  for (index in seq_along(season_levels)) {
    season_name <- season_levels[[index]]
    newdata <- reference
    newdata$season_factor <- factor(
      season_name, levels = levels(model_data$season_factor)
    )
    for (variable in active) {
      newdata[[variable]] <- factor(
        newdata[[variable]], levels = levels(model_data[[variable]])
      )
    }
    prediction_rows[[index]] <- standardized_prediction(
      fit, formula, newdata, nrow(reference)
    ) |>
      dplyr::mutate(season_text = season_name)
    standardization_rows[[index]] <- colMeans(align_design_matrix(
      formula, newdata, rownames(fit$b)
    ))
  }

  estimates <- dplyr::bind_rows(prediction_rows)
  standardization_matrix <- do.call(rbind, standardization_rows)
  covariance_log_or <- standardization_matrix %*% fit$vb %*%
    t(standardization_matrix)
  derivative_ve <- diag(-100 * exp(estimates$beta_log_or))
  covariance_ve <- derivative_ve %*% covariance_log_or %*% derivative_ve
  dimnames(covariance_ve) <- list(season_levels, season_levels)

  counts <- annual_data |>
    dplyr::group_by(.data$season_text, .data$season_mid_year) |>
    dplyr::summarise(
      n_source_estimates = dplyr::n(),
      n_studies = dplyr::n_distinct(.data$study_id),
      .groups = "drop"
    )

  list(
    estimates = estimates |>
      dplyr::left_join(counts, by = "season_text") |>
      dplyr::mutate(
        subtype = group_name,
        model_status = "ok_northern_adjusted_standardized",
        tau2_study = as.numeric(fit$sigma2[[1]]),
        .before = 1
      ),
    covariance_ve = covariance_ve,
    audit = tibble::tibble(
      subtype = group_name,
      source_estimates = nrow(annual_data),
      sufficient_statistic_rows = nrow(model_data),
      studies = dplyr::n_distinct(model_data$study_id),
      seasons = length(season_levels),
      active_adjusters = paste(active, collapse = "; "),
      coefficients = ncol(design),
      residual_design_df = nrow(design) - ncol(design),
      tau2_study = as.numeric(fit$sigma2[[1]]),
      status = "ok"
    )
  )
}

contrast_result <- function(fit, contrast, estimand, subtype_name) {
  beta <- as.numeric(fit$b[, 1])
  covariance <- fit$vb
  estimate <- as.numeric(contrast %*% beta)
  se <- sqrt(max(as.numeric(contrast %*% covariance %*% contrast), 0))
  df <- min(as.numeric(fit$ddf[contrast != 0]), na.rm = TRUE)
  critical <- stats::qt(0.975, df)
  statistic <- estimate / se
  tibble::tibble(
    subtype = subtype_name,
    estimand = estimand,
    status = "ok",
    estimate_ve_pp_per_year = estimate,
    se_model = se,
    ci_lower_model = estimate - critical * se,
    ci_upper_model = estimate + critical * se,
    t_statistic = statistic,
    df = df,
    p_value_model = 2 * stats::pt(-abs(statistic), df)
  )
}

fit_segmented_model <- function(subtype_name, annual, covariance_ve) {
  model_data <- annual |>
    dplyr::filter(.data$subtype == subtype_name) |>
    dplyr::arrange(.data$season_mid_year) |>
    dplyr::mutate(
      season_year = .data$season_mid_year,
      time_from_pandemic_knot = .data$season_year - pandemic_knot,
      post_pandemic_hinge = pmax(0, .data$time_from_pandemic_knot),
      season_node = factor(.data$season_text, levels = .data$season_text)
    )
  pre_nodes <- sum(model_data$season_year <= pandemic_knot)
  post_nodes <- sum(model_data$season_year > pandemic_knot)
  if (pre_nodes < minimum_nodes_each_side || post_nodes < minimum_nodes_each_side) {
    stop("Insufficient season nodes for ", subtype_name, call. = FALSE)
  }

  covariance_ve <- covariance_ve[
    model_data$season_text, model_data$season_text, drop = FALSE
  ]
  covariance_ve <- (covariance_ve + t(covariance_ve)) / 2
  fit <- suppressWarnings(metafor::rma.mv(
    yi = ve_percent,
    V = covariance_ve,
    mods = ~ time_from_pandemic_knot + post_pandemic_hinge,
    random = ~ 1 | season_node,
    data = model_data,
    method = "REML",
    test = "t",
    dfs = "contain",
    sparse = FALSE
  ))

  slopes <- dplyr::bind_rows(
    contrast_result(fit, c(0, 1, 0), "Slope before knot", subtype_name),
    contrast_result(fit, c(0, 1, 1), "Slope after knot", subtype_name),
    contrast_result(
      fit, c(0, 0, 1), "Slope change after minus before", subtype_name
    )
  )

  grid <- seq(min(model_data$season_year), max(model_data$season_year),
              length.out = 401L)
  design <- cbind(
    intrcpt = 1,
    time_from_pandemic_knot = grid - pandemic_knot,
    post_pandemic_hinge = pmax(0, grid - pandemic_knot)
  )
  design <- design[, rownames(fit$b), drop = FALSE]
  fitted <- as.numeric(design %*% fit$b[, 1])
  fitted_se <- sqrt(pmax(rowSums((design %*% fit$vb) * design), 0))
  prediction_df <- min(as.numeric(fit$ddf), na.rm = TRUE)
  critical <- stats::qt(0.975, prediction_df)
  predictions <- tibble::tibble(
    subtype = subtype_name,
    model_status = "ok_prespecified_pandemic_segmented_model",
    season_year = grid,
    fitted_ve_percent = fitted,
    se_fitted_model = fitted_se,
    ci_lower_model = fitted - critical * fitted_se,
    ci_upper_model = fitted + critical * fitted_se
  )

  coefficients <- tibble::tibble(
    subtype = subtype_name,
    model_status = "ok_prespecified_pandemic_segmented_model",
    term = rownames(fit$b),
    estimate = as.numeric(fit$b[, 1]),
    se_model = as.numeric(fit$se),
    ci_lower_model = as.numeric(fit$ci.lb),
    ci_upper_model = as.numeric(fit$ci.ub),
    t_statistic = as.numeric(fit$zval),
    df = as.numeric(fit$ddf),
    p_value_model = as.numeric(fit$pval)
  )
  audit <- tibble::tibble(
    subtype = subtype_name,
    seasons = nrow(model_data),
    seasons_at_or_before_knot = pre_nodes,
    seasons_after_knot = post_nodes,
    prespecified_knot_year = pandemic_knot,
    residual_df = prediction_df,
    stage2_tau2 = as.numeric(fit$sigma2[[1]]),
    status = "ok"
  )
  list(
    data = model_data,
    coefficients = coefficients,
    slopes = slopes,
    predictions = predictions,
    audit = audit
  )
}

message("Fitting standardized annual models")
annual_fits <- stats::setNames(lapply(subtype_levels, fit_annual_model), subtype_levels)
annual_estimates <- dplyr::bind_rows(lapply(annual_fits, `[[`, "estimates"))
annual_audit <- dplyr::bind_rows(lapply(annual_fits, `[[`, "audit"))

message("Fitting prespecified segmented trends")
segmented_fits <- lapply(subtype_levels, function(subtype_name) {
  fit_segmented_model(
    subtype_name,
    annual_estimates,
    annual_fits[[subtype_name]]$covariance_ve
  )
})
analysis_input <- dplyr::bind_rows(lapply(segmented_fits, `[[`, "data")) |>
  dplyr::mutate(
    season_format = "Northern two-year season (YYYY-YYYY)",
    se_ve_delta = 100 * exp(.data$beta_log_or) * .data$se_log_or,
    inverse_variance_weight_raw = 1 / .data$se_ve_delta^2,
    pandemic_segment = ifelse(
      .data$season_year <= pandemic_knot,
      "At or before prespecified knot",
      "After prespecified knot"
    ),
    subtype = factor(.data$subtype, levels = subtype_levels)
  ) |>
  dplyr::arrange(.data$subtype, .data$season_year)
model_audit <- dplyr::bind_rows(lapply(segmented_fits, `[[`, "audit"))
model_coefficients <- dplyr::bind_rows(
  lapply(segmented_fits, `[[`, "coefficients")
)
slope_results <- dplyr::bind_rows(lapply(segmented_fits, `[[`, "slopes")) |>
  dplyr::mutate(
    p_value_fdr_bh = {
      adjusted <- rep(NA_real_, dplyr::n())
      selected <- .data$estimand == "Slope change after minus before"
      adjusted[selected] <- stats::p.adjust(
        .data$p_value_model[selected], method = "BH"
      )
      adjusted
    }
  )
curve_predictions <- dplyr::bind_rows(
  lapply(segmented_fits, `[[`, "predictions")
)
covariance_long <- dplyr::bind_rows(lapply(subtype_levels, function(name) {
  covariance <- annual_fits[[name]]$covariance_ve
  as.data.frame(as.table(covariance), responseName = "covariance_ve") |>
    dplyr::transmute(
      subtype = name,
      season_1 = as.character(.data$Var1),
      season_2 = as.character(.data$Var2),
      covariance_ve = .data$covariance_ve
    )
}))

readr::write_excel_csv(
  analysis_input, file.path(out_dir, "northern_segmented_analysis_input.csv"),
  na = ""
)
readr::write_excel_csv(
  annual_audit, file.path(out_dir, "northern_annual_model_audit.csv"), na = ""
)
readr::write_excel_csv(
  covariance_long, file.path(out_dir, "northern_annual_covariance_ve.csv"),
  na = ""
)
readr::write_excel_csv(
  model_audit, file.path(out_dir, "northern_segmented_model_audit.csv"), na = ""
)
readr::write_excel_csv(
  model_coefficients,
  file.path(out_dir, "northern_segmented_model_coefficients.csv"), na = ""
)
readr::write_excel_csv(
  slope_results, file.path(out_dir, "northern_segmented_slope_results.csv"),
  na = ""
)
readr::write_excel_csv(
  curve_predictions,
  file.path(out_dir, "northern_segmented_curve_predictions.csv"), na = ""
)
add_excel_sheets(
  file.path(out_dir, "northern_subtype_pandemic_segmented_outputs.xlsx"),
  list(
    "Analysis input" = analysis_input,
    "Annual model audit" = annual_audit,
    "Annual covariance VE" = covariance_long,
    "Segmented audit" = model_audit,
    "Coefficients" = model_coefficients,
    "Slope results" = slope_results,
    "Curve predictions" = curve_predictions
  )
)

message("Outputs written to: ", out_dir)
