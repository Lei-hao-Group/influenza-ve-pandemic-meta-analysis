#!/usr/bin/env Rscript

# Shared helpers for robustness analyses based on the source-verified data.

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
candidate_script <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else ""
script_dir <- if (nzchar(candidate_script) && candidate_script != "-" && file.exists(candidate_script)) {
  dirname(normalizePath(candidate_script, winslash = "/", mustWork = TRUE))
} else {
  file.path(getwd(), "Submission_Code")
}
source(file.path(script_dir, "00_meta_analysis_core.R"))

check_required_packages(c(
  "readxl", "metafor", "dplyr", "tidyr", "purrr",
  "readr", "tibble", "stringr", "openxlsx", "ggplot2"
))

additional_root <- function(root) {
  out <- file.path(root, "Additional_Analyses")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}

prepare_additional_data <- function(root) {
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
          "0-64 years & ≥18 years"
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

cluster_robust_cr1 <- function(fit_object) {
  if (is.null(fit_object$fit)) return(NULL)
  fit <- fit_object$fit
  cluster <- droplevels(factor(fit_object$model_data$study_id))
  clusters <- nlevels(cluster)
  coefficients <- ncol(fit$X)
  degrees_freedom <- clusters - coefficients
  if (degrees_freedom <= 0) {
    coefficient_values <- as.numeric(fit$b[, 1])
    return(list(
      b = fit$b,
      vb = matrix(NA_real_, coefficients, coefficients,
                  dimnames = list(rownames(fit$b), rownames(fit$b))),
      se = rep(NA_real_, coefficients), zval = rep(NA_real_, coefficients),
      ddf = rep(NA_real_, coefficients), pval = rep(NA_real_, coefficients),
      ci.lb = rep(NA_real_, coefficients), ci.ub = rep(NA_real_, coefficients),
      clusters = clusters, adjustment = "CR1 not estimable: G <= p",
      status = "insufficient_study_clusters_for_CR1"
    ))
  }

  # Algebraically identical to metafor::robust(..., adjust=TRUE), but avoids
  # explicitly forming the dense inverse of the full marginal covariance
  # matrix. The sparse solve is important for the 2,500+ row overall model.
  residual <- as.numeric(fit$yi - fit$X %*% fit$b)
  cluster_residuals <- Matrix::sparseMatrix(
    i = seq_along(residual), j = as.integer(cluster), x = residual,
    dims = c(length(residual), clusters)
  )
  weighted_cluster_residuals <- Matrix::solve(fit$M, cluster_residuals)
  cluster_scores <- as.matrix(fit$vb %*% crossprod(fit$X, weighted_cluster_residuals))
  robust_vcov <- cluster_scores %*% t(cluster_scores)
  robust_vcov <- (clusters / degrees_freedom) * robust_vcov
  robust_se <- sqrt(diag(robust_vcov))
  statistic <- as.numeric(fit$b[, 1]) / robust_se
  critical <- stats::qt(0.975, degrees_freedom)

  list(
    b = fit$b,
    vb = robust_vcov,
    se = robust_se,
    zval = statistic,
    ddf = rep(degrees_freedom, length(robust_se)),
    pval = 2 * stats::pt(-abs(statistic), degrees_freedom),
    ci.lb = as.numeric(fit$b[, 1]) - critical * robust_se,
    ci.ub = as.numeric(fit$b[, 1]) + critical * robust_se,
    clusters = clusters,
    adjustment = "CR1 = G/(G-p)",
    status = "ok"
  )
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
