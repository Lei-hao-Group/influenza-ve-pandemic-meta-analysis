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
dat <- read_source_verified_analysis_data(root)
source_audit <- attr(dat, "source_audit")

out_dir <- file.path(root, "Results", "Table1")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_csv <- file.path(out_dir, "Table1_multilevel_random_effects_study_season.csv")
out_xlsx <- file.path(out_dir, "Table1_multilevel_random_effects_study_season.xlsx")
audit_csv <- file.path(out_dir, "Table1_multilevel_random_effects_study_season_audit.csv")
partial_csv <- file.path(out_dir, "Table1_multilevel_random_effects_study_season_partial.csv")
progress_log <- file.path(out_dir, "Table1_multilevel_random_effects_study_season_progress.log")

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
rows <- add_pair(rows, "Age", "0-64 years & \u226518 years", function(x) x$age_label == "0-64 years & \u226518 years")

rows[[length(rows) + 1]] <- section_row("Vaccine type")
write_partial(rows)
rows <- add_pair(rows, "Vaccine type", "Trivalent", function(x) x$vaccine_label == "Trivalent")
rows <- add_pair(rows, "Vaccine type", "Quadrivalent", function(x) x$vaccine_label == "Quadrivalent")
rows <- add_pair(rows, "Vaccine type", "Other", function(x) x$vaccine_label == "Other")
rows <- add_pair(rows, "Vaccine type", "Unknown", function(x) x$vaccine_label == "Unknown")

rows[[length(rows) + 1]] <- section_row("Influenza type")
write_partial(rows)
rows <- add_pair(rows, "Influenza type", "IAV", function(x) x$pathogen == "IAV")
rows <- add_pair(rows, "Influenza type", "A/H1N1", function(x) x$pathogen_specific %in% c("A(H1N1)", "A(pH1N1)"))
rows <- add_pair(rows, "Influenza type", "A/H3N2", function(x) x$pathogen_specific == "A(H3N2)")
rows <- add_pair(rows, "Influenza type", "IBV", function(x) x$pathogen == "IBV")
rows <- add_pair(rows, "Influenza type", "B/Yamagata", function(x) x$pathogen_specific == "B(Yamagata)")
rows <- add_pair(rows, "Influenza type", "B/Victoria", function(x) x$pathogen_specific == "B(Victoria)")
rows <- add_pair(rows, "Influenza type", "Unknown", function(x) x$pathogen == "No distinction")

rows[[length(rows) + 1]] <- section_row("WHO region")
write_partial(rows)
rows <- add_pair(rows, "WHO region", "EURO", function(x) x$who_label == "EURO")
rows <- add_pair(rows, "WHO region", "AMRO", function(x) x$who_label == "AMRO")
rows <- add_pair(rows, "WHO region", "WPRO", function(x) x$who_label == "WPRO")
rows <- add_pair(rows, "WHO region", "SEARO", function(x) x$who_label == "SEARO")
rows <- add_pair(rows, "WHO region", "AFRO", function(x) x$who_label == "AFRO")
rows <- add_pair(rows, "WHO region", "EMRO", function(x) x$who_label == "EMRO")
rows <- add_pair(rows, "WHO region", "Multi-region", function(x) x$who_label == "Multi-region")

rows[[length(rows) + 1]] <- section_row("Clinical outcome")
write_partial(rows)
rows <- add_pair(rows, "Clinical outcome", "Outpatient", function(x) x$outcome_label == "Outpatient")
rows <- add_pair(rows, "Clinical outcome", "Inpatient", function(x) x$outcome_label == "Inpatient")
rows <- add_pair(rows, "Clinical outcome", "Severe and Death", function(x) x$outcome_label %in% c("Severe", "Death", "Severe and Death"))
rows <- add_pair(rows, "Clinical outcome", "Unknown", function(x) x$outcome_label == "Unknown")

table1 <- dplyr::bind_rows(rows)
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

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Table 1", gridLines = FALSE)
openxlsx::addWorksheet(wb, "Audit", gridLines = FALSE)
openxlsx::writeData(wb, "Table 1", table1)
openxlsx::writeData(wb, "Audit", audit)

header_style <- openxlsx::createStyle(
  fgFill = "#1F4E79",
  fontColour = "#FFFFFF",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderColour = "#D9D9D9"
)
section_style <- openxlsx::createStyle(
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  border = "TopBottomLeftRight",
  borderColour = "#D9D9D9"
)
body_style <- openxlsx::createStyle(
  border = "TopBottomLeftRight",
  borderColour = "#D9D9D9",
  valign = "center",
  wrapText = TRUE
)

openxlsx::addStyle(wb, "Table 1", header_style, rows = 1, cols = seq_len(ncol(table1)), gridExpand = TRUE)
if (nrow(table1) > 0) {
  openxlsx::addStyle(wb, "Table 1", body_style, rows = 2:(nrow(table1) + 1), cols = seq_len(ncol(table1)), gridExpand = TRUE)
  section_rows <- which(table1$Variable == table1$Period & table1$Variable == table1$`No. of studies`) + 1
  if (length(section_rows) > 0) {
    openxlsx::addStyle(wb, "Table 1", section_style, rows = section_rows, cols = seq_len(ncol(table1)), gridExpand = TRUE, stack = TRUE)
  }
}
openxlsx::setColWidths(wb, "Table 1", cols = 1, widths = 24)
openxlsx::setColWidths(wb, "Table 1", cols = 2, widths = 12)
openxlsx::setColWidths(wb, "Table 1", cols = 3:5, widths = 14)
openxlsx::setColWidths(wb, "Table 1", cols = 6, widths = 25)
openxlsx::setColWidths(wb, "Table 1", cols = 7:12, widths = 18)
openxlsx::freezePane(wb, "Table 1", firstRow = TRUE)

openxlsx::addStyle(wb, "Audit", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
openxlsx::addStyle(wb, "Audit", body_style, rows = 2:(nrow(audit) + 1), cols = 1:2, gridExpand = TRUE)
openxlsx::setColWidths(wb, "Audit", cols = 1, widths = 28)
openxlsx::setColWidths(wb, "Audit", cols = 2, widths = 80)
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Wrote: ", out_csv)
message("Wrote: ", out_xlsx)
message("Wrote: ", audit_csv)
