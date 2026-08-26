#!/usr/bin/env Rscript

# Run the analyses in separate R/Python sessions so that options and
# environment variables set by one script do not affect later scripts.

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
code_dir <- if (nzchar(script_path) && file.exists(script_path)) {
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  normalizePath("Submission_Code", winslash = "/", mustWork = TRUE)
}
project_root <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)

jobs <- list(
  c("Rscript", "01_generate_table1_multilevel_rma_mv.R"),
  c("Rscript", "02_generate_adjusted_period_effects.R"),
  c("Rscript", "04_generate_figure2_who_region_pathogen.R"),
  c("Rscript", "06_northern_segmented_regression.R"),
  c("Rscript", "07_northern_segmented_nature_figure.R"),
  c("Rscript", "08_generate_appendix_geographic_distribution.R"),
  c("python3", "09_plot_summary_of_bias_assessment.py"),
  c("Rscript", "10_generate_table_s6_2016_2025_multilevel_rma_mv.R"),
  c("Rscript", "11_generate_table_s7_crude_or_abcd_multilevel_rma_mv.R"),
  c("Rscript", "12_generate_table_s8_high_quality_nos_ge7.R"),
  c("Rscript", "13_study_influence_balance_summary.R"),
  c("python3", "14_plot_study_influence_balance.py")
)

old_dir <- setwd(project_root)
on.exit(setwd(old_dir), add = TRUE)
for (job in jobs) {
  program <- job[[1]]
  script <- file.path(code_dir, job[[2]])
  message("Running ", basename(script))
  status <- system2(program, script)
  if (!identical(status, 0L)) {
    stop("Analysis failed: ", basename(script), call. = FALSE)
  }
}

message("All analyses completed.")
