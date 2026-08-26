#!/usr/bin/env Rscript

# Run the shared Table S6/Table S8 analysis pipeline in the NOS >=7 mode.
Sys.setenv(TABLE_SENSITIVITY_MODE = "s8_nos_ge7")

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
} else {
  file.path(getwd(), "Submission_Code")
}

source(file.path(script_dir, "10_generate_table_s6_2016_2025_multilevel_rma_mv.R"))
