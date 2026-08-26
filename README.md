# Analysis code

This folder contains the code used for the results reported in `Subtype-Specific Changes in Real-World Influenza Vaccine Effectiveness Following the Onset of the COVID-19 Pandemic.docx`. 

## Files

Main manuscript:

- `00_meta_analysis_core.R`: core data preparation, effect-size transformation, and multilevel meta-analysis functions.
- `01_generate_table1_multilevel_rma_mv.R`: period-specific pooled VE estimates in Table 1.
- `02_generate_adjusted_period_effects.R`: adjusted period effects reported in Table 1.
- `03_figure2_official_map_and_export.R`: map and export functions used for Figure 2.
- `04_generate_figure2_who_region_pathogen.R`: WHO-region and pathogen results in Figure 2.
- `05_robustness_analysis_core.R`: core functions for segmented models, cluster-robust inference, and study-influence analyses.
- `06_northern_segmented_regression.R`: standardized Northern Hemisphere annual VE and segmented regression.
- `07_northern_segmented_figure.R`: Figure 3 based on the segmented-regression results.

Supplementary appendix:

- `08_generate_appendix_geographic_distribution.R`: geographic distribution in Figure S1.
- `09_plot_summary_of_bias_assessment.py`: Newcastle–Ottawa Scale summary in Figure S2.
- `10_generate_table_s6_2016_2025_multilevel_rma_mv.R`: restricted time-window sensitivity analysis in Table S6.
- `11_generate_table_s7_crude_or_abcd_multilevel_rma_mv.R`: crude-OR sensitivity analysis in Table S7.
- `12_generate_table_s8_high_quality_nos_ge7.R`: NOS ≥7 sensitivity analysis in Table S8.
- `13_study_influence_balance_summary.R`: leave-one-study-out and study-contribution-balanced analyses for Figure S3.
- `14_plot_study_influence_balance.py`: generates Figure S3 from the study-influence results.
- `run_all.R`: runs the complete analysis sequence.

## Data

- `Data/review_data.xlsx`: source-verified review data used in the meta-analysis models.
- `Data/Supplementary Data_Characteristics and Quality score.xlsx`: study design, characteristics, and Newcastle–Ottawa Scale data.
- `Data/geo/`: geographic reference files used only for the maps.

`Article ID` runs from 1 to 349 and follows the study order in Table S5.

## Use

Keep the contents of `Submission_Code/` together and run from its parent directory:

```bash
Rscript Submission_Code/run_all.R
```

Individual scripts can be run in the same way. R 4.5.1 was used for the manuscript. Required R packages are `metafor`, `Matrix`, `readr`, `readxl`, `openxlsx`, `dplyr`, `tidyr`, `tibble`, `stringr`, `purrr`, `ggplot2`, `ggrepel`, `patchwork`, `cowplot`, `jsonlite`, and `sf`. Python figure scripts require `matplotlib`, `numpy`, `pandas`, `openpyxl`, and `Pillow`.

Results are written to `Results/` and `Additional_Analyses/`.
