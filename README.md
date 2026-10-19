# Influenza vaccine effectiveness

Code for *Subtype-Specific Changes in Real-World Influenza Vaccine Effectiveness Following the Onset of the COVID-19 Pandemic*.

| File | Purpose |
|---|---|
| `functions.R` | Shared data preparation and model functions |
| `meta_analysis.R` | Table 1, Tables S20–S21 and regression coefficients |
| `sensitivity.R` | Tables S11–S18 and analyses for Figure S3 |
| `trends.R` | Figure 3 |
| `maps.R` | Figure 2 and Figure S1 |
| `descriptive.R` | Tables S6–S8, S10 and S19 |
| `excluded_estimates.py` | Table S4 |
| `figures.py` | Figures S2–S4 |

Keep `Data/` with the scripts. From this folder, run:

```sh
Rscript descriptive.R
python3 excluded_estimates.py
Rscript meta_analysis.R
Rscript sensitivity.R
Rscript trends.R
Rscript maps.R
python3 figures.py
```

`functions.R` is loaded automatically. Outputs are saved in `Results/`; figures follow manuscript numbering, and tables are exported as CSV/Excel.

R packages: `metafor`, `clubSandwich`, `readr`, `readxl`, `openxlsx`, `dplyr`, `tidyr`, `tibble`, `stringr`, `purrr`, `ggplot2`, `ggrepel`, `scales`, `patchwork`, `cowplot`, `jsonlite`.

Install Python dependencies with `python3 -m pip install -r requirements.txt`.
