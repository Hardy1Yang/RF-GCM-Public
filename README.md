# RF-GCM

**Enhancing Random Forest Performance on Imbalanced Data Using the General Condorcet Model**

This repository contains the R code accompanying:

> Yang, H.-H., Ho, T.-Y., & Hsu, Y.-F. Enhancing random forest performance on imbalanced data using the General Condorcet Model. *Journal of Classification* (minor revision).

GCM comes from cultural consensus theory in mathematical psychology; here it replaces majority voting as the aggregation rule of a random forest.

## Overview

This project introduces the General Condorcet Model (GCM) as a novel aggregation strategy for Random Forest (RF) to handle class-imbalanced binary classification. GCM replaces the standard majority rule in RF with a likelihood-based decision rule that accounts for both the detection rate and guessing bias of individual trees, providing a principled threshold adjustment grounded in signal detection theory.

The code implements the full experimental pipeline, including:

- Standard RF with majority rule
- GCM-based threshold estimation (proportion-based and MCMC fallback)
- GCM with validation-set estimation (for OOB vs validation comparison)
- Threshold-moving methods ($\lambda_{prior}$, $\lambda_{GM}$, $\lambda_{F1}$)
- Balanced Random Forest (BRF)
- SMOTE oversampling
- Cost-Sensitive Random Forest (CSRF)
- EasyEnsemble

## Repository Structure

```
.
├── config.R                      # Central configuration (paths, parameters)
├── main.R                        # Main script: runs the full analysis pipeline
├── setup.R                       # Package installation and loading
├── get_performance_summary.R     # Summarizes results; generates tables and figures
├── run_parallel.sh               # Launches main.R in parallel batches
├── run_r3c4_subset.R             # R3C4 threshold consistency (MCMC subset)
├── method_code/                  # Implementation of each method
│   ├── RF_related.R              # RF fitting, train/test split, OOB utilities
│   ├── GCM_observeProp.R         # GCM parameter estimation via observed proportions
│   ├── GCM_MCMC.R                # GCM parameter estimation via MCMC (JAGS)
│   ├── threshold.R               # Threshold-moving methods
│   ├── other_imbalanced_methods.R  # BRF, SMOTE, CSRF, EasyEnsemble implementations
│   ├── SMOTE_N.R                 # SMOTE for discrete/mixed features
│   └── model_fixedDG3.txt        # JAGS model specification for GCM
├── dataset/                      # Data files (not tracked by git; see below)
│   └── 0. description.csv        # Dataset metadata
├── results/                      # Generated prediction outputs (not tracked)
└── result_tab_fig/               # Generated tables (not tracked)
```

## Getting Started

### Prerequisites

- **R** >= 4.2
- **JAGS** (Just Another Gibbs Sampler) must be installed separately.
  Download from: https://mcmc-jags.sourceforge.io/

### Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/Hardy1Yang/RF-GCM-Public.git
   cd RF-GCM-Public
   ```

2. Install R dependencies (handled automatically by `setup.R`):
   ```r
   source("setup.R")
   ```

   Key packages: `randomForest`, `runjags`, `rjags`, `caret`, `RSBID`, `pROC`, `PRROC`, `tidyverse`

### Configuration

All paths and experimental parameters are centralized in `config.R`. Before running the pipeline, open this file and adjust the settings for your environment:

```r
# Key settings to review:
DATASET_DIR       <- "dataset"           # where .rds data files are stored
RESULTS_DIR       <- "results"           # where prediction outputs are saved
TABLE_OUTPUT_DIR  <- "result_tab_fig"    # where summary tables are saved
FIGURE_OUTPUT_DIR <- file.path(...)      # where PDF figures are saved

# Experimental parameters:
SEED_RANGE   <- 120:219                  # 100 replications
TREE_SIZES   <- c(10, 30, 50, 100, 200, 500)
TRAIN_PROP   <- 0.7                      # train/test split ratio

# MCMC parameters (JAGS):
MCMC_CHAINS      <- 3
MCMC_ADAPT_STEPS <- 1000
MCMC_BURNIN      <- 500
MCMC_SAMPLES     <- 10000
MCMC_THIN        <- 2

# MCMC fallback:
SKIP_MCMC_FALLBACK  <- FALSE             # set TRUE to skip MCMC when H <= FA (GCM = NA)

# R3C4 threshold consistency:
RUN_MCMC_COMPARISON <- FALSE             # set TRUE to run MCMC for all cases (slow)
```

**`SKIP_MCMC_FALLBACK`**: When the OOB-estimated hit rate H is less than or equal to the false alarm rate FA (or the proportion-based threshold is out of [0, 1]), GCM normally runs a full MCMC estimation via JAGS. Setting this to `TRUE` skips the MCMC and sets GCM predictions to NA for those cases. This significantly speeds up datasets with frequent MCMC fallback (e.g., abalone19 with ~30% trigger rate) at the cost of more NA values in GCM results. The `gcm_log.rds` still records `mcmc_triggered = TRUE` and `gcm_failed = TRUE`, so the MCMC fallback frequency can be reported regardless.

The most common change is `FIGURE_OUTPUT_DIR`, which defaults to the authors' Overleaf folder. Change it to your preferred output directory (e.g., `"figures"`).

### Data

Datasets are sourced from the [UCI Machine Learning Repository](https://archive.ics.uci.edu) and [OpenML](https://www.openml.org) and are **not included** in this repository. Place the `.rds` data files in the `dataset/` folder along with the `0. description.csv` metadata file.

The 17 datasets used in the paper are:
| Dataset | Minority Proportion | Instances | Features |
|---------|-------------------|-----------|----------|
| abalone19 | <0.01 | 4177 | 8 |
| page_blocks | 0.02 | 5473 | 10 |
| mammography | 0.02 | 11183 | 6 |
| wine_quality | 0.03 | 6497 | 11 |
| yeast5 | 0.03 | 1484 | 8 |
| letter_A | 0.04 | 20000 | 16 |
| oil_spill | 0.04 | 937 | 49 |
| arrhythmia | 0.06 | 420 | 278 |
| ozone_level | 0.07 | 1847 | 72 |
| sick | 0.08 | 1947 | 27 |
| satimage | 0.10 | 6435 | 36 |
| wholesale | 0.11 | 440 | 7 |
| fertility | 0.12 | 100 | 9 |
| hepatitis | 0.21 | 155 | 19 |
| thyroid_diff | 0.28 | 383 | 16 |
| heart_failure | 0.32 | 299 | 11 |
| breast_cancer | 0.35 | 683 | 9 |

### Running the Analysis

1. **Run the full pipeline** (generates predictions for all methods, datasets, seeds, and tree counts):
   ```r
   source("main.R")
   ```
   Or run in parallel for faster execution:
   ```bash
   bash run_parallel.sh
   ```
   - 100 replications (seeds 120--219)
   - 6 tree sizes: 10, 30, 50, 100, 200, 500
   - 10 methods: RF, GCM, GCM_val, $\lambda_{prior}$, $\lambda_{GM}$, $\lambda_{F1}$, BRF, SMOTE, CSRF, EasyEnsemble
   - Outputs are saved to `results/{dataset}/seed={seed}/ntree={tree}/`.
   - Already-completed runs (where `prediction.rds` exists) are automatically skipped.
   - `main.R` accepts optional command-line arguments for dataset subsetting:
     ```bash
     Rscript main.R 1 6    # runs datasets 1-6 only
     Rscript main.R 14 14  # runs dataset 14 (letter_A) only
     ```

2. **Run R3C4 threshold consistency analysis** (optional, separate from main pipeline):
   ```r
   source("run_r3c4_subset.R")
   ```
   Runs MCMC estimation on a representative subset (2 datasets $\times$ 3 tree sizes $\times$ 15 seeds) to compare proportion-based vs MCMC thresholds. This is decoupled from the main pipeline to avoid slowing it down.

3. **Generate summary tables and figures**:
   ```r
   source("get_performance_summary.R")
   ```
   - Tables are saved to `result_tab_fig/`.
   - Figures (PDF, 300 dpi) are saved to `FIGURE_OUTPUT_DIR` (configured in `config.R`).

### Output Files

| File | Description |
|------|-------------|
| `prediction.rds` | True labels and predictions from all methods |
| `prediction_prob.rds` | Probability outputs from RF and each independently-trained method |
| `gcm_log.rds` | GCM estimation log (H, FA, delta_prop, delta_mcmc, mcmc_comparison, gcm_failed) |
| `DGp_runJagsOut.rds` | MCMC output (only when proportion-based threshold is unsuitable) |

### Summary Tables Generated

| File | Description |
|------|-------------|
| `table3_threshold_ranking_freq.csv` | Ranking frequency for threshold-moving methods |
| `table4_Gmean_ranking_freq.csv` | G-mean ranking frequency (all methods) |
| `table5_F1_ranking_freq.csv` | F1 ranking frequency (all methods) |
| `appendix_full_comparison.csv` | Full pairwise comparison (G-mean and F1) |
| `supplemental_auc.csv` | AUC-ROC and AUC-PRC (supplemental) |
| `mcmc_fallback_frequency.csv` | MCMC fallback trigger frequency per dataset |
| `stratified_Gmean_by_imbalance.csv` | G-mean stratified by imbalance level |
| `stratified_F1_by_imbalance.csv` | F1 stratified by imbalance level |
| `oob_vs_validation_comparison.csv` | OOB vs validation-set GCM comparison |
| `threshold_consistency.csv` | Proportion-based vs MCMC threshold raw data (R3C4) |
| `threshold_consistency_by_dataset.csv` | Threshold consistency summary by dataset |
| `threshold_consistency_by_tree.csv` | Threshold consistency summary by tree size |

### Figures Generated

| File | Description |
|------|-------------|
| `TPR_TNR_*.pdf` | Sensitivity/Specificity trade-off plots by dataset |
| `Precision_TPR_*.pdf` | Precision/Sensitivity trade-off plots by dataset |
| `Dhat_Ghat.pdf` | D̂ vs ĝ estimates across datasets (Figure 7) |

## Reproducibility

The analysis reported in the paper was performed with:

- R 4.2.2, Platform: x86_64-w64-mingw32/x64, Windows 10
- randomForest 4.7-1.1
- runjags 2.2.2-1.1 / rjags 4-14 / coda 0.19-4
- RSBID 0.0.2.0000
- caret 6.0-94
- pROC / PRROC (for AUC computation)
- tidyverse 1.3.2 / ggplot2 3.4.2

## License

This project is provided for academic and research purposes.

## Citation

If you use this code, please cite:

> Yang, H.-H., Ho, T.-Y., & Hsu, Y.-F. (2026). Enhancing random forest performance on imbalanced data using the General Condorcet Model. *Journal of Classification* (minor revision).

Contact: Hau-Hung Yang — hauhungyang@as.edu.tw · https://hardy1yang.github.io
