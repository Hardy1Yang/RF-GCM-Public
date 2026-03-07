
# config.R
# Central configuration file for the RF-GCM project.
# Modify this file to adjust paths and parameters for your environment.
# All other scripts read from these variables via source("config.R").

# ── Directories ───────────────────────────────────────────────
DATASET_DIR       <- "dataset"
RESULTS_DIR       <- "results"
TABLE_OUTPUT_DIR  <- "result_tab_fig"

# Set FIGURE_OUTPUT_DIR to the folder where generated plots should be saved.
# By default this points to the Overleaf project's figures/ folder.
# Change this path to match your local setup.
FIGURE_OUTPUT_DIR <- file.path(
  Sys.getenv("HOME"),
  "Library", "CloudStorage", "Dropbox",
  "\u61c9\u7528\u7a0b\u5f0f", "Overleaf", "RF-CCT", "figures"
)

DATASET_DESC_FILE <- file.path(DATASET_DIR, "0. description.csv")

# ── Experimental parameters ───────────────────────────────────
SEED_RANGE   <- 120:219        # 100 replications
TREE_SIZES   <- c(10, 30, 50, 100, 200, 500)
TRAIN_PROP   <- 0.7            # 70/30 train-test split
STRATIFY     <- TRUE           # stratified split preserving class ratio

# ── MCMC parameters (JAGS) ───────────────────────────────────
MCMC_CHAINS      <- 3
MCMC_ADAPT_STEPS <- 1000
MCMC_BURNIN      <- 500
MCMC_SAMPLES     <- 10000
MCMC_THIN        <- 2

# ── Method parameters ────────────────────────────────────────
SMOTE_RATIO       <- 100       # SMOTE oversampling percentage
SMOTE_K           <- 5         # SMOTE k-nearest neighbors
EASYENSEMBLE_NSUB <- 10        # number of EasyEnsemble sub-ensembles
GCM_MODEL_NAME    <- "DG2"     # JAGS model variant

# ── MCMC fallback ─────────────────────────────────────────────
# When TRUE, runs MCMC (JAGS) estimation when H <= FA or delta_prop
# is out of [0, 1]. When FALSE, GCM is set to NA for those cases,
# which significantly speeds up datasets with frequent fallback
# (e.g., abalone19). The gcm_log.rds still records mcmc_triggered
# and gcm_failed so the frequency can be reported.
SKIP_MCMC_FALLBACK <- TRUE

# ── R3C4 threshold consistency ──────────────────────────────
# When TRUE, runs an extra MCMC estimation even when H > FA (for
# comparing proportion-based vs MCMC thresholds).  Set to FALSE
# to speed up the main experiment; run run_r3c4_subset.R separately.
RUN_MCMC_COMPARISON <- FALSE

# ── Derived constants (do not modify) ─────────────────────────
N_SEEDS <- length(SEED_RANGE)
ALL_METHODS       <- c("lamda_prior", "BRF", "GCM", "lamda_GM", "lamda_FM",
                        "SMOTE", "CSRF", "EasyEnsemble", "RF")
THRESHOLD_METHODS <- c("lamda_prior", "GCM", "lamda_GM", "lamda_FM")
