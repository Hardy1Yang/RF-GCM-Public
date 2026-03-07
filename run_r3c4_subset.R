
# run_r3c4_subset.R
# Purpose: Run MCMC comparison for R3C4 threshold consistency analysis.
# This is separate from the main experiment to avoid slowing it down.
# Only runs on a representative subset sufficient to answer the reviewer.
#
# Strategy: 2 datasets × 3 tree sizes × 15 seeds = 90 runs
# Uses existing gcm_log.rds (updates mcmc_comparison field if missing).

source("config.R")
source("setup.R")
source(file.path("method_code", "RF_related.R"))
source(file.path("method_code", "GCM_observeProp.R"))
source(file.path("method_code", "GCM_MCMC.R"))
source(file.path("method_code", "threshold.R"))

# ── Subset selection ─────────────────────────────────────────
# Representative datasets with 0 MCMC fallback rate:
R3C4_DATASETS <- c("hepatitis", "breast_cancer")
R3C4_TREES    <- c(10, 50, 100)  # 3 tree sizes (500 excluded: too slow for MCMC)
R3C4_SEEDS    <- 120:134         # 15 seeds

# Note: stale mcmc_comparison data was already cleared in the previous run.
# Skipping cleanup to preserve partially-completed data.

dta_description <- read.csv(DATASET_DESC_FILE)

cat("=== R3C4 Threshold Consistency Subset ===\n")
cat("Datasets:", paste(R3C4_DATASETS, collapse = ", "), "\n")
cat("Trees:", paste(R3C4_TREES, collapse = ", "), "\n")
cat("Seeds:", min(R3C4_SEEDS), "to", max(R3C4_SEEDS), "\n")
cat("Total runs:", length(R3C4_DATASETS) * length(R3C4_TREES) * length(R3C4_SEEDS), "\n\n")

for (dta_name in R3C4_DATASETS) {
  row <- dta_description[dta_description$dta_name == dta_name, ]
  if (nrow(row) == 0) { cat("  Skipping", dta_name, "(not found in description)\n"); next }

  dta <- readRDS(file.path(DATASET_DIR, paste0(dta_name, ".rds")))
  target_name <- row$target_name

  for (tree in R3C4_TREES) {
    for (seed in R3C4_SEEDS) {
      dir_name <- file.path(RESULTS_DIR, dta_name, paste0("seed=", seed), paste0("ntree=", tree))
      log_file <- file.path(dir_name, "gcm_log.rds")

      # Skip if mcmc_comparison already exists
      if (file.exists(log_file)) {
        log <- readRDS(log_file)
        if (!is.null(log$mcmc_comparison) && !is.na(log$mcmc_comparison)) next
        # Skip if MCMC was triggered (delta_mcmc already serves as comparison)
        if (isTRUE(log$mcmc_triggered)) next
      }

      # Need to fit RF and compute MCMC comparison
      if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)
      cat(dir_name, "\n")

      # Split data
      if (STRATIFY) {
        split <- train_test_split_stratify(dta, TRAIN_PROP, target_name, seed)
      } else {
        split <- train_test_split(dta, TRAIN_PROP, target_name, seed)
      }
      train_data <- split[[1]]
      prop <- mean(as.numeric(train_data[[target_name]])) - 1

      # Fit RF (needed for OOB predictions)
      set.seed(seed)
      rf <- randomForest::randomForest(
        as.formula(paste(target_name, "~ .")),
        data = train_data, ntree = tree, keep.inbag = TRUE
      )

      # Proportion-based estimates
      est <- estimate_H_FA(train_data, target_name, rf)
      delta_prop <- find_gcm_threshold(pi = prop, param1 = est[["H"]], param2 = est[["FA"]], N = tree, type = "HF")

      # MCMC estimation
      pred_tree_level_oob <- get_pred_tree_level_oob(rf, train_data)
      nitem_train <- dim(pred_tree_level_oob)[2]
      res <- GCM_save_data_estiamte(
        pred_tree_level_oob, prop, dir_name,
        ntree = tree, nitem = nitem_train, fixed = "DG2",
        nChains = MCMC_CHAINS, nAdaptSteps = MCMC_ADAPT_STEPS,
        nBurninSteps = MCMC_BURNIN, nUseSteps = MCMC_SAMPLES,
        nThinSteps = MCMC_THIN, inits = NULL
      )
      mcmc_comparison <- NA
      if (!is.null(res)) {
        mcmc_comparison <- find_gcm_threshold(res$pHat, res$DHat, res$GHat, tree, type = "Dg")
      }

      # Update or create gcm_log.rds
      if (file.exists(log_file)) {
        log <- readRDS(log_file)
        log$mcmc_comparison <- mcmc_comparison
        saveRDS(log, log_file)
      } else {
        saveRDS(list(
          mcmc_triggered = FALSE, H = est[["H"]], FA = est[["FA"]],
          delta = delta_prop, delta_prop = delta_prop, delta_mcmc = NA,
          mcmc_comparison = mcmc_comparison, gcm_failed = FALSE
        ), log_file)
      }
    }
  }
  cat("  Done:", dta_name, "\n")
}

cat("\n=== R3C4 subset complete ===\n")
cat("Run get_performance_summary.R to generate threshold_consistency tables.\n")
