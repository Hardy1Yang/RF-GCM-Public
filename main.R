
# main.R
# Purpose: Run the full analysis pipeline for the paper
# Output: The following files may be saved in the `results/` folder:
#         - `prediction.rds` (key result): contains the true labels and predictions from all methods.
#         - `prediction_prob.rds`: probability outputs from RF and each independently-trained method (BRF, SMOTE, CSRF, EasyEnsemble).
#         - `gcm_log.rds`: GCM estimation log (H, FA, delta, MCMC trigger status).
#         - `dta.rds`: basic information, including training data, testing data, and the randomForest object.
#         - `DGp_runJagsOut.rds`: output of MCMC estimation; generated only when the threshold is not suitable based on the observed proportion.
# Notes: This script sources `setup.R` to load packages; 
#        see README for the R and package versions used in our analysis.

source("config.R")
source("setup.R")
source(file.path("method_code", "RF_related.R"))
source(file.path("method_code",'GCM_observeProp.R'))
source(file.path("method_code",'GCM_MCMC.R'))
source(file.path("method_code", 'threshold.R'))
source(file.path("method_code", 'other_imbalanced_methods.R'))


fitAll = function(data, dta_name, target_name, ntree, seed_num, GCM_model_nm,
                  stratify = F, train_prop=0.7, maxnodes=NULL, 
                  nChains = 3, nAdaptSteps = 1000, nBurninSteps = 500, nUseSteps = 10000, nThinSteps = 2, inits_override=NULL){
  
  dir_name = file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num), paste0("ntree=", ntree))
  print(dir_name)   
  
  # step1: split data into training/ testing data ----
  if (stratify){
    split = train_test_split_stratify(data, train_prop, target_name, seed_num)
  } else {
    split = train_test_split(data, train_prop, target_name, seed_num)
  }
  train_data = split[[1]]
  test_data = split[[2]]
  nitem = dim(test_data)[1]
  prop = mean(as.numeric(train_data[[target_name]]))-1
  
  # step2: run RF and save data to dta.rds----
  RF_obj = fit_and_save_RF(dta_name = dta_name, target_name = target_name, train_data = train_data, 
                           test_data = test_data, ntree = ntree, seed_num = seed_num)

  PRED = list()
  PRED[["true"]] = RF_obj$trueResult
  PRED[["RF"]] = RF_obj$RFResult

  # step3: GCM. Check observation proportion first; run MCMC if inappropriate. ----
  est = estimate_H_FA(train_data = train_data, target_name = target_name, rf = RF_obj[["rf"]])
  delta_prop = find_gcm_threshold(pi = prop, param1 = est[["H"]], param2 = est[["FA"]], N = ntree, type="HF")

  mcmc_triggered = FALSE
  delta_mcmc = NA
  if ((est[["H"]] <= est[["FA"]]) | delta_prop < 0 | delta_prop > 1){
    mcmc_triggered = TRUE
    if (!SKIP_MCMC_FALLBACK){
      pred_tree_level_oob = get_pred_tree_level_oob(RF_obj$rf, train_data)
      nitem_train = dim(pred_tree_level_oob)[2]
      res = GCM_save_data_estiamte(pred_tree_level_oob, prop, dir_name, ntree=ntree, nitem=nitem_train, fixed="DG2",
                                   nChains, nAdaptSteps, nBurninSteps,
                                   nUseSteps, nThinSteps, inits=inits_override)
      if (is.null(res)){
        error_log <<- c(error_log, dir_name)
      } else {
        delta_mcmc = find_gcm_threshold(res$pHat, res$DHat, res$GHat, ntree, type="Dg")
      }
    }
  }

  # R3C4: Also run MCMC when H > F for threshold consistency comparison
  mcmc_comparison = NA
  if (RUN_MCMC_COMPARISON && !mcmc_triggered){
    pred_tree_level_oob = get_pred_tree_level_oob(RF_obj$rf, train_data)
    nitem_train = dim(pred_tree_level_oob)[2]
    res_comp = GCM_save_data_estiamte(pred_tree_level_oob, prop, dir_name,
                                       ntree=ntree, nitem=nitem_train, fixed="DG2",
                                       nChains, nAdaptSteps, nBurninSteps,
                                       nUseSteps, nThinSteps, inits=inits_override)
    if (!is.null(res_comp)){
      mcmc_comparison = find_gcm_threshold(res_comp$pHat, res_comp$DHat, res_comp$GHat, ntree, type="Dg")
    }
  }

  # Final delta used for prediction
  gcm_failed = FALSE
  if (mcmc_triggered && is.na(delta_mcmc)){
    # MCMC was needed but failed; delta_prop is unreliable — mark as failed
    gcm_failed = TRUE
    delta = NA
  } else if (mcmc_triggered){
    delta = delta_mcmc
  } else {
    delta = delta_prop
  }

  # Save GCM estimation log for sensitivity analysis
  saveRDS(list(mcmc_triggered = mcmc_triggered, H = est[["H"]], FA = est[["FA"]],
               delta = delta, delta_prop = delta_prop, delta_mcmc = delta_mcmc,
               mcmc_comparison = mcmc_comparison, gcm_failed = gcm_failed),
          file.path(dir_name, "gcm_log.rds"))

  if (gcm_failed){
    PRED[["GCM"]] = factor(rep(NA, length(RF_obj[["pred_p"]])), levels = c("0", "1"))
  } else {
    PRED[["GCM"]] = as.factor(as.numeric(RF_obj[["pred_p"]] > delta))
  }

  # step3b: GCM with validation-set estimation (for OOB vs validation comparison) ----
  val_est = estimate_H_FA_validation(test_data, target_name, RF_obj[["rf"]])
  val_delta = find_gcm_threshold(pi = prop, param1 = val_est[["H"]], param2 = val_est[["FA"]], N = ntree, type="HF")
  if (!gcm_failed && !is.na(val_delta) && val_delta >= 0 && val_delta <= 1){
    PRED[["GCM_val"]] = as.factor(as.numeric(RF_obj[["pred_p"]] > val_delta))
  } else {
    PRED[["GCM_val"]] = PRED[["GCM"]]  # fallback to OOB version (or NA if failed)
  }
  
  # step4: threshold moving ----
  # performance-based: threshold that max OOB samples G-mean / F1-score
  # prior: threshold = proportion(class1)
  
  opt_pred = get_opt_pred(rf = RF_obj$rf, train_data = train_data, 
               target_nm = target_name, pred_p = RF_obj$pred_p)
  PRED[["lamda_GM"]] = opt_pred$GM
  PRED[["lamda_FM"]] = opt_pred$FM
  PRED[["lamda_prior"]] = factor(as.numeric(RF_obj$pred_p > prop))
  
  # step5: other method (BRF, SMOTE, CSRF, EasyEnsemble)----
  brf_result = get_BRF_pred(seed_num, tree_nm = ntree, train_data, test_data, target_nm = target_name)
  PRED[["BRF"]] = brf_result$pred
  smote_result = get_SMOTE_pred(seed_num, tree_nm = ntree, train_data, test_data, target_nm = target_name)
  PRED[["SMOTE"]] = smote_result$pred
  csrf_result = get_CSRF_pred(seed_num, tree_nm = ntree, train_data, test_data, target_nm = target_name)
  PRED[["CSRF"]] = csrf_result$pred
  ee_result = get_EasyEnsemble_pred(seed_num, tree_nm = ntree, train_data, test_data, target_nm = target_name)
  PRED[["EasyEnsemble"]] = ee_result$pred

  saveRDS(PRED, file.path(dir_name, "prediction.rds"))

  # Save probability outputs for AUC computation ----
  PRED_PROB = list()
  PRED_PROB[["true"]] = RF_obj$trueResult
  PRED_PROB[["RF"]] = RF_obj$pred_p  # RF probability (shared by all threshold-moving methods)
  PRED_PROB[["BRF"]] = brf_result$prob
  PRED_PROB[["SMOTE"]] = smote_result$prob
  PRED_PROB[["CSRF"]] = csrf_result$prob
  PRED_PROB[["EasyEnsemble"]] = ee_result$prob

  saveRDS(PRED_PROB, file.path(dir_name, "prediction_prob.rds"))
  return(NULL)
}



# run ----
# Usage: Rscript main.R [start_index end_index [seed_start seed_end]]
# Example: Rscript main.R 1 6           # runs datasets 1-6, all seeds
#          Rscript main.R 9 9 120 149    # runs dataset 9, seeds 120-149
#          Rscript main.R                # runs all datasets, all seeds
dta_description = read.csv(DATASET_DESC_FILE)
args = commandArgs(trailingOnly = TRUE)
if (length(args) >= 2){
  start_idx = as.integer(args[1])
  end_idx = as.integer(args[2])
  select = start_idx:end_idx
  cat("Running datasets", start_idx, "to", end_idx, "\n")
} else {
  select = seq_len(nrow(dta_description))
}
if (length(args) >= 4){
  SEED_RANGE = as.integer(args[3]):as.integer(args[4])
  cat("Seed range:", min(SEED_RANGE), "to", max(SEED_RANGE), "\n")
}
file_name_vec = dta_description[, "dta_name"][select]
print(file_name_vec)
file_route_vec = lapply(file_name_vec,
                        function(x){file.path(DATASET_DIR, paste0(x, ".rds"))}) |> unlist()
target_name_vec = dta_description[, "target_name"][select]
target_num_vec = dta_description[, "target"][select]
model_nm_vec = c(GCM_MODEL_NAME)

error_log = c()
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR)
for (f_index in seq_along(file_name_vec)){
  dta = read_rds(file_route_vec[f_index])

  for (tree in TREE_SIZES){
    for (seed in SEED_RANGE){
      out_dir = file.path(RESULTS_DIR, file_name_vec[f_index], paste0("seed=", seed), paste0("ntree=", tree))
      if (file.exists(file.path(out_dir, "prediction.rds"))) next
      fitAll(data = dta, dta_name = file_name_vec[f_index], target_name = target_name_vec[f_index],
             ntree = tree, seed_num = seed, GCM_model_nm = model_nm_vec,
             stratify = STRATIFY, train_prop = TRAIN_PROP, maxnodes = NULL,
             nChains = MCMC_CHAINS, nAdaptSteps = MCMC_ADAPT_STEPS,
             nBurninSteps = MCMC_BURNIN, nUseSteps = MCMC_SAMPLES,
             nThinSteps = MCMC_THIN, inits_override = NULL)
    }
  }
}

