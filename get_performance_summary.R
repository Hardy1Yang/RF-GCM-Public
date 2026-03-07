
# This script generates the main tables and figures for the paper.
# Run this after executing `main.R`.
# Tables are saved in TABLE_OUTPUT_DIR; figures are saved to FIGURE_OUTPUT_DIR.
# All paths and parameters are loaded from config.R.

source("config.R")

library(caret)
library(psych)
library(tidyverse)
library(pROC)
library(PRROC)

dta_des = read.csv(DATASET_DESC_FILE)
file_nm = dta_des[["dta_name"]]

# ---------- 1. Collect classification performance ----------
pred_result = data.frame()

missing_files = c()
for (i in seq_along(file_nm)){
  print(file_nm[i])
  for (seed in SEED_RANGE){
    print(paste0("seed_num=",  seed))
    for (tree in TREE_SIZES){
      dir_name = file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=", tree))
      pred_file = file.path(dir_name, "prediction.rds")
      if (!file.exists(pred_file)){
        missing_files = c(missing_files, pred_file)
        next
      }
      predictionn = readRDS(pred_file)
      trueResult = predictionn$true

      for (j in 2:length(predictionn)){
        Result = predictionn[[j]]
        # Skip methods with all-NA predictions (e.g. GCM failed cases)
        if (all(is.na(Result))) next
        CM = confusionMatrix(factor(Result, levels=c(0,1)),
                             factor(trueResult, levels=c(0,1)), positive = "1")$byClass
        CM_table = confusionMatrix(factor(Result, levels=c(0,1)),
                                   factor(trueResult, levels=c(0,1)), positive = "1")$table
        A = CM_table[1,1]; B = CM_table[1,2]; C = CM_table[2,1]; D = CM_table[2,2]
        Gmean = sqrt((A/(A+C))*(D/(B+D)))
        ACC = mean(factor(Result ,levels=c(0,1)) == factor(trueResult, levels=c(0,1)))

        pred_result = rbind(pred_result, c(file_nm[i], seed, tree, names(predictionn[j]), ACC, CM, Gmean))
      }
    }
  }
}
if (length(missing_files) > 0) warning(paste0(length(missing_files), " prediction.rds files missing; skipped."))


colnames(pred_result) = c("file_name", "seed", "tree", "model", "ACC", names(CM), "Gmean")
performance_idx = colnames(pred_result)[5:17]
pred_result$tree = as.numeric(pred_result$tree)
pred_result_summary = pred_result |>
  group_by(file_name, tree, model) |>
  summarize(
    across(all_of(performance_idx),
           list(avg = ~ mean(as.numeric(replace_na(as.numeric(.x), 0))),
                mdn = ~ median(as.numeric(replace_na(as.numeric(.x), 0))),
                se = ~ sd(as.numeric(replace_na(as.numeric(.x), 0))) / sqrt(N_SEEDS)),
           .names = "{fn}_{col}"),
    .groups = 'drop'
  )


# ---------- 2. Collect AUC-ROC and AUC-PRC ----------
auc_result = data.frame()

for (i in seq_along(file_nm)){
  print(paste0("AUC: ", file_nm[i]))
  for (seed in SEED_RANGE){
    for (tree in TREE_SIZES){
      dir_name = file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=", tree))
      prob_file = file.path(dir_name, "prediction_prob.rds")
      pred_file = file.path(dir_name, "prediction.rds")

      if (!file.exists(prob_file) || !file.exists(pred_file)) next
      pred_prob = readRDS(prob_file)
      predictionn = readRDS(pred_file)
      trueResult = as.numeric(as.character(predictionn$true))

      # RF probability (shared by all threshold-moving methods)
      rf_prob = pred_prob[["RF"]]
      if (!is.null(rf_prob) && !all(is.na(rf_prob))){
        roc_obj = roc(trueResult, rf_prob, quiet = TRUE)
        auc_roc_rf = as.numeric(auc(roc_obj))
        pr_obj = pr.curve(scores.class0 = rf_prob[trueResult == 1],
                          scores.class1 = rf_prob[trueResult == 0])
        auc_prc_rf = pr_obj$auc.integral
      } else {
        auc_roc_rf = NA; auc_prc_rf = NA
      }

      # All threshold-moving methods share the RF probability
      for (m in c("RF", "GCM", "GCM_val", "lamda_GM", "lamda_FM", "lamda_prior")){
        auc_result = rbind(auc_result,
          data.frame(file_name = file_nm[i], seed = seed, tree = tree,
                     model = m, AUC_ROC = auc_roc_rf, AUC_PRC = auc_prc_rf))
      }

      # Methods with their own probability outputs
      for (m in c("BRF", "CSRF", "SMOTE", "EasyEnsemble")){
        m_prob = pred_prob[[m]]
        if (!is.null(m_prob) && !all(is.na(m_prob))){
          roc_obj = roc(trueResult, m_prob, quiet = TRUE)
          auc_roc_m = as.numeric(auc(roc_obj))
          pr_obj = pr.curve(scores.class0 = m_prob[trueResult == 1],
                            scores.class1 = m_prob[trueResult == 0])
          auc_prc_m = pr_obj$auc.integral
        } else {
          auc_roc_m = NA; auc_prc_m = NA
        }
        auc_result = rbind(auc_result,
          data.frame(file_name = file_nm[i], seed = seed, tree = tree,
                     model = m, AUC_ROC = auc_roc_m, AUC_PRC = auc_prc_m))
      }
    }
  }
}

auc_summary = auc_result |>
  group_by(file_name, tree, model) |>
  summarise(
    avg_AUC_ROC = mean(AUC_ROC, na.rm = TRUE),
    se_AUC_ROC  = sd(AUC_ROC, na.rm = TRUE) / sqrt(N_SEEDS),
    avg_AUC_PRC = mean(AUC_PRC, na.rm = TRUE),
    se_AUC_PRC  = sd(AUC_PRC, na.rm = TRUE) / sqrt(N_SEEDS),
    .groups = 'drop'
  )


# ---------- 3. Variable assignments for convenience ----------
avg_performance = "avg_F1"; se_performance = "se_F1"; name_col = "F_mean"
avg_performance = "avg_Gmean"; se_performance = "se_Gmean"; name_col = "G_mean"
avg_performance = "avg_Sensitivity"; se_performance = "se_Sensitivity"; name_col = "TPR_mean"
avg_performance = "avg_Specificity"; se_performance = "se_Specificity"; name_col = "TNR_mean"
avg_performance = "avg_Precision"; se_performance = "se_Precision"; name_col = "Precision"


# ---------- 4. Summary functions ----------

get_full_summarize = function(avg_performance){
  se_col = sub("^avg_", "se_", avg_performance)
  x = pred_result_summary |>
    filter(model %in% ALL_METHODS) |>
    mutate(name_col = paste0(sprintf("%.3f", !!sym(avg_performance)), "(", sprintf("%.3f", !!sym(se_col)), ")")) |>
    dplyr::select(file_name, tree, model, name_col) |>
    filter(tree==100) |>
    pivot_wider(names_from = model, values_from = name_col) |>
    dplyr::select(file_name, all_of(ALL_METHODS))
  return(x)
}


get_rank_frequency_table = function(avg_performance, type_of_methods){
  # avg_performance = {"avg_Gmean", "avg_F1"}
  # type_of_methods = {"threshold", "all"}

  if (type_of_methods == "all"){
    methods = ALL_METHODS
    x = pred_result_summary |>
      filter(tree == 100, model %in% methods)
  }
  if (type_of_methods == "threshold"){
    methods = THRESHOLD_METHODS
    x = pred_result_summary |>
      filter(tree == 100, model %in% methods)
  }

  n_methods = length(methods)

  x = x |> dplyr::select(file_name, model, !!sym(avg_performance)) |>
       group_by(file_name) |>
       mutate(rank=rank(-round(!!sym(avg_performance),3), ties.method="min")) |>
       ungroup() |>
       dplyr::select(file_name, model, rank) |>
       pivot_wider(names_from = model, values_from = rank)

  x = x |> dplyr::select(all_of(methods)) |>
    as.data.frame() |>
    sapply(table, useNA = "ifany", simplify = FALSE)

  rank_table = data.frame(matrix(rep(0, n_methods * (n_methods + 1)), n_methods))
  colnames(rank_table) = c("Rank", names(x))
  rank_table["Rank"] = seq_len(n_methods)

  # m: method
  # r: rank
  for (m in seq_along(x)){
    for (r in seq_along(x[[m]])){
      rank_table[as.numeric(names(x[[m]][r])) ,
                 names(x[m])] = unname(x[[m]][r])
    }
  }
  return(rank_table)
}


# ---------- 5. Plot functions ----------

save_plot_precision_TPR = function(){
  result = pred_result_summary |>
    group_by(model, tree)  |>
    summarise(sensitivity = mean(avg_Sensitivity),
              precision = mean(avg_Precision))  |>
    ungroup() |>
    filter(model %in% c("RF", "SMOTE", "GCM", "BRF", "CSRF", "EasyEnsemble", "lamda_prior"))

  result_long <- result %>%
    pivot_longer(cols = c("sensitivity", "precision"), names_to = "metric", values_to = "value")
  result_long$metric = factor(result_long$metric, levels=c("sensitivity", "precision"))

  ggplot(result_long, aes(x = tree, y = value, color = model, linetype = metric)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = TREE_SIZES) +
    labs(title = " ",
         x = "Number of trees",
         y = "Sensitivity / Precision",
         color = "Method",
         linetype = "Metric") +
    theme_minimal()+
    theme(text = element_text(size = 16))

  ggsave(file.path(FIGURE_OUTPUT_DIR, "precisionTPR.pdf"), width = 6, height = 5, bg = "white", dpi = 300)
}

save_plot_TPR_TNR = function(){
  result = pred_result_summary |>
    group_by(model, tree)  |>
    summarise(sensitivity = mean(avg_Sensitivity),
              specificity = mean(avg_Specificity))  |>
    ungroup() |>
    filter(model %in% c("RF", "SMOTE", "GCM", "BRF", "CSRF", "EasyEnsemble", "lamda_prior"))

  result_long <- result %>%
    pivot_longer(cols = c("sensitivity", "specificity"), names_to = "metric", values_to = "value")
  result_long$metric = factor(result_long$metric, levels=c("sensitivity", "specificity"))

  ggplot(result_long, aes(x = tree, y = value, color = model, linetype = metric)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = TREE_SIZES) +
    labs(title = " ",
         x = "Number of trees",
         y = "Sensitivity / Specificity",
         color = "Method",
         linetype = "Metric") +
    theme_minimal()+
    theme(text = element_text(size = 16))

  ggsave(file.path(FIGURE_OUTPUT_DIR, "TPRTNR.pdf"), width = 6, height = 5, bg = "white", dpi = 300)
}

save_plot_Dhat_Ghat = function(){
  # Figure 7: D-hat and g-hat estimates across datasets (from gcm_log)
  # Shows how guessing parameter g tracks the class proportion
  dg_data = data.frame()
  for (i in seq_along(file_nm)){
    for (seed in SEED_RANGE){
      dir_name = file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=100"))
      log_file = file.path(dir_name, "gcm_log.rds")
      if (file.exists(log_file)){
        log = readRDS(log_file)
        H = log$H; FA = log$FA
        D_hat = H - FA
        g_hat = if (D_hat < 1) FA / (1 - D_hat) else NA
        dg_data = rbind(dg_data,
          data.frame(file_name = file_nm[i], seed = seed,
                     D_hat = D_hat, g_hat = g_hat))
      }
    }
  }

  dg_summary = dg_data |>
    group_by(file_name) |>
    summarise(D_hat = mean(D_hat, na.rm = TRUE),
              g_hat = mean(g_hat, na.rm = TRUE), .groups = "drop") |>
    left_join(dta_des, by = c("file_name" = "dta_name"))

  ggplot(dg_summary, aes(x = D_hat, y = g_hat)) +
    geom_point(size = 3) +
    geom_text(aes(label = file_name), vjust = -0.8, size = 3) +
    geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "grey50") +
    labs(x = expression(hat(D)), y = expression(hat(g)),
         title = " ") +
    theme_minimal() +
    theme(text = element_text(size = 14))

  ggsave(file.path(FIGURE_OUTPUT_DIR, "Dhat_Ghat.pdf"),
         width = 7, height = 5, bg = "white", dpi = 300)
}


# ---------- 6. Sensitivity analysis functions ----------

get_mcmc_frequency_table = function(){
  # Report MCMC fallback frequency per dataset x tree (R3C4)
  mcmc_log = data.frame()
  for (i in seq_along(file_nm)){
    for (seed in SEED_RANGE){
      for (tree in TREE_SIZES){
        dir_name = file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=", tree))
        log_file = file.path(dir_name, "gcm_log.rds")
        if (file.exists(log_file)){
          log = readRDS(log_file)
          mcmc_log = rbind(mcmc_log,
            data.frame(file_name = file_nm[i], seed = seed, tree = tree,
                       mcmc_triggered = log$mcmc_triggered))
        }
      }
    }
  }
  mcmc_log |> group_by(file_name, tree) |>
    summarise(mcmc_freq = mean(mcmc_triggered), .groups = "drop") |>
    pivot_wider(names_from = tree, values_from = mcmc_freq)
}


get_stratified_analysis = function(avg_performance){
  # Stratified analysis by imbalance ratio (R1C3 / R3C5)
  dta_des_with_group = dta_des |>
    mutate(imbalance_group = case_when(
      prop.1. < 0.10 ~ "extreme (<10%)",
      prop.1. < 0.25 ~ "moderate (10-25%)",
      TRUE ~ "mild (>25%)"
    ))
  pred_result_summary |>
    filter(model %in% ALL_METHODS) |>
    left_join(dta_des_with_group, by = c("file_name" = "dta_name")) |>
    filter(tree == 100) |>
    group_by(imbalance_group, model) |>
    summarise(mean_perf = mean(!!sym(avg_performance), na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = model, values_from = mean_perf)
}


get_oob_vs_val_comparison = function(){
  # Compare GCM (OOB) vs GCM_val (validation-set) for G-mean and F1 (R3C6)
  pred_result_summary |>
    filter(model %in% c("GCM", "GCM_val"), tree == 100) |>
    dplyr::select(file_name, model, avg_Gmean, avg_F1) |>
    pivot_wider(names_from = model, values_from = c(avg_Gmean, avg_F1))
}


get_threshold_consistency = function(){
  # R3C4: Compare proportion-based and MCMC thresholds
  threshold_data = data.frame()
  for (i in seq_along(file_nm)){
    for (seed in SEED_RANGE){
      for (tree in TREE_SIZES){
        dir_name = file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=", tree))
        log_file = file.path(dir_name, "gcm_log.rds")
        if (file.exists(log_file)){
          log = readRDS(log_file)
          # Use mcmc_comparison for H>F cases, delta_mcmc for fallback cases
          mcmc_val = if (!is.null(log$mcmc_comparison) && !is.na(log$mcmc_comparison)) {
            log$mcmc_comparison
          } else if (!is.null(log$delta_mcmc)) {
            log$delta_mcmc
          } else NA
          threshold_data = rbind(threshold_data,
            data.frame(file_name = file_nm[i], seed = seed, tree = tree,
                       delta_prop = if (!is.null(log$delta_prop)) log$delta_prop else NA,
                       delta_mcmc = mcmc_val,
                       mcmc_triggered = log$mcmc_triggered))
        }
      }
    }
  }
  threshold_data
}

get_threshold_consistency_summary = function(){
  # R3C4 summary: statistics for H > F cases where both estimates are available
  raw = get_threshold_consistency()
  comp = raw |>
    filter(!mcmc_triggered, !is.na(delta_prop), !is.na(delta_mcmc),
           delta_prop >= 0, delta_prop <= 1, delta_mcmc >= 0, delta_mcmc <= 1) |>
    mutate(abs_diff = abs(delta_prop - delta_mcmc))

  by_dataset = comp |>
    group_by(file_name) |>
    summarise(n = n(),
              mean_prop = mean(delta_prop),
              mean_mcmc = mean(delta_mcmc),
              mean_abs_diff = mean(abs_diff),
              median_abs_diff = median(abs_diff),
              cor = if (n() > 2) cor(delta_prop, delta_mcmc) else NA,
              .groups = "drop")

  by_tree = comp |>
    group_by(tree) |>
    summarise(n = n(),
              mean_abs_diff = mean(abs_diff),
              median_abs_diff = median(abs_diff),
              cor = if (n() > 2) cor(delta_prop, delta_mcmc) else NA,
              .groups = "drop")

  list(by_dataset = by_dataset, by_tree = by_tree)
}


# ---------- 7. Generate outputs ----------

if (!dir.exists(TABLE_OUTPUT_DIR)) dir.create(TABLE_OUTPUT_DIR, recursive = TRUE)
if (!dir.exists(FIGURE_OUTPUT_DIR)) dir.create(FIGURE_OUTPUT_DIR, recursive = TRUE)

# Main ranking tables
write.csv(get_rank_frequency_table("avg_Gmean", "all"),
          file.path(TABLE_OUTPUT_DIR, "table4_Gmean_ranking_freq.csv"), row.names = F)
write.csv(get_rank_frequency_table("avg_F1", "all"),
          file.path(TABLE_OUTPUT_DIR, "table5_F1_ranking_freq.csv"), row.names = F)

# Threshold-only ranking table
write.csv(cbind(get_rank_frequency_table("avg_Gmean", "threshold"),
                get_rank_frequency_table("avg_F1", "threshold")[,2:5]),
          file.path(TABLE_OUTPUT_DIR, "table3_threshold_ranking_freq.csv"), row.names = F)

# Full comparison (appendix)
write.csv(rbind(get_full_summarize("avg_Gmean"),
                get_full_summarize("avg_F1")),
          file.path(TABLE_OUTPUT_DIR, "appendix_full_comparison.csv"), row.names = F)

# AUC supplemental table
write.csv(auc_summary |> filter(tree == 100, model %in% ALL_METHODS),
          file.path(TABLE_OUTPUT_DIR, "supplemental_auc.csv"), row.names = F)

# Sensitivity analysis tables
write.csv(get_mcmc_frequency_table(),
          file.path(TABLE_OUTPUT_DIR, "mcmc_fallback_frequency.csv"), row.names = F)
write.csv(get_stratified_analysis("avg_Gmean"),
          file.path(TABLE_OUTPUT_DIR, "stratified_Gmean_by_imbalance.csv"), row.names = F)
write.csv(get_stratified_analysis("avg_F1"),
          file.path(TABLE_OUTPUT_DIR, "stratified_F1_by_imbalance.csv"), row.names = F)
write.csv(get_oob_vs_val_comparison(),
          file.path(TABLE_OUTPUT_DIR, "oob_vs_validation_comparison.csv"), row.names = F)

# Threshold consistency (raw + summary)
write.csv(get_threshold_consistency(),
          file.path(TABLE_OUTPUT_DIR, "threshold_consistency.csv"), row.names = F)
tc_summary = get_threshold_consistency_summary()
write.csv(tc_summary$by_dataset,
          file.path(TABLE_OUTPUT_DIR, "threshold_consistency_by_dataset.csv"), row.names = F)
write.csv(tc_summary$by_tree,
          file.path(TABLE_OUTPUT_DIR, "threshold_consistency_by_tree.csv"), row.names = F)

# Figures
save_plot_TPR_TNR()
save_plot_precision_TPR()
save_plot_Dhat_Ghat()
