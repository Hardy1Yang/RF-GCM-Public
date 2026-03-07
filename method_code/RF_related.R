
# RF_related.R
# Purpose: Run step1 in `main.R`

train_test_split = function(data, train_prop, target_name, seed_num){
  N = nrow(data)
  check_zero = T
  while (check_zero){
    set.seed(seed_num)
    train_idx = sample(1:N, ceiling(N*train_prop))
    train_dta = data[train_idx, ]
    test_dta = data[-train_idx, ]
    if(train_dta[[target_name]] |> table() |> min() == 0){
      seed_num = runif(1)*1000 |> ceiling()
    } else {
      check_zero = F
    }
  }
  return(list(train_dta, test_dta))
}

train_test_split_stratify = function(data, train_prop, target_name, seed_num){
  N = nrow(data)
  check_zero = T
  while (check_zero){
    set.seed(seed_num)
    
    # stratify (find target = "0", "1" index first, then sample)
    idx_0 = which(data[[target_name]] == "0")
    train_idx_0 = sample(idx_0, ceiling(length(idx_0)*train_prop))
    idx_1 = which(data[[target_name]] == "1")
    train_idx_1 = sample(idx_1, ceiling(length(idx_1)*train_prop))
    
    train_dta = data[c(train_idx_0, train_idx_1), ]
    test_dta = data[-c(train_idx_0, train_idx_1), ]
    if(train_dta[[target_name]] |> table() |> min() == 0){
      seed_num = runif(1)*1000 |> ceiling()
    } else {
      check_zero = F
    }
  }
  return(list(train_dta, test_dta))
}


fit_and_save_RF = function(dta_name, target_name, train_data, test_data, ntree, seed_num){
  
  # 1. fit RF, get predictions at tree level & forest level
  formula = as.formula(paste0(target_name, "~."))
  set.seed(seed_num)
  rf = randomForest(formula, data = train_data, ntree = ntree, 
                    prox=T, norm.votes = F, keep.inbag = T)
  
  pred_forest_level = predict(rf, test_data, predict.all = TRUE)[[1]]   # 長度為 nitem 的向量
  pred_tree_level = predict(rf, test_data, predict.all = TRUE)[[2]] |> t()  # ntree*nitem 的矩陣
  acc_test = mean(pred_forest_level == test_data[[target_name]])
  
  # 2. Create path
  if (!dir.exists(file.path(RESULTS_DIR, dta_name))){
    dir.create(file.path(RESULTS_DIR, dta_name))
  }
  if (!dir.exists(file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num)))){
    dir.create(file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num)))
  }
  if (!dir.exists(file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num), paste0("ntree=", ntree)))){
    dir.create(file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num), paste0("ntree=", ntree)))
  }
  dir_name = file.path(RESULTS_DIR, dta_name, paste0("seed=", seed_num), paste0("ntree=", ntree))
  
  # 3. dta.rds removed — RF object + data is large and not needed for analysis.
  #    All outputs needed by get_performance_summary.R are saved in
  #    prediction.rds, prediction_prob.rds, and gcm_log.rds.
  
  trueResult = test_data[[target_name]]
  RFResult = unname(pred_forest_level)
  pred_p = matrix(as.numeric(pred_tree_level), nrow = ntree) |> 
    apply(2, mean)
  
  return(list("trueResult" = trueResult,
              "RFResult" = RFResult,
              "pred_tree_level" = pred_tree_level,
              "pred_p" = pred_p,
              "rf" = rf))
}
