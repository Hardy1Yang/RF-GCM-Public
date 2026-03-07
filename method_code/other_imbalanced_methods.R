
# other_imbalanced_methods.R
# Purpose: Run step5 in `main.R`

# library(randomForest)
# library(ranger)
# library(RSBID)    # https://github.com/dongyuanwu/RSBID
source(file.path("method_code", "SMOTE_N.R"))

SMOTE_ratio = if (exists("SMOTE_RATIO")) SMOTE_RATIO else 100
SMOTE_k     = if (exists("SMOTE_K"))     SMOTE_K     else 5

get_SMOTE_pred = function(seed_num, tree_nm, train_data, test_data, target_nm){
  set.seed(seed_num)

  # check feature types: all numeric / all factor / mixed
  target_idx = which(target_nm == colnames(train_data))
  f_type = sapply(train_data[, -target_idx], class)   # Vector: classes of features
  
  if (all(f_type == "numeric" | f_type == "integer")){
    trainDta_SMOTE = SMOTE(as.data.frame(train_data), target_nm, SMOTE_ratio, SMOTE_k)
    
  } else if (all(f_type == "character" | f_type == "factor")){
    trainDta_SMOTE = SMOTE_N(train_data, target_nm, SMOTE_ratio, SMOTE_k)
    
  } else if ((("character" %in% f_type) | ("factor" %in% f_type)) & (("numeric" %in% f_type) | ("integer" %in% f_type))) {
    # integer 要換成 double
    for (int_idx in which(f_type == "integer")){
      train_data[, -target_idx][[int_idx]] = as.double(train_data[, -target_idx][[int_idx]])
    }
    trainDta_SMOTE = SMOTE_NC(as.data.frame(train_data), target_nm, SMOTE_ratio, SMOTE_k)
    # trainDta_SMOTE[[target_nm]] = as.factor(trainDta_SMOTE[[target_nm]])
    
  } else{
    print("檢查訓練集的 class, 出現非 char, fct, int, num")
  }
  
  rf_SMOTE = randomForest(as.formula(paste0(target_nm, "~.")),
                          ntree = tree_nm,
                          data = trainDta_SMOTE)
  pred_SMOTE = predict(rf_SMOTE, test_data)
  prob_SMOTE = predict(rf_SMOTE, test_data, type = "prob")[, "1"]

  return(list(pred = unname(pred_SMOTE), prob = prob_SMOTE))
}


get_BRF_pred = function(seed_num, tree_nm, train_data, test_data, target_nm){
  set.seed(seed_num)

  # Get the number of training samples in class 0 / class 1
  n_class0 = table(train_data[[target_nm]])[[1]]   # factor, "0"
  n_class1 = table(train_data[[target_nm]])[[2]]   # factor, "1

  min_n = min(n_class0, n_class1)
  rf_BRF = randomForest(as.formula(paste0(target_nm, "~.")),
                        ntree = tree_nm,
                        data = train_data,
                        sampsize = c(min_n, min_n))
  pred_BRF = predict(rf_BRF, test_data)
  prob_BRF = predict(rf_BRF, test_data, type = "prob")[, "1"]

  return(list(pred = unname(pred_BRF), prob = prob_BRF))
}


get_CSRF_pred = function(seed_num, tree_nm, train_data, test_data, target_nm){
  # Cost-Sensitive Random Forest using inverse class-frequency weights
  set.seed(seed_num)
  target_vec = train_data[[target_nm]]
  n0 = sum(target_vec == levels(target_vec)[1])
  n1 = sum(target_vec == levels(target_vec)[2])
  wt = c(1, n0 / n1)
  names(wt) = levels(target_vec)

  rf_CSRF = randomForest(as.formula(paste0(target_nm, "~.")),
                         data = train_data, ntree = tree_nm,
                         classwt = wt)
  pred_CSRF = predict(rf_CSRF, test_data)
  prob_CSRF = predict(rf_CSRF, test_data, type = "prob")[, "1"]
  return(list(pred = unname(pred_CSRF), prob = prob_CSRF))
}


get_EasyEnsemble_pred = function(seed_num, tree_nm, train_data, test_data,
                                  target_nm, n_subsets = if (exists("EASYENSEMBLE_NSUB")) EASYENSEMBLE_NSUB else 10){
  # EasyEnsemble: train T balanced sub-ensembles, aggregate by majority vote
  set.seed(seed_num)
  target_vec = train_data[[target_nm]]
  minority_class = names(which.min(table(target_vec)))
  majority_class = names(which.max(table(target_vec)))

  idx_min = which(target_vec == minority_class)
  idx_maj = which(target_vec == majority_class)
  n_min = length(idx_min)
  trees_per_subset = max(1, round(tree_nm / n_subsets))

  pred_matrix = matrix(NA, nrow = nrow(test_data), ncol = n_subsets)
  prob_matrix = matrix(NA, nrow = nrow(test_data), ncol = n_subsets)
  for (t in seq_len(n_subsets)){
    sampled_maj = sample(idx_maj, n_min, replace = (n_min > length(idx_maj)))
    balanced_data = train_data[c(idx_min, sampled_maj), ]
    rf_t = randomForest(as.formula(paste0(target_nm, "~.")),
                        data = balanced_data, ntree = trees_per_subset)
    pred_t = predict(rf_t, test_data)
    prob_t = predict(rf_t, test_data, type = "prob")[, "1"]
    pred_matrix[, t] = as.numeric(pred_t) - 1
    prob_matrix[, t] = prob_t
  }
  final_pred = factor(as.numeric(rowMeans(pred_matrix) > 0.5))
  final_prob = rowMeans(prob_matrix)
  return(list(pred = final_pred, prob = final_prob))
}








