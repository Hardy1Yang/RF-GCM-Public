
# GCM_observeProp.R
# Purpose: Run step3 in `main.R`


find_gcm_threshold = function(pi, param1, param2, N, type){
  if (type == "Dg"){
    H = param1 + (1-param1)*param2
    FA = (1-param1)*param2
  } else if (type =="HF"){
    H = param1
    FA = param2
  } else {stop("type 請輸入 Dg 或 HF 一種")}
  
  threshold = ((log((1-pi)/pi)/(N*log(H*(1-FA)/FA/(1-H)))) +
                 (log((1-FA)/(1-H))/log(H*(1-FA)/FA/(1-H))))
  return(threshold)
}


estimate_H_FA = function(train_data, target_name, rf){
  # 觀察訓練集 OOB 資料，用觀察比例估計H, FA
  # 先計算每棵樹的，再用每棵樹有的 oob 數量加權
  
  class0 = (as.numeric(as.character(train_data[[target_name]])) == 0)
  p0 = rf[["err.rate"]][,2]   # ans=0, res=1, FA
  p1 = 1 - rf[["err.rate"]][,3]   # ans=1, res=1, Hit
  
  # tree i 的 FA, H 用多少資料估計
  n0 = rf[["inbag"]][class0, ] |>     # 類別 0 有的數量
    apply(2, function(x){mean(x==0)})
  n1 = rf[["inbag"]][-class0, ] |>    # 類別 1 有的數量
    apply(2, function(x){mean(x==0)})
  
  FA = sum(n0 * p0)/sum(n0)
  H = sum((n1 * p1)[!is.na(n1 * p1)]) / sum(n1)
  
  return(c(H=H, FA=FA))
}


get_pred_tree_level_oob = function(rf, train_data){
  # 取得用 RF 預測訓練集 OOB 的結果
  x = predict(rf, train_data, predict.all = T)[[2]]
  x[rf[["inbag"]] > 0] <- NA    # 如果被拿去 train 就 NA，剩下 OOB 的
  pred_tree_level_oob = t(x)         # tree*item

  return(pred_tree_level_oob)
}


estimate_H_FA_validation = function(test_data, target_name, rf){
  # Estimate H (hit rate) and FA (false alarm rate) using validation/test data
  # instead of OOB samples. Used for OOB vs validation-set sensitivity analysis.
  pred_all = predict(rf, test_data, predict.all = TRUE)$individual
  true_labels = as.numeric(test_data[[target_name]]) - 1

  ntree = ncol(pred_all)
  H_vec = numeric(ntree)
  FA_vec = numeric(ntree)

  pos_idx = which(true_labels == 1)
  neg_idx = which(true_labels == 0)

  for (k in seq_len(ntree)){
    tree_pred = as.numeric(pred_all[, k]) - 1
    H_vec[k] = ifelse(length(pos_idx) > 0, mean(tree_pred[pos_idx]), NA)
    FA_vec[k] = ifelse(length(neg_idx) > 0, mean(tree_pred[neg_idx]), NA)
  }
  return(list(H = mean(H_vec, na.rm = TRUE), FA = mean(FA_vec, na.rm = TRUE)))
}






