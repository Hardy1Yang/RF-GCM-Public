

# threshold.R
# Purpose: Run step4 in `main.R`


get_performance <- function(pred, ans) {
  
  # 確保 pred 和 ans 是數值型向量
  if (!is.numeric(pred) || !is.numeric(ans)) {
    stop("pred 和 ans 必須是數值型向量")
  }
  if (length(pred) != length(ans)) {
    stop("pred 和 ans 的長度必須相同")
  }
  
  # G-Mean
  Sensitivity = ifelse(sum(ans == 1) > 0, sum(pred == 1 & ans == 1) / sum(ans == 1), 0)  # 避免除零
  Specificity = ifelse(sum(ans == 0) > 0, sum(pred == 0 & ans == 0) / sum(ans == 0), 0)  # 避免除零
  GM = sqrt(Sensitivity * Specificity)  
  
  # Precision (精確率) 和 Recall (召回率)
  Precision = ifelse(sum(pred == 1) > 0, sum(pred == 1 & ans == 1) / sum(pred == 1), 0)  # 避免除零
  Recall = Sensitivity  
  FM = ifelse((Precision + Recall) > 0, 2*(Precision * Recall)/(Precision + Recall), 0)  # 若分母為 0，則 FM 設為 0
  
  
  return(list(TPR = Sensitivity, TNR = Specificity, GM = GM, FM = FM))
}



find_optimal_threshold <- function(pred, ans, performance) {
  
  # performance : "GM" 或 "FM"
  # pred 為預測為 class1 的機率 (數值型向量)
  # ans 為正確答案 (0, 1 向量)
  
  max_performance <- 0
  optimal_threshold <- 0
  
  for (threshold in seq(0, 1, 0.01)) {
    performance_result <- get_performance(as.numeric(pred > threshold), ans)
    current_performance <- performance_result[[performance]]
    
    if (!is.na(current_performance) && (current_performance > max_performance)) {
      max_performance <- current_performance
      optimal_threshold <- threshold
    }
  }
  return(optimal_threshold)
}


get_opt_pred = function(rf, train_data, target_nm, pred_p){
  
  # training data 中 OOB 被預測為 1 的機率
  p = rf[["votes"]] |> 
    apply(1, function(x)x[2]/(x[1]+x[2])) |> unname()
  
  # training data 真實 class
  train_label = as.numeric(as.character(train_data[[target_nm]]))
  
  opt_gm = find_optimal_threshold(p, train_label, "GM")
  opt_fm = find_optimal_threshold(p, train_label, "FM")
  
  return(list("GM" = factor(as.numeric(pred_p > opt_gm)),
              "FM" = factor(as.numeric(pred_p > opt_fm))))
}





