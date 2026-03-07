
# SMOTE_N.R
# Purpose: Run get_SMOTE_pred in `other_imbalanced_methods.R`


# SMOTE-N
# Distance metric: VDM. First, calculate the distances between different values within each feature
# (for each attribute), and store the results in a data frame with 4 columns:
# feature_name, value1, value2, distance.
# For any two data points, compute the distance by looking up the values in the above data frame.
# SMOTE-N
# 距離: VDM, 先計算 feature 內不同 values 的距離(對每個屬性做)，儲存成 df(4col, feature_nm, value1, value2, dist)
# 任兩資料點算距離，用上面做出的 df 查找


VDM_F = function(df, target){
  # df 是完整的, target 是類別那個欄位名稱
  target_idx = which(target == colnames(df))
  res = c()
  
  for (col in (1:ncol(df))[-target_idx]){
    x = table(df[[col]], df[[target_idx]]) |> 
      prop.table(1) 
    
    n_value = length(rownames(x))    # 該 feature 有多少種 vlaue
    for (i in 1:(n_value-1)){
      for (j in (i+1):(n_value)){
        d = abs(x[i,1]-x[j,1]) + abs(x[i,2]-x[j,2])
        res = rbind(res, 
                    c(colnames(df)[col], rownames(x)[i], rownames(x)[j], d))
        res = rbind(res, 
                    c(colnames(df)[col], rownames(x)[j], rownames(x)[i], d))
      }
    }
  }
  
  res = data.frame(res)
  colnames(res) = c("feature", "V1", "V2", "distance")
  res$distance = as.numeric(res$distance)
  return(res)
}


VDM = function(res, instance1, instance2){
  
  # 回傳兩資料點距離; res is VDM_F 的回傳值;
  # instance 要去掉 target col, 是 named vector
  
  instances = rbind(instance1, instance2)
  dist = instances |> 
    apply(2, function(a, colnm){
      if (a[1] == a[2]){0} else{
        res[((res$feature == colnm) & (res$V1 == a[1]) & (res$V2 == a[2])), "distance"]^2
      }
    }, colnm=names(instances)) |> sum()
  
  return(dist)
}



SMOTE_N = function(df, target, ratio=100, k=5){
  
  # 回傳 res, 為舊的加上新樣本的資料集; df 為訓練集; target 為類別名稱字串
  # 類別 factor 的 levels 要是 c(0,1)
  
  res = c()
  # 要合成的數量; 合成時要做幾輪(以min_n 為標準)
  syn_num = (max(table(df[[target]]))*ratio/100) - min(table(df[[target]]))
  roundd = syn_num %/% min(table(df[[target]]))
  restt = syn_num %% min(table(df[[target]]))
  
  # 先找出少數類樣本 index (為在df的第幾 row); 因為合成新樣本只考慮少數類別資料點
  target_idx = which(target == colnames(df))
  if (mean(as.numeric(df[[target_idx]])) > 1.5) {
    min_idx = which(df[[target_idx]]=="0")
  } else {
    min_idx = which(df[[target_idx]]=="1")
  }
  
  # 做 feature 內 value 距離的 data.frame
  feature_dist = VDM_F(df, target)
  print("feature_dist done")
  
  # 先跑完整的 round
  for (i in min_idx){
    
    # 幫 i 做距離排序，得距離近到遠的 index sequence (index 是在 min_idx 的 idx)
    ranked_min_idx = apply(df[min_idx, -target_idx], 1, function(a){
      VDM(feature_dist, df[i, -target_idx], a)
    }) |> rank(ties.method = "random") |> order()
    
    for (r in 1:roundd){
      # 從 k 個中挑一個, 合成一個新的
      nn = min_idx[ranked_min_idx[2:(k+1)]] |> sample(1)
      new = df[c(i, nn),] |> apply(2, function(a){sample(a, 1)})
      res = rbind(res, new)
    }
  }
  print("round done")
  
  # 再跑 restt 部分
  min_idx_rest = sample(min_idx, restt, F)
  for (i in min_idx_rest){
    
    ranked_min_idx = apply(df[min_idx, -target_idx], 1, function(a){
      VDM(feature_dist, df[i, -target_idx], a)
    }) |> rank(ties.method = "random") |> order()
    nn = min_idx[ranked_min_idx[2:(k+1)]] |> sample(1)
    new = df[c(i, nn),] |> apply(2, function(a){sample(a, 1)})
    res = rbind(res, new)
  }
  print("rest done")
  
  res = rbind(data.frame(res), df)
  res = data.frame(lapply(res, factor))
  return(res)
}



