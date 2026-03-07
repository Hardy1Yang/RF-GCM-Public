
# GCM_MCMC.R
# Purpose: Run step3 in `main.R`

GCM_save_data_estiamte = function(dta_pred_ind, prop, dir_name, ntree=ntree, nitem, fixed,
                                  nChains=3, nAdaptSteps = 1000, nBurninSteps =  500,
                                  nUseSteps = 10000, nThinSteps = 2, inits=NA){
  
  dataList = list(
    X = dta_pred_ind,
    n = dim(dta_pred_ind)[1],   # numbers of classifier
    m = dim(dta_pred_ind)[2]    # numbers of item
  )
  if (fixed == "DG2") dataList[["prop"]] = prop
  
  # init 1----
    inits1 <- list(D = rbeta(1, 2, 2), g = prop + runif(1, -0.05, 0.05), z = rbinom(nitem, 1, 0.5), 
                   Dmu = rbeta(1, 2, 2), Dtau = rgamma(1, 1, 0.1), gtau = rgamma(1, 1, 0.1), p = prop + runif(1, -0.05, 0.05),
                   .RNG.name="base::Wichmann-Hill", .RNG.state=c(10132L,16949L,26595L))
    inits2 <- list(D = rbeta(1, 2, 2), g = prop + runif(1, -0.05, 0.05), z = rbinom(nitem, 1, 0.5), 
                   Dmu = rbeta(1, 2, 2), Dtau = rgamma(1, 1, 0.1), gtau = rgamma(1, 1, 0.1), p = prop + runif(1, -0.05, 0.05),
                   .RNG.name="base::Marsaglia-Multicarry", .RNG.state= c(1269883289L,592129798L))
    inits3 <- list(D = rbeta(1, 2, 2), g = prop + runif(1, -0.05, 0.05), z = rbinom(nitem, 1, 0.5), 
                   Dmu = rbeta(1, 2, 2), Dtau = rgamma(1, 1, 0.1), gtau = rgamma(1, 1, 0.1), p = prop + runif(1, -0.05, 0.05),
                   .RNG.name="base::Super-Duper", .RNG.state = c(-1951678836L,-1949181411L))
    
    # Parameters are now passed from the caller (main.R) — no longer overridden here.

  
  if (! is.null(inits)){
    for (param in names(inits)){
      inits1[[param]] = inits[[param]][1]
      inits2[[param]] = inits[[param]][2]
      inits3[[param]] = inits[[param]][3]
    }
  }
  
  model_txt  = "model_fixedDG3.txt"
  print(model_txt)
  runJagsOut <- tryCatch({run.jags( method = "parallel" ,
                                    model = file.path("method_code", model_txt),
                                    monitor = c("z", "D", "g", "p"),
                                    inits = list(inits1, inits2, inits3),
                                    data = dataList,
                                    n.chains = nChains,
                                    adapt = nAdaptSteps,
                                    burnin = nBurninSteps,
                                    sample = ceiling(nUseSteps/nChains),
                                    thin = nThinSteps ,
                                    summarise = TRUE ,
                                    plots = TRUE )},
                         error = function(e){return(NULL)})
  
  
  # save data
  if (!is.null(runJagsOut)){
    saveRDS(runJagsOut, file = file.path(dir_name, "DGp_runJagsOut.rds"))
  }
  
  
  if (is.null(runJagsOut)){return (NULL)}
  return(estimate(as.mcmc.list(runJagsOut), 3, nitem, ntree, fixed))
}


estimate = function(codaSamples, chainNum, nitem, ntree, fixed){
  
  zHat = numeric(nitem)
  DHat = numeric(1)
  GHat = numeric(1)
  pHat = numeric(1)
  
  for (item in 1:nitem){
    zHat[item] = (sapply(codaSamples, function(x) x[,item]) |> as.vector() |> DescTools::Mode())[1]
  }
  DHat = sapply(codaSamples, function(x) x[,nitem+1]) |> mean() 
  GHat = sapply(codaSamples, function(x) x[,nitem+2]) |> mean()
  pHat = sapply(codaSamples, function(x) x[,nitem+3]) |> mean()
  
  
  return(list(zHat = zHat, DHat = DHat, GHat = GHat, pHat = pHat))
}

