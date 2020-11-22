############Bag of Little Bootstraps###############

BLBoot = function(dta, statistic, T, subsets, ..., b = n^.6, iter = 100){
  statistic <- match.fun(statistic)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  R = c()

  for(s in 1:subsets){
    index = sample(1:n, b, replace = FALSE)
    X_j = X[index,]
    X_j = cbind(X_j, c())
    theta_j = statistic(X_j, ...)
    for(i in 1:iter){
      resampleIndex = sample(1:b, n, replace = TRUE)
      X_j_i = X_j[resampleIndex,]
      theta_j_i = statistic(X_j_i, ...)
      T_i = T(theta_j_i, theta_j)
      R = c(R, T_i)
    }#close inner loop
  }#close outer loop
  #Calculate the actual statistic for the data
  theta_n = statistic(X, ...)
  #Find the standard error for the statistic of interest
  rtn = list(t0=theta_n, T = R, iter = length(R))
return(rtn)
}#end fun


#Parallel implementation of BLB_boot
BLB_par = function(dta, FUN, T, subsets, ..., b = n^.6, iter = 100, ncores = ncores){
  FUN <- match.fun(FUN)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])

  #Set up parallel environment
  #ncores = parallel::detectCores() - 2
  R = parallel::mclapply(1:subsets,
      function(i){blb_sampling(X = X, FUN = FUN, T=T,iter = iter, b = b, n=n)},
      mc.cores = ncores)
return(list(T = unlist(R), iters = length(R)))
}
