############Bag of Little Bootstraps###############

BLBoot = function(dta, statistic, T, subsets, ..., b = n^.6, iter = 100){
  FUN <- match.fun(statistic)
  T.star <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  R = c()

  for(s in 1:subsets){
    index = sample(1:n, b, replace = FALSE)
    X_j = X[index,]
    theta_j = FUN(X_j, ...)
    for(i in 1:iter){
      X_j_i = sample(X_j, n, replace = TRUE)
      theta_j_i = FUN(X_j_i, ...)
      T_i = T(theta_j_i, theta_j)
      R = c(R, T_i)
    }#close inner loop
  }#close outer loop
  #Calculate the actual statistic for the data
  theta_n = FUN(X, ...)
  #Find the standard error for the statistic of interest
  rtn = list(t0=theta_n, T = R, iter = length(R))
return(rtn)
}#end fun


#Parallel implemntation of BLB_boot
BLB_par = function(dta, statistic, T, subsets, ..., b = n^.6, iter = 100, ncores = ncores){
  FUN <- match.fun(statistic)
  T.star <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])

  #Set up parallel environment
  #ncores = parallel::detectCores() - 2
  R = parallel::mclapply(1:subsets,
            function(i){blb_sampling(X = X, statistic = FUN, T=T.star,
            iter = iter, b = b)},
            mc.cores = ncores)
return(R)
}
