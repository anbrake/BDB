############Bag of Little Bootstraps###############

BLBoot = function(dta, FUN, T, subsets, ..., b = n^.6, iter = 100){
  FUN <- match.fun(FUN)
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
  rtn = list(t0=theta_n, T = R, subsets = iter)
return(rtn)
}#end fun
