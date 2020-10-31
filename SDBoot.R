############Subsampled Double Bootstrap################
SDBoot = function(dta, FUN,T,subset_size,..., time_lim=300){
  FUN <- match.fun(FUN)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  start = proc.time()[1]
  time = 0
  iter = 0
  R = c()
  while(time < time_lim){
    #Get indexes
    index = sample(1:n, subset_size, replace = TRUE)
    resample.index = sample(1:subset_size, n, replace = TRUE)

    #Subsample and resample data
    X_j <- X[index,]
    X_jresample <- sample(X_j, n, replace = TRUE)

    #Calculate the statistic of interest
    theta_j <- FUN(X_j, ...)
    theta_resample <- FUN(X_jresample, ...)

    #Calculate the root function
    T_iter <- T(theta_resample, theta_j)
    R = c(R, T_iter)

    #add 1 to iteration and check time
    iter = iter + 1
    end = proc.time()[1]
    time = end-start
  }#endloop
  #Calculate the actual statistic for the data
  theta_n = FUN(X, ...)
  #Find the standard error for the statistic of interest
  rtn = list(t0=theta_n, T = R, iter = iter)
  return(rtn)
} #end
