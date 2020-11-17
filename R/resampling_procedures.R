blb_sampling = function(X, FUN, T, b = .6*n, iter = 100, ...){
  R = c()
  index = sample(1:n, b, replace = FALSE)
  X_j = X[index,]
  theta_j = FUN(X_j, ...)
  for(i in 1:iter){
    X_j_i = sample(X_j, n, replace = TRUE)
    theta_j_i = FUN(X_j_i, ...)
    T_i = T(theta_j_i, theta_j)
    R = c(R, T_i)
  }#end loop
  return(R)
}


sdb_resampling = function(X, FUN, T, subset_size, ..., time_limPC = time_lim / ncores){
  R = c()
  start = proc.time()[1]
  time = 0
  while(time < time_limPC){
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

    #Check time
    end = proc.time()[1]
    time = end-start
  }#endloop
  return(R)
}











