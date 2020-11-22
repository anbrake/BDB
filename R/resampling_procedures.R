blb_sampling = function(X, FUN, T, b = .6*n, iter = 100, ...){
  R = c()
  n  = length(X[,1])
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

sdb_pal = function(X, FUN,T,subset_size, n, niter){
  foreach::foreach(i = 1:niter, .combine = 'c') %dopar% {
    index = sample(1:n, subset_size, replace = TRUE)
    resample.index = sample(1:subset_size, n, replace = TRUE)
    #Subsample and resample data
    X_j <- X[index,]
    X_j = cbind(X_j, c())
    X_jresample <- X_j[resample.index,]

    #Calculate the statistic of interest
    theta_j <- FUN(X_j)
    theta_resample <- FUN(X_jresample)

    #Calculate the root function
    T_iter <- T(theta_resample, theta_j)
  }
}


timefcn = function(X, statistic, T, subset_size){
  iters1 = SDBBoot(X, statistic, T, subset_size = subset_size, time_lim = 15)$iters
  iters2 = SDBBoot(X, statistic, T, subset_size = subset_size, time_lim = 15)$iters
  #2*iters*t(b) = time
  t.b = 15/(iters1+iters2)
  sd_iters = sd(iters1,iters2)
  return(t.b)
}



