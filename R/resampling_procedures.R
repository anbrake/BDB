
blb_sampling_outer = function(){}

blb_sampling_inner = function(){}




sdb_sampling = function(X, FUN, T, ...){
  index = sample(1:n, subset_size, replace = TRUE)
  resample.index = sample(1:subset_size, n, replace = TRUE)

  #Subsample and resample data
  X_j <- X[index,]
  X_jresample <- sample(X_j, n, replace = TRUE)

  #Calculate the statistic of interest
  theta_j <- FUN(X_j, ...)
  theta_resample <- FUN(X_jresample, ...)

  #Calculate the root function
  T_iter <- T.star(theta_resample, theta_j)
  return(T_iter)
}










