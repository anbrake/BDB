############Subsampled Double Bootstrap################
#' Subsampled Double Bootstrap
#' @description Given the data, some function of the data, and some error function,
#' runs the Subsampled Double bootstrap algorithm.
#' @param dta The data you want to bootstrap.
#' @param statistic The statistic of interest (theta_hat).
#' @param T The "root" or error function of interest.
#' Note that the first argument should be the theta of the sample, and the senond arguent should be the
#' "true" theta.
#' @param subset_size How large should the initial subset be.
#' @param ... Extra parameters to pass into T.star or FUN.
#' @param time_lim How long should the function run. Default is 5 minutes.
#'
#' @return List of the theta_hat of dta, a vector containing all the T.star
#' and the number of iterations completed in the time limit
#' @export
#'
#' @examples
SDBBoot = function(dta, statistic,T,subset_size,..., time_lim=300){
  FUN <- match.fun(statistic)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  start = proc.time()[1]
  time = 0
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

    #Check time
    end = proc.time()[1]
    time = end-start
  }#endloop
  #Calculate the actual statistic for the data
  theta_n = FUN(X, ...)
  #Find the standard error for the statistic of interest
  rtn = list(t0=theta_n, T = R, iter = length(R))
  return(rtn)
} #end


SDB_par = function(dta, statistic,T,subset_size,..., time_lim=300){
  FUN <- match.fun(statistic)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  start = proc.time()[1]
  time = 0

  #Set up parallel environment
  ncores = parallel::detectCores() - 2
  R = parallel::mclapply(1:ncores,
      function(i){sdb_resampling(X, FUN, T, subset_size = subset_size,..., time.limPC = time_lim / ncores)},
      mc.cores = ncores)
}






