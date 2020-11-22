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
    X_j = cbind(X_j, c())
    X_jresample <- X_j[resample.index,]

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
  rtn = list(t0=theta_n, T = R, iter = length(R), total.time = time)
  return(rtn)
} #end

###################Parallel Implementation#####
SDB_ParallelNaive = function(dta, statistic,T,subset_size,..., niter,time_lim=300){
  FUN <- match.fun(statistic)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  start = proc.time()[3]
  R = c()
  time = 0

  #Set up parallel environ
  ncores = parallel::detectCores()
  cl = parallel::makeCluster(ncores-2)
  doParallel::registerDoParallel(cl)
  while(time < time_lim){
    T_iters = sdb_pal(X, FUN, T, subset_size, n=n,niter = niter)
    R = c(R, T_iters)
    end = proc.time()[3]
    time = end - start
  }
  on.exit(expr=parallel::stopCluster(cl))
  #Calculate the actual statistic for the data
  theta_n = FUN(X, ...)
  #Find the standard error for the statistic of interest
  tot.time = proc.time()[3] - start
  rtn = list(t0=theta_n, T = R, iter = length(R), total.time = tot.time)
  return(rtn)
}

SDB_ParallelTimeSolve = function(dta, statistic,T,subset_size,..., c,time_lim=300){
  FUN <- match.fun(statistic)
  T <- match.fun(T)
  X = cbind(dta,c())
  n = length(X[,1])
  start = proc.time()[3]

  #Number of iterations
  t.b = timefcn(X, statistic, T, subset_size)
  #solve for number of iterations
  iters = time_lim / (2*t.b)
  R = sdb_pal(X, FUN,T,subset_size, n, niter = iters)

  end = proc.time()[3]
  tot.time = end-start
  rtn = list(t0=theta_n,T=R, iter = iters, total.time = tot.time)
  return(rtn)
}




