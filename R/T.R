MSE = function(theta_hat, theta){(theta_hat-theta)^2}
VAR = function(theta_hat, theta){(theta_hat-theta)^2}
CI = function(theta_hat, alpha){quantile(theta_hat, c(1-(alpha/2), alpha/2))}

