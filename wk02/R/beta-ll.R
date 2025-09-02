# manually typing log-likelihood
ll_fn <- function(theta, y) { 
  alpha <- theta[1] 
  beta <- theta[2] 
  ll <- alpha*sum(log(y)) + beta*sum(log(1 - y)) - 
    length(y)*log(beta(alpha, beta)) 
  return(ll) 
}

# using dbeta shortcut (good!)
ll_fn <- function(theta, y) { 
  alpha <- theta[1] 
  beta <- theta[2] 
  ll <- sum(dbeta(y, shape1 = alpha, shape2 = beta, log = TRUE))
  return(ll)
}

# use optim()
est <- optim(par = c(2, 2), 
             fn = ll_fn, 
             y = y, 
             control = list(fnscale = -1), 
             method = "Nelder-Mead") 

# make function that fits beta model
est_beta <- function(y) {
  est <- optim(par = c(2, 2), fn = ll_fn, y = y,
               control = list(fnscale = -1),
               method = "BFGS") # for >1d problems
  if (est$convergence != 0) print("Model did not converge!")
  res <- list(est = est$par)
  return(res)
}

# fit beta model
ml_est <- est_beta(y)
print(ml_est, digits = 3)