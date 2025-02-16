
f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB <- c(1e-6, 1e-6, 1e-6)       

  # Stationarity conditions
  
  # Inequality constraints
  A      <- matrix(c(0, -1, -1), nrow = 1) # Coefficients for alpha and beta 
  b      <- -1  # Right-hand side 
  
  # Optimization
  res <- constrOptim(theta0,
                     f  = f_nll,
                     grad = NULL,
                     ui = matrix(c(1,0,0, 0,1,0 , 0,0,1, A), 
                                 nrow = 4, 
                                 ncol = 3, 
                                 byrow = TRUE),
                     ci = matrix(c(LB, b), nrow = 4, ncol = 1, byrow = TRUE),
                     y  = y)
  
  # Extract the estimated parameters
  theta <- res$par
  

  # Recompute conditional variance 
  sig2 <- f_ht(theta, y)
  
  # Compute the next-day ahead VaR for the Normal model
  z_alpha <- qnorm(level)
  T <- length(y)
  VaR <- - z_alpha * sqrt(sig2[T+1])
  
  out <- list(
    VaR_Forecast         = VaR, 
    ConditionalVariances = sig2,
    GARCH_param          = theta
  )
  
  return(out)
}


f_nll <- function(theta, y) {
  ### Function which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- f_ht(theta, y)
  
  # Drop the last forecast since we only have T observations
  sig2 <- sig2[1:T]
  
  # Log-likelihood 
  ll <- sum(dnorm(x = y, 
                  mean = 0, 
                  sd = sqrt(sig2), 
                  log = TRUE))
  
  # Output the negative value
  nll <- -ll
  return(nll)
}


f_ht <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]  # omega
  a1 <- theta[2]  # alpha
  b1 <- theta[3]  # beta
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start at unconditional variance
  # If a1 + b1 < 1, the unconditional variance = a0 / (1 - a1 - b1)
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # GARCH recursion
  for (t in 2:(T+1)) {
    sig2[t] <- a0 + a1 * y[t-1]^2 + b1 * sig2[t-1]
  }
  
  return(sig2)
}



f_roll_forecast_var <- function(y, level, window_size, n_forecast) {
  ### Function to get the rolling forecast of the VaR
  # INPUTS
  #   y     : [vector] (T x 1) log-returns
  #   level : [scalar] (1 x 1) confidence level
  #   window_size : [scalar] (1 x 1) window size
  #   n_forecast : [scalar] (1 x 1) number of forecasts
  # OUTPUTS
  #   forecast : [list] with the VaR forecast and the conditional variances
  
  y <- as.numeric(y)
  var_forecast <- rep(NA, n_forecast)
  cvar_forecast <- rep(NA, n_forecast)
  
  for (t in 1:(2*n_forecast - window_size) ) {
      forecast <- f_forecast_var(y = y[t:(t + window_size - 1)], level = level)
      var_forecast[t] <-  forecast$VaR_Forecast
      cvar_forecast[t] <- tail(forecast$ConditionalVariances, 1)
    }
    
  out <- list(roll_VaR_Forecast = var_forecast, 
              roll_ConditionalVariances = cvar_forecast)
  
  return(out)
}
















