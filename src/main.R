###########################################################
############### Load and process data #####################
###########################################################

rm(list = ls())

library(PerformanceAnalytics)
library(GAS)
library(here)

source(here('src','core_functions.R'))
data_dir <- here::here('data')
plots_dir <- here::here('plots')
backtest_dir <- here::here('backtest')

# Load data 
load(here(data_dir,'indices.rda'))

# Consider only index values since January 2005
prices <- prices['2005-01-01/']

# Compute the log-returns for both series
logret <-  CalculateReturns(prices, 'log')[-1]

# Plot the logreturns
png(file = here('figures', "cumsum_logret.png"))
par(mfrow = c(2, 1))
plot(cumsum(logret[, 1]), 
     col = 'orange', 
     xlab = 'Time', 
     ylab = 'Cumsum Log-returns',
     main = 'S&P500')
plot(cumsum(logret[, 2]),
     col = 'blue', 
     xlab = 'Time', 
     ylab = 'Cumsum Log-returns',
     main = 'FTSE100')
dev.off()


##########################################################
######### Static estimation of the 95% VaR ###############
##########################################################

T = 1000
level = 0.95

## GARCH
forecast_SP500 <- f_forecast_var(as.numeric(logret[1:T, 1]), level)
forecast_FTSE <- f_forecast_var(as.numeric(logret[1:T, 2]), level)

# VaR forecast at T+1 
VaR_SP500 <- forecast_SP500$VaR_Forecast
VaR_FTSE100 <- forecast_FTSE$VaR_Forecast

# MLE GARCH parameters
GARCH_param_SP500 <- forecast_SP500$GARCH_param
GARCH_param_FTSE <- forecast_FTSE$GARCH_param

# Create a data frame with Omega, Alpha, and Beta 
GARCH_params_table <- data.frame(
  Index = c("SP500", "FTSE"),
  Omega = format(round(c(GARCH_param_SP500[1], GARCH_param_FTSE[1]), 10), nsmall = 10, scientific = TRUE),
  Alpha = format(round(c(GARCH_param_SP500[2], GARCH_param_FTSE[2]), 3), nsmall = 3, scientific = FALSE),
  Beta  = format(round(c(GARCH_param_SP500[3], GARCH_param_FTSE[3]), 3), nsmall = 3, scientific = FALSE)
)

GARCH_params_table

# Create a data frame with T+1 forecasts 
VaR_forecast <- data.frame(
  Index = c("VaR_forecast"),
  SP500 = VaR_SP500,
  FTSE = VaR_FTSE100
)

VaR_forecast

########################################################
#################### Backtesting #######################
########################################################

roll_garch_SP500 <- f_roll_forecast_var(y = logret[1:(2*T), 1], 
                         level = level, window_size = T, n_forecast = T)

roll_garch_FTSE100 <- f_roll_forecast_var(y = logret[1:(2*T), 2], 
                         level = level, window_size = T, n_forecast = T)


# Create df to plot
df <- xts(
  data.frame(logret[(T + 1):(2*T), ],
              var_SP500 = roll_garch_SP500$roll_VaR_Forecast, 
              var_FTSE100 = roll_garch_FTSE100$roll_VaR_Forecast,
              cvar_SP500 = roll_garch_SP500$roll_ConditionalVariances,
              cvar_FTSE100 = roll_garch_FTSE100$roll_ConditionalVariances),
  order.by = index(logret)[(T + 1):(2*T)])



# Plot GARCH VaR series for both indices and save 
png(file = here('figures', "Static_GARCH_VaR_forecast.png"), width = 800, height = 400)
par(mfrow = c(1, 1))
matplot(df[, 3:4], 
        col = c("orange", "blue"), 
        type = "l",  
        lty = 1, 
        lwd = 2,
        xlab = 'Time', 
        ylab = 'VaR', 
        main = 'VaR S&P500 & FTSE100')
grid(col = "gray", lty = "dotted", lwd = 1)
legend("bottomright", 
       legend = c("S&P 500", "FTSE 100"), 
       col = c("orange", "blue"), 
       lty = 1, 
       cex = 0.8,
       lwd = 2) 
dev.off()



# VaR and returns for the S&P500 in the same plot
png(file = here('figures', "Rolling_windows_VaR_SP500.png"), width = 800, height = 400)
plot(index(df),
     as.numeric(df$SP500), 
     type = 'l', 
     col = 'blue', 
     xlab = 'Time', 
     ylab = 'Returns', 
     main = 'S&P500 - Rolling Window VaR & Returns')
lines(index(df), 
      as.numeric(df$var_SP500), 
      col = 'red',
      lwd = 2)
grid(col = "gray", lty = "dotted", lwd = 1)
legend("topright", 
       legend = c("Returns", "Predicted VaR"),
       col = c("blue", "red"), 
       lty = 1:1, cex = 0.8, lwd = 2)
dev.off()


# VaR and returns for the FTSE100 in the same plot 
png(file = here('figures', "Rolling_windows_VaR_FTSE100.png"), width = 800, height = 400)
plot(index(df), 
     as.numeric(df$FTSE100), 
     type = 'l', 
     col = 'blue', 
     xlab = 'Time', 
     ylab = 'Returns', 
     main = 'FTSE100 - Rolling Window VaR & Returns')
lines(index(df), 
      as.numeric(df$var_FTSE100), 
      col = 'red',
      lwd = 2)
grid(col = "gray", lty = "dotted", lwd = 1)
legend("topright", 
       legend = c("Returns", "Predicted sVaR"),
       col = c("blue", "red"), 
       lty = 1:1, cex = 0.8, lwd = 2)
dev.off()



# Coverage ratio

violations_SP500 <- sum(df$SP500 < df$var_SP500)
violations_FTSE100 <- sum(df$FTSE100 < df$var_FTSE100)


coverage_df <- data.frame(
  Index = "Coverage",
  SP500 =  violations_SP500 / ((1 - level) * length(df$var_SP500)),
  FTSE100 =  violations_FTSE100 / ((1 - level)*length(df$var_FTSE100))
)

coverage_df


########################################################
############ BONUS - Kupiec and  Christoffersen ########
########################################################


sum(df$SP500 < df$var_SP500)
sum(df$FTSE100 < df$var_FTSE100)


# GAS package: backtest and formal tests

GAS_backtest_SP500 <- GAS::BacktestVaR(data = as.numeric(df$SP500),
                                        VaR = as.numeric(df$var_SP500),
                                         alpha = .05)

GAS_backtest_FTSE100 <- GAS::BacktestVaR(data = as.numeric(df$FTSE100),
                                       VaR = as.numeric(df$var_FTSE100),
                                        alpha = .05)



# GAS results

GAS_test_results <- data.frame(
  Index = rep(c("SP500", "FTSE100"), each = 2),
  Test = rep(c("LRuc", "LRcc"), times = 2),
  Test_Value = c(GAS_backtest_SP500$LRuc["Test"], GAS_backtest_SP500$LRcc["Test"], 
                 GAS_backtest_FTSE100$LRuc["Test"], GAS_backtest_FTSE100$LRcc["Test"]),
  P_Value = c(GAS_backtest_SP500$LRuc["Pvalue"], GAS_backtest_SP500$LRcc["Pvalue"], 
              GAS_backtest_FTSE100$LRuc["Pvalue"], GAS_backtest_FTSE100$LRcc["Pvalue"])
)

GAS_test_results



# Save the backtest results
save(df, 
     GAS_backtest_SP500, 
     GAS_backtest_FTSE100,
     file = here('backtest', "backtest_VaR_GARCH.rda"))

#load(file = here('backtest', "backtest_VaR_GARCH.rda"))









