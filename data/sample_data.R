# Generate sample data for WaveQTEX package
# This script creates example datasets for demonstration purposes

library(xts)
library(quantmod)

set.seed(12345)

# Generate sample stock returns data
n_obs <- 1000
dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n_obs)

# Market parameters for realistic simulation
market_params <- data.frame(
  market = c("SP500", "Nikkei", "DAX", "FTSE", "CAC", "SSE", "BSE", "MOEX", "Bovespa", "IPC"),
  mu = c(0.0003, 0.0002, 0.0003, 0.0002, 0.0002, 0.0001, 0.0004, 0.0001, 0.0002, 0.0003),
  sigma = c(0.012, 0.015, 0.014, 0.011, 0.013, 0.018, 0.016, 0.020, 0.019, 0.017),
  stringsAsFactors = FALSE
)

# Generate correlated returns using factor model
n_markets <- nrow(market_params)

# Common factor (global market factor)
global_factor <- rnorm(n_obs, 0, 0.01)

# Regional factors
regional_factors <- data.frame(
  Americas = rnorm(n_obs, 0, 0.008),
  Europe = rnorm(n_obs, 0, 0.007),
  Asia = rnorm(n_obs, 0, 0.009)
)

# Market-specific noise
idiosyncratic <- matrix(rnorm(n_obs * n_markets, 0, 0.01), nrow = n_obs, ncol = n_markets)

# Factor loadings
factor_loadings <- data.frame(
  market = market_params$market,
  global_beta = c(0.8, 0.7, 0.9, 0.8, 0.9, 0.6, 0.5, 0.4, 0.6, 0.7),
  regional_beta = c(0.6, 0.5, 0.7, 0.6, 0.6, 0.4, 0.3, 0.5, 0.5, 0.6),
  region = c("Americas", "Asia", "Europe", "Europe", "Europe", "Asia", "Asia", "Europe", "Americas", "Americas"),
  stringsAsFactors = FALSE
)

# Generate returns
returns_matrix <- matrix(0, nrow = n_obs, ncol = n_markets)
colnames(returns_matrix) <- market_params$market

for (i in 1:n_markets) {
  market <- market_params$market[i]
  mu <- market_params$mu[i]
  sigma <- market_params$sigma[i]
  
  global_beta <- factor_loadings$global_beta[i]
  regional_beta <- factor_loadings$regional_beta[i]
  region <- factor_loadings$region[i]
  
  regional_factor <- regional_factors[[region]]
  
  returns_matrix[, i] <- mu + 
                        global_beta * global_factor +
                        regional_beta * regional_factor +
                        sqrt(1 - global_beta^2 - regional_beta^2) * idiosyncratic[, i]
  
  # Apply market-specific volatility scaling
  returns_matrix[, i] <- returns_matrix[, i] * (sigma / sd(returns_matrix[, i]))
}

# Add some crisis periods with increased volatility and correlation
crisis_periods <- c(100:150, 400:430, 750:780)  # Three crisis periods
for (period in crisis_periods) {
  if (period <= n_obs) {
    # Increase volatility
    returns_matrix[period, ] <- returns_matrix[period, ] * 2.5
    
    # Add common shock
    common_shock <- rnorm(1, -0.02, 0.01)
    returns_matrix[period, ] <- returns_matrix[period, ] + common_shock
  }
}

# Create final dataset
sample_stock_returns <- data.frame(
  date = dates,
  returns_matrix,
  stringsAsFactors = FALSE
)

# Generate crisis indicators
crisis_indicator <- rep(0, n_obs)
for (period in crisis_periods) {
  if (period <= n_obs) {
    crisis_indicator[max(1, period-10):min(n_obs, period+10)] <- 1
  }
}

crisis_indicators <- data.frame(
  date = dates,
  crisis_indicator = crisis_indicator,
  stringsAsFactors = FALSE
)

# Market classifications
market_classifications <- data.frame(
  market = c("SP500", "Nikkei", "DAX", "FTSE", "CAC", "SSE", "BSE", "MOEX", "Bovespa", "IPC"),
  country = c("USA", "Japan", "Germany", "UK", "France", "China", "India", "Russia", "Brazil", "Mexico"),
  region = c("North_America", "Asia", "Europe", "Europe", "Europe", "Asia", "Asia", "Europe", "South_America", "North_America"),
  development_level = c("Developed", "Developed", "Developed", "Developed", "Developed", 
                       "Emerging", "Emerging", "Emerging", "Emerging", "Emerging"),
  stringsAsFactors = FALSE
)

# Save datasets
save(sample_stock_returns, file = "sample_stock_returns.rda")
save(crisis_indicators, file = "crisis_indicators.rda")
save(market_classifications, file = "market_classifications.rda")

cat("Sample datasets created successfully:\n")
cat("- sample_stock_returns: ", nrow(sample_stock_returns), " observations, ", ncol(sample_stock_returns)-1, " markets\n")
cat("- crisis_indicators: ", sum(crisis_indicators$crisis_indicator), " crisis periods\n")
cat("- market_classifications: ", nrow(market_classifications), " markets classified\n")