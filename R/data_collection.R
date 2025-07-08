# Data Collection and Processing Module
# Comprehensive market data collection for WaveQTE Network Economics

#' Collect Global Market Data
#' 
#' Collects stock returns and macroeconomic data from major developed 
#' and emerging markets for WaveQTE analysis
#' 
#' @param start_date Start date for data collection
#' @param end_date End date for data collection
#' @param frequency Data frequency ("daily", "weekly", "monthly")
#' @param include_emerging Include emerging markets data
#' @param include_macro Include macroeconomic variables
#' @return Comprehensive market dataset
#' @export
collect_global_market_data <- function(start_date = "2000-01-01", 
                                      end_date = Sys.Date(),
                                      frequency = "daily",
                                      include_emerging = TRUE,
                                      include_macro = TRUE) {
  
  cat("Collecting global market data from", start_date, "to", end_date, "\n")
  
  # Define market indices
  developed_markets <- list(
    "US_SP500" = "^GSPC",
    "US_NASDAQ" = "^IXIC", 
    "US_RUSSELL2000" = "^RUT",
    "EU_STOXX600" = "^STOXX",
    "DE_DAX" = "^GDAXI",
    "FR_CAC40" = "^FCHI",
    "UK_FTSE100" = "^FTSE",
    "JP_NIKKEI" = "^N225",
    "JP_TOPIX" = "^TPX",
    "AU_ASX200" = "^AXJO",
    "CA_TSX" = "^GSPTSE",
    "CH_SMI" = "^SSMI"
  )
  
  emerging_markets <- list(
    "CN_SHANGHAI" = "000001.SS",
    "CN_CSI300" = "000300.SS",
    "HK_HANGSENG" = "^HSI",
    "IN_SENSEX" = "^BSESN",
    "IN_NIFTY" = "^NSEI",
    "BR_BOVESPA" = "^BVSP",
    "ZA_JSE" = "^JALSH",
    "RU_MOEX" = "^MOEX",
    "KR_KOSPI" = "^KS11",
    "TW_TAIEX" = "^TWII",
    "MX_IPC" = "^MXX",
    "TR_BIST100" = "^XU100"
  )
  
  # Combine market lists
  all_markets <- developed_markets
  if (include_emerging) {
    all_markets <- c(all_markets, emerging_markets)
  }
  
  # Collect market data
  market_data <- collect_market_indices(all_markets, start_date, end_date, frequency)
  
  # Collect macroeconomic data
  macro_data <- NULL
  if (include_macro) {
    macro_data <- collect_macroeconomic_data(start_date, end_date, frequency)
  }
  
  # Create comprehensive dataset
  dataset <- list(
    market_returns = market_data$returns,
    market_prices = market_data$prices,
    market_volumes = market_data$volumes,
    macro_data = macro_data,
    metadata = list(
      start_date = start_date,
      end_date = end_date,
      frequency = frequency,
      n_markets = length(all_markets),
      market_names = names(all_markets),
      collection_date = Sys.time()
    )
  )
  
  class(dataset) <- "waveqte_dataset"
  return(dataset)
}

#' Collect Market Indices Data
#' 
#' Collects price, return, and volume data for market indices
#' 
#' @param market_symbols Named list of market symbols
#' @param start_date Start date
#' @param end_date End date
#' @param frequency Data frequency
#' @return Market data list
collect_market_indices <- function(market_symbols, start_date, end_date, frequency) {
  cat("Collecting market indices data...\n")
  
  # Simulate comprehensive market data (in practice, would use quantmod/tidyquant)
  date_seq <- generate_date_sequence(start_date, end_date, frequency)
  n_periods <- length(date_seq)
  n_markets <- length(market_symbols)
  
  # Generate realistic market data with correlations
  set.seed(12345)  # For reproducible simulation
  
  # Base parameters for simulation
  annual_returns <- c(0.08, 0.10, 0.07, 0.06, 0.09, 0.05, 0.07, 0.04, 0.03, 0.08, 0.06, 0.05,  # Developed
                     0.12, 0.15, 0.10, 0.14, 0.13, 0.09, 0.11, 0.08, 0.10, 0.09, 0.07, 0.12)   # Emerging
  
  annual_volatilities <- c(0.15, 0.20, 0.18, 0.16, 0.17, 0.15, 0.16, 0.18, 0.17, 0.16, 0.15, 0.14,  # Developed
                          0.25, 0.30, 0.28, 0.32, 0.30, 0.26, 0.28, 0.35, 0.26, 0.24, 0.22, 0.29)   # Emerging
  
  # Convert to period-specific parameters
  if (frequency == "daily") {
    period_returns <- annual_returns / 252
    period_volatilities <- annual_volatilities / sqrt(252)
  } else if (frequency == "weekly") {
    period_returns <- annual_returns / 52
    period_volatilities <- annual_volatilities / sqrt(52)
  } else if (frequency == "monthly") {
    period_returns <- annual_returns / 12
    period_volatilities <- annual_volatilities / sqrt(12)
  }
  
  # Create correlation structure
  correlation_matrix <- generate_realistic_correlation_matrix(n_markets)
  
  # Generate correlated returns
  returns_data <- generate_correlated_returns(n_periods, n_markets, 
                                            period_returns[1:n_markets], 
                                            period_volatilities[1:n_markets],
                                            correlation_matrix)
  
  # Add crisis periods and volatility clustering
  returns_data <- add_crisis_periods(returns_data, date_seq)
  returns_data <- add_volatility_clustering(returns_data)
  
  # Convert returns to prices (starting at 100)
  prices_data <- apply(returns_data, 2, function(r) 100 * cumprod(1 + r))
  
  # Generate volume data (correlated with volatility)
  volumes_data <- generate_volume_data(returns_data, n_periods, n_markets)
  
  # Create data frames with proper dates
  returns_df <- data.frame(Date = date_seq, returns_data)
  prices_df <- data.frame(Date = date_seq, prices_data)
  volumes_df <- data.frame(Date = date_seq, volumes_data)
  
  # Set column names
  market_names <- names(market_symbols)
  colnames(returns_df)[-1] <- market_names
  colnames(prices_df)[-1] <- market_names
  colnames(volumes_df)[-1] <- market_names
  
  return(list(
    returns = returns_df,
    prices = prices_df,
    volumes = volumes_df
  ))
}

#' Generate Date Sequence
#' 
#' Generates appropriate date sequence based on frequency
#' 
#' @param start_date Start date
#' @param end_date End date
#' @param frequency Data frequency
#' @return Date sequence
generate_date_sequence <- function(start_date, end_date, frequency) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (frequency == "daily") {
    dates <- seq(start_date, end_date, by = "day")
    # Remove weekends (simplified)
    dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  } else if (frequency == "weekly") {
    dates <- seq(start_date, end_date, by = "week")
  } else if (frequency == "monthly") {
    dates <- seq(start_date, end_date, by = "month")
  }
  
  return(dates)
}

#' Generate Realistic Correlation Matrix
#' 
#' Creates realistic correlation structure between markets
#' 
#' @param n_markets Number of markets
#' @return Correlation matrix
generate_realistic_correlation_matrix <- function(n_markets) {
  # Base correlation structure
  correlation_matrix <- matrix(0.1, n_markets, n_markets)  # Base correlation
  diag(correlation_matrix) <- 1
  
  # Higher correlations within regions
  # Developed markets (first 12)
  if (n_markets >= 12) {
    # US markets
    correlation_matrix[1:3, 1:3] <- 0.8
    # European markets
    correlation_matrix[4:7, 4:7] <- 0.7
    # Japanese markets
    correlation_matrix[8:9, 8:9] <- 0.85
    # Other developed
    correlation_matrix[10:12, 10:12] <- 0.6
  }
  
  # Emerging markets (if included)
  if (n_markets > 12) {
    emerging_start <- 13
    emerging_end <- min(n_markets, 24)
    
    # Asian emerging markets
    correlation_matrix[emerging_start:(emerging_start+5), emerging_start:(emerging_start+5)] <- 0.6
    # Other emerging markets
    if (emerging_end > emerging_start + 5) {
      correlation_matrix[(emerging_start+6):emerging_end, (emerging_start+6):emerging_end] <- 0.5
    }
  }
  
  # Ensure positive definite
  eigenvals <- eigen(correlation_matrix)$values
  if (min(eigenvals) < 0.01) {
    correlation_matrix <- correlation_matrix + diag(0.01 - min(eigenvals), n_markets)
  }
  
  return(correlation_matrix)
}

#' Generate Correlated Returns
#' 
#' Generates correlated return series using Cholesky decomposition
#' 
#' @param n_periods Number of time periods
#' @param n_markets Number of markets
#' @param means Mean returns
#' @param sds Standard deviations
#' @param correlation_matrix Correlation matrix
#' @return Correlated returns matrix
generate_correlated_returns <- function(n_periods, n_markets, means, sds, correlation_matrix) {
  # Generate independent normal random variables
  independent_normals <- matrix(rnorm(n_periods * n_markets), n_periods, n_markets)
  
  # Apply Cholesky decomposition
  chol_decomp <- chol(correlation_matrix)
  correlated_normals <- independent_normals %*% chol_decomp
  
  # Scale by means and standard deviations
  returns <- sweep(sweep(correlated_normals, 2, sds, "*"), 2, means, "+")
  
  return(returns)
}

#' Add Crisis Periods
#' 
#' Adds realistic crisis periods to return data
#' 
#' @param returns_data Returns matrix
#' @param date_seq Date sequence
#' @return Returns with crisis periods
add_crisis_periods <- function(returns_data, date_seq) {
  # Define crisis periods
  crisis_periods <- list(
    dot_com = c(as.Date("2000-03-01"), as.Date("2002-10-01")),
    financial_crisis = c(as.Date("2007-10-01"), as.Date("2009-03-01")),
    european_crisis = c(as.Date("2010-05-01"), as.Date("2012-09-01")),
    covid_crisis = c(as.Date("2020-02-01"), as.Date("2020-04-01"))
  )
  
  for (crisis in crisis_periods) {
    crisis_mask <- date_seq >= crisis[1] & date_seq <= crisis[2]
    if (any(crisis_mask)) {
      # Increase volatility and add negative drift during crisis
      returns_data[crisis_mask, ] <- returns_data[crisis_mask, ] * 2 - 0.001
    }
  }
  
  return(returns_data)
}

#' Add Volatility Clustering
#' 
#' Adds GARCH-like volatility clustering to returns
#' 
#' @param returns_data Returns matrix
#' @return Returns with volatility clustering
add_volatility_clustering <- function(returns_data) {
  n_periods <- nrow(returns_data)
  n_markets <- ncol(returns_data)
  
  for (j in 1:n_markets) {
    # Simple GARCH(1,1) like process
    sigma_sq <- rep(var(returns_data[, j]), n_periods)
    
    for (i in 2:n_periods) {
      sigma_sq[i] <- 0.00001 + 0.05 * returns_data[i-1, j]^2 + 0.9 * sigma_sq[i-1]
      returns_data[i, j] <- returns_data[i, j] * sqrt(sigma_sq[i] / var(returns_data[, j]))
    }
  }
  
  return(returns_data)
}

#' Generate Volume Data
#' 
#' Generates realistic volume data correlated with returns and volatility
#' 
#' @param returns_data Returns matrix
#' @param n_periods Number of periods
#' @param n_markets Number of markets
#' @return Volume data matrix
generate_volume_data <- function(returns_data, n_periods, n_markets) {
  volumes <- matrix(0, n_periods, n_markets)
  
  for (j in 1:n_markets) {
    # Base volume level
    base_volume <- 1000000 * (1 + rnorm(1, 0, 0.5))
    
    # Volume correlated with absolute returns (volatility)
    abs_returns <- abs(returns_data[, j])
    volume_multiplier <- 1 + 2 * (abs_returns - mean(abs_returns)) / sd(abs_returns)
    volume_multiplier <- pmax(0.1, volume_multiplier)  # Ensure positive
    
    # Add random component
    volumes[, j] <- base_volume * volume_multiplier * exp(rnorm(n_periods, 0, 0.2))
  }
  
  return(volumes)
}

#' Collect Macroeconomic Data
#' 
#' Collects comprehensive macroeconomic and financial time series
#' 
#' @param start_date Start date
#' @param end_date End date
#' @param frequency Data frequency
#' @return Macroeconomic dataset
collect_macroeconomic_data <- function(start_date, end_date, frequency) {
  cat("Collecting macroeconomic data...\n")
  
  date_seq <- generate_date_sequence(start_date, end_date, frequency)
  n_periods <- length(date_seq)
  
  # Simulate comprehensive macro data
  macro_data <- data.frame(
    Date = date_seq,
    
    # Growth indicators
    US_GDP_Growth = generate_macro_series(n_periods, 0.02, 0.01, 0.8),
    EU_GDP_Growth = generate_macro_series(n_periods, 0.015, 0.012, 0.75),
    JP_GDP_Growth = generate_macro_series(n_periods, 0.008, 0.008, 0.7),
    CN_GDP_Growth = generate_macro_series(n_periods, 0.06, 0.02, 0.85),
    
    # Inflation rates
    US_CPI = generate_macro_series(n_periods, 0.025, 0.005, 0.9),
    EU_CPI = generate_macro_series(n_periods, 0.02, 0.004, 0.88),
    JP_CPI = generate_macro_series(n_periods, 0.005, 0.003, 0.85),
    
    # Interest rates
    US_10Y_Yield = generate_macro_series(n_periods, 0.03, 0.01, 0.95),
    EU_10Y_Yield = generate_macro_series(n_periods, 0.02, 0.008, 0.93),
    JP_10Y_Yield = generate_macro_series(n_periods, 0.005, 0.003, 0.9),
    
    # Policy rates
    US_Fed_Rate = generate_macro_series(n_periods, 0.02, 0.015, 0.98),
    EU_ECB_Rate = generate_macro_series(n_periods, 0.01, 0.01, 0.97),
    JP_BOJ_Rate = generate_macro_series(n_periods, 0.001, 0.002, 0.95),
    
    # Exchange rates (vs USD)
    EUR_USD = generate_macro_series(n_periods, 1.2, 0.1, 0.9),
    JPY_USD = generate_macro_series(n_periods, 110, 10, 0.85),
    GBP_USD = generate_macro_series(n_periods, 1.3, 0.08, 0.88),
    
    # Commodity prices
    Oil_Price = generate_macro_series(n_periods, 70, 20, 0.8),
    Gold_Price = generate_macro_series(n_periods, 1500, 200, 0.85),
    Copper_Price = generate_macro_series(n_periods, 7000, 1500, 0.75),
    
    # Volatility indices
    VIX = generate_macro_series(n_periods, 20, 8, 0.7, min_val = 10, max_val = 80),
    VSTOXX = generate_macro_series(n_periods, 22, 9, 0.72, min_val = 12, max_val = 85),
    
    # Credit spreads
    US_IG_Spread = generate_macro_series(n_periods, 150, 50, 0.8, min_val = 50),
    US_HY_Spread = generate_macro_series(n_periods, 400, 200, 0.75, min_val = 200),
    
    # Liquidity measures
    TED_Spread = generate_macro_series(n_periods, 0.5, 0.3, 0.7, min_val = 0.1),
    US_2Y_10Y_Spread = generate_macro_series(n_periods, 1.5, 1, 0.85)
  )
  
  return(macro_data)
}

#' Generate Macroeconomic Series
#' 
#' Generates realistic macroeconomic time series with persistence
#' 
#' @param n_periods Number of periods
#' @param mean Mean value
#' @param sd Standard deviation
#' @param persistence AR(1) persistence parameter
#' @param min_val Minimum value (optional)
#' @param max_val Maximum value (optional)
#' @return Time series vector
generate_macro_series <- function(n_periods, mean, sd, persistence, min_val = NULL, max_val = NULL) {
  # Generate AR(1) process
  series <- numeric(n_periods)
  series[1] <- mean + rnorm(1, 0, sd)
  
  for (i in 2:n_periods) {
    series[i] <- mean * (1 - persistence) + persistence * series[i-1] + rnorm(1, 0, sd)
  }
  
  # Apply bounds if specified
  if (!is.null(min_val)) {
    series <- pmax(series, min_val)
  }
  if (!is.null(max_val)) {
    series <- pmin(series, max_val)
  }
  
  return(series)
}

#' Process Dataset for WaveQTE Analysis
#' 
#' Processes raw dataset for WaveQTE network economics analysis
#' 
#' @param dataset Raw dataset from collect_global_market_data
#' @param return_type Type of returns to calculate
#' @param outlier_treatment How to handle outliers
#' @param missing_data_method Method for handling missing data
#' @return Processed dataset ready for analysis
#' @export
process_waveqte_dataset <- function(dataset, 
                                   return_type = "log", 
                                   outlier_treatment = "winsorize",
                                   missing_data_method = "interpolate") {
  
  cat("Processing dataset for WaveQTE analysis...\n")
  
  # Calculate returns if not already done
  if (return_type == "log") {
    returns_matrix <- calculate_log_returns(dataset$market_prices)
  } else if (return_type == "simple") {
    returns_matrix <- calculate_simple_returns(dataset$market_prices)
  } else {
    returns_matrix <- dataset$market_returns[, -1]  # Remove date column
  }
  
  # Handle missing data
  returns_matrix <- handle_missing_data(returns_matrix, missing_data_method)
  
  # Handle outliers
  returns_matrix <- handle_outliers(returns_matrix, outlier_treatment)
  
  # Create processed dataset
  processed_dataset <- list(
    returns = returns_matrix,
    dates = dataset$market_returns$Date,
    market_names = dataset$metadata$market_names,
    macro_data = dataset$macro_data,
    processing_info = list(
      return_type = return_type,
      outlier_treatment = outlier_treatment,
      missing_data_method = missing_data_method,
      processing_date = Sys.time()
    ),
    metadata = dataset$metadata
  )
  
  class(processed_dataset) <- "waveqte_processed_dataset"
  return(processed_dataset)
}

#' Calculate Log Returns
#' 
#' Calculates log returns from price data
#' 
#' @param price_data Price data frame
#' @return Log returns matrix
calculate_log_returns <- function(price_data) {
  prices <- as.matrix(price_data[, -1])  # Remove date column
  log_returns <- diff(log(prices))
  return(log_returns)
}

#' Calculate Simple Returns
#' 
#' Calculates simple returns from price data
#' 
#' @param price_data Price data frame
#' @return Simple returns matrix
calculate_simple_returns <- function(price_data) {
  prices <- as.matrix(price_data[, -1])  # Remove date column
  simple_returns <- diff(prices) / prices[-nrow(prices), ]
  return(simple_returns)
}

#' Handle Missing Data
#' 
#' Handles missing data in return series
#' 
#' @param returns_matrix Returns matrix
#' @param method Method for handling missing data
#' @return Returns matrix with missing data handled
handle_missing_data <- function(returns_matrix, method) {
  if (method == "interpolate") {
    # Linear interpolation
    for (j in 1:ncol(returns_matrix)) {
      na_indices <- which(is.na(returns_matrix[, j]))
      if (length(na_indices) > 0) {
        returns_matrix[, j] <- approx(1:nrow(returns_matrix), returns_matrix[, j], 
                                    xout = 1:nrow(returns_matrix))$y
      }
    }
  } else if (method == "forward_fill") {
    # Forward fill
    for (j in 1:ncol(returns_matrix)) {
      na_indices <- which(is.na(returns_matrix[, j]))
      for (i in na_indices) {
        if (i > 1) {
          returns_matrix[i, j] <- returns_matrix[i-1, j]
        }
      }
    }
  } else if (method == "zero_fill") {
    # Replace with zeros
    returns_matrix[is.na(returns_matrix)] <- 0
  }
  
  return(returns_matrix)
}

#' Handle Outliers
#' 
#' Handles outliers in return series
#' 
#' @param returns_matrix Returns matrix
#' @param method Method for handling outliers
#' @return Returns matrix with outliers handled
handle_outliers <- function(returns_matrix, method) {
  if (method == "winsorize") {
    # Winsorize at 1% and 99% quantiles
    for (j in 1:ncol(returns_matrix)) {
      q01 <- quantile(returns_matrix[, j], 0.01, na.rm = TRUE)
      q99 <- quantile(returns_matrix[, j], 0.99, na.rm = TRUE)
      returns_matrix[, j] <- pmax(pmin(returns_matrix[, j], q99), q01)
    }
  } else if (method == "trim") {
    # Remove extreme outliers (beyond 3 standard deviations)
    for (j in 1:ncol(returns_matrix)) {
      mean_ret <- mean(returns_matrix[, j], na.rm = TRUE)
      sd_ret <- sd(returns_matrix[, j], na.rm = TRUE)
      outlier_indices <- abs(returns_matrix[, j] - mean_ret) > 3 * sd_ret
      returns_matrix[outlier_indices, j] <- NA
    }
    # Interpolate removed outliers
    returns_matrix <- handle_missing_data(returns_matrix, "interpolate")
  }
  
  return(returns_matrix)
}

#' Create Sample Dataset
#' 
#' Creates a sample dataset for testing and demonstration
#' 
#' @param n_periods Number of time periods
#' @param n_markets Number of markets
#' @return Sample dataset
#' @export
create_sample_dataset <- function(n_periods = 1000, n_markets = 12) {
  cat("Creating sample dataset with", n_periods, "periods and", n_markets, "markets\n")
  
  # Create synthetic data for testing
  start_date <- Sys.Date() - n_periods
  end_date <- Sys.Date()
  
  dataset <- collect_global_market_data(
    start_date = start_date,
    end_date = end_date,
    frequency = "daily",
    include_emerging = n_markets > 12,
    include_macro = TRUE
  )
  
  return(dataset)
}