# WaveQTEX: Efficient Market Hypothesis Testing Functions
# Multi-Scale Market Efficiency Analysis
# Version 1.0.0

#' Multi-Scale Efficient Market Hypothesis Testing
#'
#' Performs comprehensive efficiency testing across multiple wavelet scales
#' to examine scale-dependent market efficiency patterns.
#'
#' @param decomp_data Object of class "waveqte_decomposition"
#' @param tests Vector of test names to perform (default: c("adf", "kpss", "variance_ratio"))
#' @param lags Vector of lags for variance ratio test (default: c(2, 4, 8, 16))
#' @return Data frame with efficiency test results
#' @export
#' @importFrom tseries adf.test
#' @importFrom urca ur.kpss
#' @examples
#' \dontrun{
#' efficiency_results <- multi_scale_efficiency_test(decomp)
#' }
multi_scale_efficiency_test <- function(decomp_data, 
                                       tests = c("adf", "kpss", "variance_ratio"),
                                       lags = c(2, 4, 8, 16)) {
  
  if (!inherits(decomp_data, "waveqte_decomposition")) {
    stop("Input must be of class 'waveqte_decomposition'")
  }
  
  markets <- decomp_data$markets
  scales <- decomp_data$scales
  
  # Initialize result data frame
  result_df <- expand.grid(
    market = markets,
    scale = scales,
    stringsAsFactors = FALSE
  )
  
  # Initialize test result columns
  if ("adf" %in% tests) {
    result_df$adf_statistic <- NA
    result_df$adf_p_value <- NA
  }
  
  if ("kpss" %in% tests) {
    result_df$kpss_statistic <- NA
    result_df$kpss_p_value <- NA
  }
  
  if ("variance_ratio" %in% tests) {
    for (lag in lags) {
      result_df[[paste0("vr_", lag)]] <- NA
      result_df[[paste0("vr_", lag, "_p_value")]] <- NA
    }
  }
  
  result_df$efficiency_score <- NA
  
  # Perform tests for each market and scale
  for (market in markets) {
    for (scale in scales) {
      
      scale_name <- paste0("scale_", scale)
      coeffs <- decomp_data$decomposition[[market]][[scale_name]]
      
      if (is.null(coeffs) || length(coeffs) < 50) {
        warning(paste("Insufficient data for", market, "at", scale_name))
        next
      }
      
      # Remove NAs and outliers
      coeffs <- coeffs[!is.na(coeffs)]
      coeffs <- coeffs[abs(coeffs) < quantile(abs(coeffs), 0.99, na.rm = TRUE)]
      
      if (length(coeffs) < 30) {
        next
      }
      
      row_idx <- result_df$market == market & result_df$scale == scale
      
      # Augmented Dickey-Fuller Test
      if ("adf" %in% tests) {
        tryCatch({
          adf_result <- tseries::adf.test(coeffs, alternative = "stationary")
          result_df$adf_statistic[row_idx] <- adf_result$statistic
          result_df$adf_p_value[row_idx] <- adf_result$p.value
        }, error = function(e) {
          warning(paste("ADF test failed for", market, scale_name, ":", e$message))
        })
      }
      
      # KPSS Test
      if ("kpss" %in% tests) {
        tryCatch({
          kpss_result <- urca::ur.kpss(coeffs, type = "mu")
          result_df$kpss_statistic[row_idx] <- kpss_result@teststat
          # KPSS critical values (approximate)
          kpss_cv <- kpss_result@cval
          if (kpss_result@teststat < kpss_cv[1, "5pct"]) {
            result_df$kpss_p_value[row_idx] <- 0.1  # Fail to reject H0 (stationary)
          } else {
            result_df$kpss_p_value[row_idx] <- 0.01  # Reject H0 (non-stationary)
          }
        }, error = function(e) {
          warning(paste("KPSS test failed for", market, scale_name, ":", e$message))
        })
      }
      
      # Variance Ratio Tests
      if ("variance_ratio" %in% tests) {
        for (lag in lags) {
          tryCatch({
            vr_result <- variance_ratio_test(coeffs, lag)
            result_df[[paste0("vr_", lag)]][row_idx] <- vr_result$vr
            result_df[[paste0("vr_", lag, "_p_value")]][row_idx] <- vr_result$p_value
          }, error = function(e) {
            warning(paste("VR test failed for", market, scale_name, "lag", lag, ":", e$message))
          })
        }
      }
      
      # Calculate composite efficiency score
      efficiency_components <- c()
      
      # ADF component (lower p-value = more efficient)
      if (!is.na(result_df$adf_p_value[row_idx])) {
        adf_component <- 1 - result_df$adf_p_value[row_idx]
        efficiency_components <- c(efficiency_components, adf_component)
      }
      
      # KPSS component (higher p-value = more efficient)
      if (!is.na(result_df$kpss_p_value[row_idx])) {
        kpss_component <- result_df$kpss_p_value[row_idx]
        efficiency_components <- c(efficiency_components, kpss_component)
      }
      
      # VR component (closer to 1 = more efficient)
      if ("variance_ratio" %in% tests) {
        vr_components <- c()
        for (lag in lags) {
          vr_col <- paste0("vr_", lag)
          if (!is.na(result_df[[vr_col]][row_idx])) {
            vr_deviation <- abs(result_df[[vr_col]][row_idx] - 1)
            vr_component <- exp(-vr_deviation)  # Exponential decay from 1
            vr_components <- c(vr_components, vr_component)
          }
        }
        if (length(vr_components) > 0) {
          efficiency_components <- c(efficiency_components, mean(vr_components))
        }
      }
      
      if (length(efficiency_components) > 0) {
        result_df$efficiency_score[row_idx] <- mean(efficiency_components)
      }
    }
  }
  
  class(result_df) <- c("waveqte_efficiency", "data.frame")
  return(result_df)
}

#' Scale-Specific Efficiency Test
#'
#' Performs efficiency tests for a specific scale across all markets.
#'
#' @param decomp_data Object of class "waveqte_decomposition"
#' @param scale Integer specifying which scale to test
#' @param test_type Character string specifying test type
#' @return List with test results
#' @export
#' @examples
#' \dontrun{
#' scale_test <- scale_efficiency_test(decomp, scale = 1, test_type = "adf")
#' }
scale_efficiency_test <- function(decomp_data, scale, test_type = "adf") {
  
  if (!inherits(decomp_data, "waveqte_decomposition")) {
    stop("Input must be of class 'waveqte_decomposition'")
  }
  
  if (!scale %in% decomp_data$scales) {
    stop(paste("Scale", scale, "not available in decomposition"))
  }
  
  markets <- decomp_data$markets
  scale_name <- paste0("scale_", scale)
  
  results <- list()
  results$scale <- scale
  results$test_type <- test_type
  results$markets <- markets
  
  test_stats <- numeric(length(markets))
  p_values <- numeric(length(markets))
  names(test_stats) <- markets
  names(p_values) <- markets
  
  for (i in 1:length(markets)) {
    market <- markets[i]
    coeffs <- decomp_data$decomposition[[market]][[scale_name]]
    
    if (is.null(coeffs) || length(coeffs) < 30) {
      test_stats[i] <- NA
      p_values[i] <- NA
      next
    }
    
    # Clean data
    coeffs <- coeffs[!is.na(coeffs)]
    
    if (test_type == "adf") {
      tryCatch({
        test_result <- tseries::adf.test(coeffs, alternative = "stationary")
        test_stats[i] <- test_result$statistic
        p_values[i] <- test_result$p.value
      }, error = function(e) {
        test_stats[i] <- NA
        p_values[i] <- NA
      })
    } else if (test_type == "kpss") {
      tryCatch({
        test_result <- urca::ur.kpss(coeffs, type = "mu")
        test_stats[i] <- test_result@teststat
        # Approximate p-value based on critical values
        cv <- test_result@cval["tau3", "5pct"]
        p_values[i] <- ifelse(test_result@teststat < cv, 0.1, 0.01)
      }, error = function(e) {
        test_stats[i] <- NA
        p_values[i] <- NA
      })
    }
  }
  
  results$test_statistics <- test_stats
  results$p_values <- p_values
  results$summary <- data.frame(
    market = markets,
    test_statistic = test_stats,
    p_value = p_values,
    efficient = p_values < 0.05,
    stringsAsFactors = FALSE
  )
  
  return(results)
}

#' Variance Ratio Test
#'
#' Performs Lo-MacKinlay variance ratio test for random walk hypothesis.
#'
#' @param x Numeric vector of time series data
#' @param lag Integer lag for variance ratio calculation
#' @return List with variance ratio and test statistics
#' @export
#' @importFrom stats var
#' @examples
#' \dontrun{
#' vr_result <- variance_ratio_test(returns, lag = 4)
#' }
variance_ratio_test <- function(x, lag = 2) {
  
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < lag * 2) {
    stop("Insufficient observations for variance ratio test")
  }
  
  # Calculate variance ratio
  var_1 <- var(x)
  
  # Calculate lag-k differences
  x_lag <- numeric(n - lag)
  for (i in 1:(n - lag)) {
    x_lag[i] <- sum(x[i:(i + lag - 1)])
  }
  
  var_k <- var(x_lag) / lag
  
  vr <- var_k / var_1
  
  # Calculate test statistic (Lo-MacKinlay)
  # Under null hypothesis of random walk, VR should equal 1
  m <- lag
  z_stat <- sqrt(n) * (vr - 1) / sqrt((2 * (2 * m - 1) * (m - 1)) / (3 * m))
  
  # Calculate p-value (two-tailed test)
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  result <- list(
    vr = vr,
    z_statistic = z_stat,
    p_value = p_value,
    lag = lag,
    n_obs = n,
    null_hypothesis = "Random walk (VR = 1)"
  )
  
  return(result)
}

#' Calculate Efficiency Score
#'
#' Computes a comprehensive efficiency score combining multiple test results.
#'
#' @param efficiency_data Object of class "waveqte_efficiency" 
#' @param weights Named vector of weights for different test components
#' @return Data frame with updated efficiency scores
#' @export
#' @examples
#' \dontrun{
#' scores <- efficiency_score(efficiency_results, 
#'                           weights = c(adf = 0.3, kpss = 0.3, vr = 0.4))
#' }
efficiency_score <- function(efficiency_data, 
                            weights = c(adf = 0.3, kpss = 0.3, vr = 0.4)) {
  
  if (!inherits(efficiency_data, "waveqte_efficiency")) {
    stop("Input must be of class 'waveqte_efficiency'")
  }
  
  result_df <- efficiency_data
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Calculate weighted efficiency score for each row
  for (i in 1:nrow(result_df)) {
    
    score_components <- c()
    
    # ADF component
    if ("adf" %in% names(weights) && !is.na(result_df$adf_p_value[i])) {
      adf_score <- 1 - result_df$adf_p_value[i]  # Lower p-value = more efficient
      score_components <- c(score_components, weights["adf"] * adf_score)
    }
    
    # KPSS component  
    if ("kpss" %in% names(weights) && !is.na(result_df$kpss_p_value[i])) {
      kpss_score <- result_df$kpss_p_value[i]  # Higher p-value = more efficient
      score_components <- c(score_components, weights["kpss"] * kpss_score)
    }
    
    # Variance ratio component
    if ("vr" %in% names(weights)) {
      vr_cols <- grep("^vr_[0-9]+$", names(result_df), value = TRUE)
      if (length(vr_cols) > 0) {
        vr_scores <- c()
        for (col in vr_cols) {
          if (!is.na(result_df[[col]][i])) {
            vr_deviation <- abs(result_df[[col]][i] - 1)
            vr_score <- exp(-vr_deviation)  # Closer to 1 = more efficient
            vr_scores <- c(vr_scores, vr_score)
          }
        }
        if (length(vr_scores) > 0) {
          avg_vr_score <- mean(vr_scores)
          score_components <- c(score_components, weights["vr"] * avg_vr_score)
        }
      }
    }
    
    # Calculate final weighted score
    if (length(score_components) > 0) {
      # Normalize by sum of weights used
      used_weights <- sum(weights[c(
        if (!is.na(result_df$adf_p_value[i])) "adf" else character(0),
        if (!is.na(result_df$kpss_p_value[i])) "kpss" else character(0),
        if (any(!is.na(result_df[i, grep("^vr_[0-9]+$", names(result_df))]))) "vr" else character(0)
      )])
      
      result_df$efficiency_score[i] <- sum(score_components) / used_weights
    }
  }
  
  # Add efficiency ranking within each scale
  result_df <- result_df %>%
    dplyr::group_by(scale) %>%
    dplyr::mutate(
      efficiency_rank = rank(-efficiency_score, ties.method = "min"),
      efficiency_percentile = rank(efficiency_score) / dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  return(result_df)
}

#' Adaptive Market Efficiency Analysis
#'
#' Examines time-varying market efficiency using rolling window analysis.
#'
#' @param data Matrix or data.frame of financial time series
#' @param window_size Integer size of rolling window (default: 252)
#' @param step_size Integer step size for rolling window (default: 21)
#' @param scale Integer wavelet scale to analyze (default: 1)
#' @return Data frame with time-varying efficiency measures
#' @export
#' @importFrom zoo rollapply
#' @examples
#' \dontrun{
#' adaptive_eff <- adaptive_efficiency(stock_returns, window_size = 252)
#' }
adaptive_efficiency <- function(data, window_size = 252, step_size = 21, scale = 1) {
  
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame")
  }
  
  n_obs <- nrow(data)
  n_markets <- ncol(data)
  market_names <- colnames(data)
  
  if (is.null(market_names)) {
    market_names <- paste0("Market_", 1:n_markets)
  }
  
  if (window_size > n_obs) {
    stop("Window size cannot be larger than data length")
  }
  
  # Calculate number of windows
  n_windows <- floor((n_obs - window_size) / step_size) + 1
  
  # Initialize result data frame
  result_df <- data.frame()
  
  for (w in 1:n_windows) {
    
    start_idx <- (w - 1) * step_size + 1
    end_idx <- start_idx + window_size - 1
    
    if (end_idx > n_obs) {
      break
    }
    
    # Extract window data
    window_data <- data[start_idx:end_idx, , drop = FALSE]
    
    # Perform wavelet decomposition
    tryCatch({
      decomp <- waveqte_decompose(window_data, scales = scale)
      
      # Test efficiency for this window
      efficiency_results <- multi_scale_efficiency_test(decomp, 
                                                       tests = "adf")
      
      # Add time information
      efficiency_results$window <- w
      efficiency_results$start_date <- start_idx
      efficiency_results$end_date <- end_idx
      efficiency_results$center_date <- round((start_idx + end_idx) / 2)
      
      result_df <- rbind(result_df, efficiency_results)
      
    }, error = function(e) {
      warning(paste("Error in window", w, ":", e$message))
    })
  }
  
  # Calculate rolling statistics
  if (nrow(result_df) > 0) {
    result_df <- result_df %>%
      dplyr::group_by(market) %>%
      dplyr::arrange(window) %>%
      dplyr::mutate(
        efficiency_trend = c(NA, diff(efficiency_score)),
        efficiency_ma = zoo::rollapply(efficiency_score, width = 3, 
                                      FUN = mean, fill = NA, align = "right")
      ) %>%
      dplyr::ungroup()
  }
  
  class(result_df) <- c("adaptive_efficiency", "data.frame")
  return(result_df)
}