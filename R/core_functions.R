# WaveQTEX: Core Functions
# Extended Wavelet-Based Quantile Transfer Entropy Networks
# Version 1.0.0

#' Wavelet Decomposition for Financial Time Series
#'
#' Performs discrete wavelet transform decomposition of financial time series
#' into multiple temporal scales for subsequent QTE analysis.
#'
#' @param data Matrix or data.frame of financial time series (each column is a market)
#' @param scales Integer vector specifying wavelet scales to compute (default: 1:6)
#' @param wavelet Character string specifying wavelet family (default: "d4")
#' @param method Character string for boundary conditions (default: "periodic")
#' @return List containing wavelet coefficients for each scale and market
#' @export
#' @importFrom wavelets dwt
#' @examples
#' \dontrun{
#' data(stock_returns)
#' decomp <- waveqte_decompose(stock_returns, scales = 1:6)
#' }
waveqte_decompose <- function(data, scales = 1:6, wavelet = "d4", method = "periodic") {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame")
  }
  
  if (ncol(data) < 2) {
    stop("Data must contain at least 2 time series")
  }
  
  n_markets <- ncol(data)
  n_obs <- nrow(data)
  market_names <- colnames(data)
  
  if (is.null(market_names)) {
    market_names <- paste0("Market_", 1:n_markets)
  }
  
  # Initialize result structure
  result <- list()
  result$scales <- scales
  result$markets <- market_names
  result$n_obs <- n_obs
  result$wavelet <- wavelet
  
  # Perform wavelet decomposition for each market
  decomposition <- list()
  
  for (i in 1:n_markets) {
    market_data <- as.numeric(data[, i])
    
    # Remove NAs
    market_data <- market_data[!is.na(market_data)]
    
    if (length(market_data) < 32) {
      warning(paste("Market", market_names[i], "has insufficient data for decomposition"))
      next
    }
    
    # Perform DWT
    dwt_result <- wavelets::dwt(market_data, filter = wavelet, 
                               n.levels = max(scales), boundary = method)
    
    # Extract coefficients for specified scales
    market_coeffs <- list()
    for (scale in scales) {
      if (scale <= length(dwt_result@W)) {
        market_coeffs[[paste0("scale_", scale)]] <- dwt_result@W[[scale]]
      } else {
        warning(paste("Scale", scale, "not available for market", market_names[i]))
      }
    }
    
    decomposition[[market_names[i]]] <- market_coeffs
  }
  
  result$decomposition <- decomposition
  result$original_data <- data
  
  class(result) <- "waveqte_decomposition"
  return(result)
}

#' Calculate Quantile Transfer Entropy
#'
#' Computes quantile transfer entropy between pairs of wavelet-decomposed 
#' financial time series at specified quantile levels.
#'
#' @param decomp_data Object of class "waveqte_decomposition" 
#' @param quantiles Numeric vector of quantile levels (default: c(0.05, 0.50, 0.95))
#' @param lag Integer lag for transfer entropy calculation (default: 1)
#' @param min_obs Minimum observations required per cell (default: 30)
#' @return Array of QTE values [from_market, to_market, scale, quantile]
#' @export
#' @importFrom stats quantile
#' @examples
#' \dontrun{
#' qte_results <- calculate_qte(decomp, quantiles = c(0.05, 0.50, 0.95))
#' }
calculate_qte <- function(decomp_data, quantiles = c(0.05, 0.50, 0.95), 
                         lag = 1, min_obs = 30) {
  
  if (!inherits(decomp_data, "waveqte_decomposition")) {
    stop("Input must be of class 'waveqte_decomposition'")
  }
  
  markets <- decomp_data$markets
  scales <- decomp_data$scales
  n_markets <- length(markets)
  n_scales <- length(scales)
  n_quantiles <- length(quantiles)
  
  # Initialize result array
  qte_array <- array(0, 
                    dim = c(n_markets, n_markets, n_scales, n_quantiles),
                    dimnames = list(from = markets, 
                                   to = markets,
                                   scale = paste0("scale_", scales),
                                   quantile = paste0("q_", quantiles)))
  
  # Calculate QTE for each pair, scale, and quantile
  for (s in 1:n_scales) {
    scale_name <- paste0("scale_", scales[s])
    
    for (q in 1:n_quantiles) {
      quantile_level <- quantiles[q]
      
      for (i in 1:n_markets) {
        for (j in 1:n_markets) {
          if (i != j) {  # Skip self-loops
            
            # Get wavelet coefficients
            x_coeffs <- decomp_data$decomposition[[markets[i]]][[scale_name]]
            y_coeffs <- decomp_data$decomposition[[markets[j]]][[scale_name]]
            
            if (is.null(x_coeffs) || is.null(y_coeffs)) {
              warning(paste("Missing coefficients for", markets[i], "->", markets[j], 
                          "at", scale_name))
              next
            }
            
            # Align series lengths
            min_length <- min(length(x_coeffs), length(y_coeffs))
            if (min_length < min_obs) {
              warning(paste("Insufficient observations for", markets[i], "->", markets[j], 
                          "at", scale_name, "quantile", quantile_level))
              next
            }
            
            x_coeffs <- x_coeffs[1:min_length]
            y_coeffs <- y_coeffs[1:min_length]
            
            # Calculate quantile thresholds
            x_threshold <- quantile(x_coeffs, quantile_level, na.rm = TRUE)
            y_threshold <- quantile(y_coeffs, quantile_level, na.rm = TRUE)
            
            # Create binary indicator variables
            x_binary <- as.numeric(x_coeffs <= x_threshold)
            y_binary <- as.numeric(y_coeffs <= y_threshold)
            
            # Calculate transfer entropy
            qte_value <- compute_transfer_entropy_binary(x_binary, y_binary, lag)
            
            qte_array[i, j, s, q] <- qte_value
          }
        }
      }
    }
  }
  
  # Create result object
  result <- list(
    qte_array = qte_array,
    markets = markets,
    scales = scales,
    quantiles = quantiles,
    lag = lag,
    min_obs = min_obs
  )
  
  class(result) <- "waveqte_result"
  return(result)
}

#' Internal function to compute transfer entropy for binary series
#' @param x Binary time series (source)
#' @param y Binary time series (target)
#' @param lag Lag parameter
#' @return Transfer entropy value
compute_transfer_entropy_binary <- function(x, y, lag = 1) {
  
  n <- length(y)
  if (n <= lag) {
    return(0)
  }
  
  # Create lagged series
  y_t <- y[(lag + 1):n]
  y_t_lag <- y[1:(n - lag)]
  x_t_lag <- x[1:(n - lag)]
  
  # Count joint occurrences
  states <- data.frame(y_t = y_t, y_t_lag = y_t_lag, x_t_lag = x_t_lag)
  
  # Calculate probabilities
  p_xyz <- table(states$y_t, states$y_t_lag, states$x_t_lag) / nrow(states)
  p_xy <- table(states$y_t, states$y_t_lag) / nrow(states)
  p_yz <- table(states$y_t_lag, states$x_t_lag) / nrow(states)
  p_y <- table(states$y_t_lag) / nrow(states)
  
  # Calculate transfer entropy
  te <- 0
  
  for (i in dimnames(p_xyz)[[1]]) {
    for (j in dimnames(p_xyz)[[2]]) {
      for (k in dimnames(p_xyz)[[3]]) {
        
        p_xyz_ijk <- p_xyz[i, j, k]
        
        if (p_xyz_ijk > 0) {
          p_xy_ij <- p_xy[i, j]
          p_yz_jk <- p_yz[j, k]
          p_y_j <- p_y[j]
          
          if (p_xy_ij > 0 && p_yz_jk > 0 && p_y_j > 0) {
            conditional_prob <- p_xyz_ijk / p_yz_jk
            marginal_prob <- p_xy_ij / p_y_j
            
            if (conditional_prob > 0 && marginal_prob > 0) {
              te <- te + p_xyz_ijk * log2(conditional_prob / marginal_prob)
            }
          }
        }
      }
    }
  }
  
  return(max(0, te))  # Ensure non-negative
}

#' Adaptive Network Thresholding
#'
#' Applies adaptive thresholding to QTE matrices to create meaningful
#' network structures while avoiding the empty network problem.
#'
#' @param qte_result Object of class "waveqte_result"
#' @param threshold_percentile Percentile for threshold calculation (default: 0.75)
#' @param min_density Minimum network density to maintain (default: 0.05)
#' @return List of thresholded adjacency matrices
#' @export
#' @importFrom stats quantile
#' @examples
#' \dontrun{
#' networks <- adaptive_threshold(qte_results, threshold_percentile = 0.75)
#' }
adaptive_threshold <- function(qte_result, threshold_percentile = 0.75, 
                              min_density = 0.05) {
  
  if (!inherits(qte_result, "waveqte_result")) {
    stop("Input must be of class 'waveqte_result'")
  }
  
  qte_array <- qte_result$qte_array
  markets <- qte_result$markets
  scales <- qte_result$scales
  quantiles <- qte_result$quantiles
  
  n_markets <- length(markets)
  n_scales <- length(scales)
  n_quantiles <- length(quantiles)
  
  # Initialize result structure
  networks <- array(0, 
                   dim = c(n_markets, n_markets, n_scales, n_quantiles),
                   dimnames = dimnames(qte_array))
  
  thresholds <- array(0, 
                     dim = c(n_scales, n_quantiles),
                     dimnames = list(scale = paste0("scale_", scales),
                                    quantile = paste0("q_", quantiles)))
  
  # Calculate adaptive thresholds for each scale-quantile combination
  for (s in 1:n_scales) {
    for (q in 1:n_quantiles) {
      
      # Extract QTE matrix for this scale-quantile
      qte_matrix <- qte_array[, , s, q]
      
      # Get non-zero, non-diagonal elements
      non_zero_elements <- qte_matrix[qte_matrix > 0 & !diag(n_markets)]
      
      if (length(non_zero_elements) == 0) {
        warning(paste("No non-zero QTE values for scale", scales[s], 
                     "quantile", quantiles[q]))
        next
      }
      
      # Calculate adaptive threshold
      threshold <- quantile(non_zero_elements, threshold_percentile, na.rm = TRUE)
      
      # Apply threshold
      network_matrix <- qte_matrix
      network_matrix[network_matrix < threshold] <- 0
      diag(network_matrix) <- 0  # Remove self-loops
      
      # Check minimum density constraint
      n_possible_edges <- n_markets * (n_markets - 1)
      n_actual_edges <- sum(network_matrix > 0)
      actual_density <- n_actual_edges / n_possible_edges
      
      if (actual_density < min_density && length(non_zero_elements) > 0) {
        # Lower threshold to maintain minimum density
        n_required_edges <- ceiling(min_density * n_possible_edges)
        if (n_required_edges <= length(non_zero_elements)) {
          threshold <- sort(non_zero_elements, decreasing = TRUE)[n_required_edges]
          network_matrix <- qte_matrix
          network_matrix[network_matrix < threshold] <- 0
          diag(network_matrix) <- 0
        }
      }
      
      networks[, , s, q] <- network_matrix
      thresholds[s, q] <- threshold
    }
  }
  
  # Create result object
  result <- list(
    networks = networks,
    thresholds = thresholds,
    markets = markets,
    scales = scales,
    quantiles = quantiles,
    threshold_percentile = threshold_percentile,
    min_density = min_density,
    original_qte = qte_result
  )
  
  class(result) <- "waveqte_networks"
  return(result)
}

#' Construct Network Objects
#'
#' Creates igraph network objects from thresholded QTE matrices
#' for visualization and network analysis.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param scale Integer specifying which scale to use
#' @param quantile Numeric specifying which quantile to use
#' @return igraph network object
#' @export
#' @importFrom igraph graph_from_adjacency_matrix
#' @examples
#' \dontrun{
#' network_obj <- construct_network(networks, scale = 1, quantile = 0.95)
#' }
construct_network <- function(networks_result, scale, quantile) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("Input must be of class 'waveqte_networks'")
  }
  
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  
  # Find indices
  scale_idx <- which(scales == scale)
  quantile_idx <- which(abs(quantiles - quantile) < 1e-6)
  
  if (length(scale_idx) == 0) {
    stop(paste("Scale", scale, "not found in networks"))
  }
  
  if (length(quantile_idx) == 0) {
    stop(paste("Quantile", quantile, "not found in networks"))
  }
  
  # Extract network matrix
  network_matrix <- networks_result$networks[, , scale_idx, quantile_idx]
  
  # Create igraph object
  g <- igraph::graph_from_adjacency_matrix(network_matrix, 
                                          mode = "directed", 
                                          weighted = TRUE)
  
  # Add vertex attributes
  igraph::V(g)$name <- networks_result$markets
  igraph::V(g)$market <- networks_result$markets
  
  # Add edge attributes
  igraph::E(g)$qte <- igraph::E(g)$weight
  
  # Add graph attributes
  igraph::graph_attr(g, "scale") <- scale
  igraph::graph_attr(g, "quantile") <- quantile
  igraph::graph_attr(g, "threshold") <- networks_result$thresholds[scale_idx, quantile_idx]
  
  return(g)
}

#' Calculate Network Centrality Measures
#'
#' Computes degree, eigenvector, and betweenness centrality measures
#' for all markets across all scale-quantile combinations.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @return Data frame with centrality measures
#' @export
#' @importFrom igraph degree eigen_centrality betweenness
#' @examples
#' \dontrun{
#' centralities <- calculate_centrality(networks)
#' }
calculate_centrality <- function(networks_result) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("Input must be of class 'waveqte_networks'")
  }
  
  markets <- networks_result$markets
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  
  # Initialize result data frame
  result_df <- expand.grid(
    market = markets,
    scale = scales,
    quantile = quantiles,
    stringsAsFactors = FALSE
  )
  
  result_df$degree_in <- 0
  result_df$degree_out <- 0
  result_df$degree_total <- 0
  result_df$eigenvector <- 0
  result_df$betweenness <- 0
  result_df$closeness <- 0
  
  # Calculate centralities for each scale-quantile combination
  for (scale in scales) {
    for (quantile in quantiles) {
      
      # Construct network
      tryCatch({
        g <- construct_network(networks_result, scale, quantile)
        
        if (igraph::vcount(g) == 0 || igraph::ecount(g) == 0) {
          next
        }
        
        # Calculate centrality measures
        deg_in <- igraph::degree(g, mode = "in")
        deg_out <- igraph::degree(g, mode = "out")
        deg_total <- igraph::degree(g, mode = "total")
        eigen_cent <- igraph::eigen_centrality(g, directed = TRUE)$vector
        between_cent <- igraph::betweenness(g, directed = TRUE, normalized = TRUE)
        close_cent <- igraph::closeness(g, mode = "total", normalized = TRUE)
        
        # Store results
        idx <- result_df$scale == scale & result_df$quantile == quantile
        
        for (i in 1:length(markets)) {
          market_idx <- idx & result_df$market == markets[i]
          result_df$degree_in[market_idx] <- deg_in[i]
          result_df$degree_out[market_idx] <- deg_out[i]
          result_df$degree_total[market_idx] <- deg_total[i]
          result_df$eigenvector[market_idx] <- eigen_cent[i]
          result_df$betweenness[market_idx] <- between_cent[i]
          result_df$closeness[market_idx] <- close_cent[i]
        }
        
      }, error = function(e) {
        warning(paste("Error calculating centrality for scale", scale, 
                     "quantile", quantile, ":", e$message))
      })
    }
  }
  
  # Calculate composite centrality score
  result_df$composite_centrality <- 0.3 * scale(result_df$degree_total) + 
                                   0.3 * scale(result_df$eigenvector) + 
                                   0.4 * scale(result_df$betweenness)
  
  class(result_df) <- c("waveqte_centrality", "data.frame")
  return(result_df)
}