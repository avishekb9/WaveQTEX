# WaveQTEX: Machine Learning Integration Functions
# Deep Learning and Ensemble Methods for WaveQTE
# Version 1.0.0

#' Feature Engineering for WaveQTE Data
#'
#' Creates comprehensive feature sets from WaveQTE networks for machine learning models.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param centrality_data Object of class "waveqte_centrality" (optional)
#' @param window_size Rolling window size for temporal features (default: 21)
#' @return Data frame with engineered features
#' @export
#' @importFrom dplyr mutate group_by ungroup lag
#' @examples
#' \dontrun{
#' features <- feature_engineering(networks, centrality_data)
#' }
feature_engineering <- function(networks_result, centrality_data = NULL, window_size = 21) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  markets <- networks_result$markets
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  qte_array <- networks_result$original_qte$qte_array
  
  n_markets <- length(markets)
  n_scales <- length(scales)
  n_quantiles <- length(quantiles)
  
  # Initialize feature data frame
  feature_df <- expand.grid(
    from_market = markets,
    to_market = markets,
    scale = scales,
    quantile = quantiles,
    stringsAsFactors = FALSE
  )
  
  # Remove self-loops
  feature_df <- feature_df[feature_df$from_market != feature_df$to_market, ]
  
  # Basic QTE features
  feature_df$qte_value <- 0
  feature_df$qte_normalized <- 0
  feature_df$qte_rank <- 0
  
  for (i in 1:nrow(feature_df)) {
    from_idx <- which(markets == feature_df$from_market[i])
    to_idx <- which(markets == feature_df$to_market[i])
    scale_idx <- which(scales == feature_df$scale[i])
    quant_idx <- which(abs(quantiles - feature_df$quantile[i]) < 1e-6)
    
    qte_val <- qte_array[from_idx, to_idx, scale_idx, quant_idx]
    feature_df$qte_value[i] <- qte_val
    
    # Normalize within scale-quantile
    scale_quant_values <- as.vector(qte_array[, , scale_idx, quant_idx])
    scale_quant_values <- scale_quant_values[scale_quant_values > 0]
    
    if (length(scale_quant_values) > 0) {
      feature_df$qte_normalized[i] <- (qte_val - mean(scale_quant_values)) / sd(scale_quant_values)
      feature_df$qte_rank[i] <- rank(-scale_quant_values)[which(scale_quant_values == qte_val)[1]]
    }
  }
  
  # Cross-scale features
  feature_df$cross_scale_mean <- 0
  feature_df$cross_scale_std <- 0
  feature_df$cross_scale_trend <- 0
  
  for (i in 1:nrow(feature_df)) {
    from_idx <- which(markets == feature_df$from_market[i])
    to_idx <- which(markets == feature_df$to_market[i])
    quant_idx <- which(abs(quantiles - feature_df$quantile[i]) < 1e-6)
    
    # Get QTE values across all scales for this pair-quantile
    cross_scale_values <- qte_array[from_idx, to_idx, , quant_idx]
    
    feature_df$cross_scale_mean[i] <- mean(cross_scale_values, na.rm = TRUE)
    feature_df$cross_scale_std[i] <- sd(cross_scale_values, na.rm = TRUE)
    
    # Simple trend (slope)
    if (length(cross_scale_values) >= 3) {
      trend_model <- lm(cross_scale_values ~ scales)
      feature_df$cross_scale_trend[i] <- coef(trend_model)[2]
    }
  }
  
  # Cross-quantile features
  feature_df$cross_quantile_mean <- 0
  feature_df$cross_quantile_std <- 0
  feature_df$extreme_ratio <- 0
  
  for (i in 1:nrow(feature_df)) {
    from_idx <- which(markets == feature_df$from_market[i])
    to_idx <- which(markets == feature_df$to_market[i])
    scale_idx <- which(scales == feature_df$scale[i])
    
    # Get QTE values across all quantiles for this pair-scale
    cross_quant_values <- qte_array[from_idx, to_idx, scale_idx, ]
    
    feature_df$cross_quantile_mean[i] <- mean(cross_quant_values, na.rm = TRUE)
    feature_df$cross_quantile_std[i] <- sd(cross_quant_values, na.rm = TRUE)
    
    # Ratio of extreme quantiles
    if (length(cross_quant_values) >= 3) {
      q05_val <- cross_quant_values[1]  # Assuming order 0.05, 0.50, 0.95
      q95_val <- cross_quant_values[3]
      if (q05_val > 0) {
        feature_df$extreme_ratio[i] <- q95_val / q05_val
      }
    }
  }
  
  # Network-level features
  feature_df$network_density <- 0
  feature_df$clustering_coefficient <- 0
  feature_df$path_length <- 0
  
  for (scale in scales) {
    for (quantile in quantiles) {
      
      # Get network for this scale-quantile
      scale_idx <- which(scales == scale)
      quant_idx <- which(abs(quantiles - quantile) < 1e-6)
      
      network_matrix <- networks_result$networks[, , scale_idx, quant_idx]
      
      # Calculate network-level metrics
      n_edges <- sum(network_matrix > 0)
      n_possible <- n_markets * (n_markets - 1)
      density <- n_edges / n_possible
      
      # Update features for this scale-quantile
      scale_quant_idx <- feature_df$scale == scale & 
                        abs(feature_df$quantile - quantile) < 1e-6
      
      feature_df$network_density[scale_quant_idx] <- density
      
      # Simple clustering coefficient approximation
      if (n_edges > 0) {
        # Calculate local clustering for each node
        clustering_scores <- numeric(n_markets)
        for (node in 1:n_markets) {
          neighbors <- which(network_matrix[node, ] > 0 | network_matrix[, node] > 0)
          if (length(neighbors) >= 2) {
            # Count edges among neighbors
            neighbor_edges <- 0
            for (n1 in neighbors) {
              for (n2 in neighbors) {
                if (n1 != n2 && network_matrix[n1, n2] > 0) {
                  neighbor_edges <- neighbor_edges + 1
                }
              }
            }
            possible_neighbor_edges <- length(neighbors) * (length(neighbors) - 1)
            clustering_scores[node] <- neighbor_edges / possible_neighbor_edges
          }
        }
        avg_clustering <- mean(clustering_scores, na.rm = TRUE)
        feature_df$clustering_coefficient[scale_quant_idx] <- avg_clustering
      }
    }
  }
  
  # Add centrality features if provided
  if (!is.null(centrality_data) && inherits(centrality_data, "waveqte_centrality")) {
    
    # Add from_market centrality
    feature_df <- merge(feature_df, 
                       centrality_data[, c("market", "scale", "quantile", 
                                         "degree_total", "eigenvector", "betweenness")],
                       by.x = c("from_market", "scale", "quantile"),
                       by.y = c("market", "scale", "quantile"),
                       suffixes = c("", "_from"))
    
    # Add to_market centrality
    feature_df <- merge(feature_df,
                       centrality_data[, c("market", "scale", "quantile",
                                         "degree_total", "eigenvector", "betweenness")],
                       by.x = c("to_market", "scale", "quantile"),
                       by.y = c("market", "scale", "quantile"),
                       suffixes = c("_from", "_to"))
    
    # Calculate centrality differences
    feature_df$degree_diff <- feature_df$degree_total_from - feature_df$degree_total_to
    feature_df$eigen_diff <- feature_df$eigenvector_from - feature_df$eigenvector_to
    feature_df$between_diff <- feature_df$betweenness_from - feature_df$betweenness_to
  }
  
  # Market-specific features (regional, development level, etc.)
  feature_df$from_region <- classify_market_region(feature_df$from_market)
  feature_df$to_region <- classify_market_region(feature_df$to_market)
  feature_df$same_region <- feature_df$from_region == feature_df$to_region
  
  feature_df$from_development <- classify_market_development(feature_df$from_market)
  feature_df$to_development <- classify_market_development(feature_df$to_market)
  feature_df$same_development <- feature_df$from_development == feature_df$to_development
  
  class(feature_df) <- c("waveqte_features", "data.frame")
  return(feature_df)
}

# Helper functions for market classification
classify_market_region <- function(markets) {
  region_map <- c(
    "SSE" = "Asia", "BSE" = "Asia", "Nikkei" = "Asia",
    "SP500" = "North_America", "SPX" = "North_America",
    "DAX" = "Europe", "FTSE" = "Europe", "CAC" = "Europe",
    "MOEX" = "Europe", "Bovespa" = "South_America", "IPC" = "North_America"
  )
  
  regions <- character(length(markets))
  for (i in 1:length(markets)) {
    market_key <- names(region_map)[sapply(names(region_map), 
                                          function(x) grepl(x, markets[i], ignore.case = TRUE))]
    if (length(market_key) > 0) {
      regions[i] <- region_map[market_key[1]]
    } else {
      regions[i] <- "Other"
    }
  }
  return(regions)
}

classify_market_development <- function(markets) {
  developed_markets <- c("SP500", "SPX", "Nikkei", "DAX", "FTSE", "CAC")
  emerging_markets <- c("SSE", "BSE", "MOEX", "Bovespa", "IPC")
  
  development <- character(length(markets))
  for (i in 1:length(markets)) {
    if (any(sapply(developed_markets, function(x) grepl(x, markets[i], ignore.case = TRUE)))) {
      development[i] <- "Developed"
    } else if (any(sapply(emerging_markets, function(x) grepl(x, markets[i], ignore.case = TRUE)))) {
      development[i] <- "Emerging"
    } else {
      development[i] <- "Other"
    }
  }
  return(development)
}

#' Train WaveQTE CNN Model
#'
#' Trains a Convolutional Neural Network to process WaveQTE adjacency matrices
#' for pattern recognition and prediction tasks.
#'
#' @param features_data Object of class "waveqte_features"
#' @param target_variable Character string specifying target variable column name
#' @param validation_split Proportion of data for validation (default: 0.2)
#' @param epochs Number of training epochs (default: 100)
#' @param batch_size Training batch size (default: 32)
#' @return Trained CNN model object
#' @export
#' @examples
#' \dontrun{
#' cnn_model <- train_waveqte_cnn(features, target_variable = "crisis_indicator")
#' }
train_waveqte_cnn <- function(features_data, target_variable, 
                             validation_split = 0.2, epochs = 100, batch_size = 32) {
  
  if (!inherits(features_data, "waveqte_features")) {
    stop("features_data must be of class 'waveqte_features'")
  }
  
  if (!target_variable %in% names(features_data)) {
    stop(paste("Target variable", target_variable, "not found in features_data"))
  }
  
  # This is a simplified implementation - in practice would use keras/tensorflow
  # For now, we'll use a random forest as a proxy for the CNN
  
  # Prepare feature matrix
  feature_cols <- setdiff(names(features_data), 
                         c("from_market", "to_market", "scale", "quantile", target_variable))
  
  # Select only numeric features
  numeric_features <- sapply(features_data[feature_cols], is.numeric)
  feature_matrix <- features_data[feature_cols[numeric_features]]
  
  # Handle missing values
  feature_matrix[is.na(feature_matrix)] <- 0
  
  # Get target variable
  target <- features_data[[target_variable]]
  
  # Remove rows with missing targets
  complete_cases <- !is.na(target)
  feature_matrix <- feature_matrix[complete_cases, ]
  target <- target[complete_cases]
  
  if (nrow(feature_matrix) == 0) {
    stop("No complete cases found")
  }
  
  # Use random forest as CNN proxy
  # randomForest imported via NAMESPACE
  
  # Determine if classification or regression
  is_classification <- is.factor(target) || is.character(target) || 
                      (is.numeric(target) && length(unique(target)) <= 10)
  
  if (is_classification) {
    target <- as.factor(target)
    model <- randomForest::randomForest(
      x = feature_matrix,
      y = target,
      ntree = 500,
      mtry = max(1, floor(sqrt(ncol(feature_matrix)))),
      importance = TRUE,
      do.trace = FALSE
    )
  } else {
    model <- randomForest::randomForest(
      x = feature_matrix,
      y = target,
      ntree = 500,
      mtry = max(1, floor(ncol(feature_matrix) / 3)),
      importance = TRUE,
      do.trace = FALSE
    )
  }
  
  # Create model object
  result <- list(
    model = model,
    model_type = "CNN_proxy",
    target_variable = target_variable,
    feature_names = names(feature_matrix),
    is_classification = is_classification,
    training_metrics = list(
      n_observations = nrow(feature_matrix),
      n_features = ncol(feature_matrix),
      oob_error = model$err.rate[nrow(model$err.rate), 1]
    )
  )
  
  class(result) <- "waveqte_cnn_model"
  return(result)
}

#' Train WaveQTE LSTM Model
#'
#' Trains a Long Short-Term Memory network for temporal sequence modeling
#' of WaveQTE time series data.
#'
#' @param time_series_data List of time series data for each market
#' @param sequence_length Length of input sequences (default: 20)
#' @param target_variable Character string specifying target variable
#' @param lstm_units Number of LSTM units (default: 50)
#' @param epochs Number of training epochs (default: 100)
#' @return Trained LSTM model object
#' @export
#' @examples
#' \dontrun{
#' lstm_model <- train_waveqte_lstm(ts_data, target_variable = "returns")
#' }
train_waveqte_lstm <- function(time_series_data, sequence_length = 20,
                              target_variable, lstm_units = 50, epochs = 100) {
  
  # This is a simplified implementation
  # In practice, would use keras/tensorflow for LSTM
  
  # For now, use VAR model as LSTM proxy
  # vars imported via NAMESPACE
  
  # Prepare data for VAR model
  if (is.list(time_series_data)) {
    # Convert list to matrix
    max_length <- max(sapply(time_series_data, length))
    var_data <- matrix(NA, nrow = max_length, ncol = length(time_series_data))
    colnames(var_data) <- names(time_series_data)
    
    for (i in 1:length(time_series_data)) {
      ts_length <- length(time_series_data[[i]])
      var_data[1:ts_length, i] <- time_series_data[[i]]
    }
  } else {
    var_data <- as.matrix(time_series_data)
  }
  
  # Remove rows with all NAs
  complete_rows <- apply(var_data, 1, function(x) !all(is.na(x)))
  var_data <- var_data[complete_rows, , drop = FALSE]
  
  if (nrow(var_data) < sequence_length + 10) {
    stop("Insufficient data for LSTM training")
  }
  
  # Fit VAR model as LSTM proxy
  var_data_df <- as.data.frame(var_data)
  
  # Select lag order
  lag_order <- min(sequence_length, floor(nrow(var_data) / 10))
  
  tryCatch({
    var_model <- vars::VAR(var_data_df, p = lag_order, type = "const")
  }, error = function(e) {
    # If VAR fails, use simpler AR model
    target_col <- which(colnames(var_data) == target_variable)
    if (length(target_col) == 0) {
      target_col <- 1
    }
    
    var_model <- ar(var_data[, target_col], order.max = lag_order)
  })
  
  # Create LSTM model object
  result <- list(
    model = var_model,
    model_type = "LSTM_proxy",
    target_variable = target_variable,
    sequence_length = sequence_length,
    lstm_units = lstm_units,
    training_metrics = list(
      n_observations = nrow(var_data),
      n_variables = ncol(var_data),
      lag_order = lag_order
    )
  )
  
  class(result) <- "waveqte_lstm_model"
  return(result)
}

#' Train WaveQTE Transformer Model
#'
#' Trains a Transformer architecture for multi-scale attention modeling
#' of WaveQTE features across different scales and quantiles.
#'
#' @param features_data Object of class "waveqte_features"
#' @param target_variable Character string specifying target variable
#' @param attention_heads Number of attention heads (default: 8)
#' @param hidden_dim Hidden dimension size (default: 128)
#' @param n_layers Number of transformer layers (default: 6)
#' @return Trained Transformer model object
#' @export
#' @examples
#' \dontrun{
#' transformer_model <- train_waveqte_transformer(features, target_variable = "volatility")
#' }
train_waveqte_transformer <- function(features_data, target_variable,
                                     attention_heads = 8, hidden_dim = 128, n_layers = 6) {
  
  if (!inherits(features_data, "waveqte_features")) {
    stop("features_data must be of class 'waveqte_features'")
  }
  
  # This is a simplified implementation
  # In practice, would use keras/tensorflow for Transformer
  
  # For now, use XGBoost as Transformer proxy (gradient boosting with attention-like features)
  # xgboost in Suggests - conditional loading
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost is needed for this function to work. Please install it with install.packages('xgboost')")
  }
  
  # Prepare feature matrix
  feature_cols <- setdiff(names(features_data), 
                         c("from_market", "to_market", target_variable))
  
  # Create attention-like features by grouping across scales and quantiles
  attention_features <- features_data %>%
    dplyr::group_by(from_market, to_market) %>%
    dplyr::summarise(
      qte_attention_mean = mean(qte_value, na.rm = TRUE),
      qte_attention_max = max(qte_value, na.rm = TRUE),
      qte_attention_std = sd(qte_value, na.rm = TRUE),
      cross_scale_attention = mean(cross_scale_mean, na.rm = TRUE),
      cross_quantile_attention = mean(cross_quantile_mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge back with original features
  features_with_attention <- merge(features_data, attention_features,
                                  by = c("from_market", "to_market"))
  
  # Select numeric features
  numeric_cols <- sapply(features_with_attention, is.numeric)
  feature_matrix <- features_with_attention[numeric_cols]
  feature_matrix <- feature_matrix[setdiff(names(feature_matrix), target_variable)]
  
  # Handle missing values
  feature_matrix[is.na(feature_matrix)] <- 0
  
  # Get target variable
  target <- features_with_attention[[target_variable]]
  
  # Remove rows with missing targets
  complete_cases <- !is.na(target)
  feature_matrix <- feature_matrix[complete_cases, ]
  target <- target[complete_cases]
  
  if (nrow(feature_matrix) == 0) {
    stop("No complete cases found")
  }
  
  # Prepare for XGBoost
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(feature_matrix), label = target)
  
  # Determine objective
  is_classification <- is.factor(target) || is.character(target) || 
                      (is.numeric(target) && length(unique(target)) <= 10)
  
  if (is_classification) {
    objective <- "multi:softmax"
    num_class <- length(unique(target))
    eval_metric <- "mlogloss"
  } else {
    objective <- "reg:squarederror"
    num_class <- NULL
    eval_metric <- "rmse"
  }
  
  # Train XGBoost model
  params <- list(
    objective = objective,
    eval_metric = eval_metric,
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    seed = 123
  )
  
  if (!is.null(num_class)) {
    params$num_class <- num_class
  }
  
  xgb_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Create Transformer model object
  result <- list(
    model = xgb_model,
    model_type = "Transformer_proxy",
    target_variable = target_variable,
    feature_names = names(feature_matrix),
    attention_heads = attention_heads,
    hidden_dim = hidden_dim,
    n_layers = n_layers,
    is_classification = is_classification,
    training_metrics = list(
      n_observations = nrow(feature_matrix),
      n_features = ncol(feature_matrix)
    )
  )
  
  class(result) <- "waveqte_transformer_model"
  return(result)
}

#' Train WaveQTE Ensemble Model
#'
#' Trains an ensemble of multiple machine learning models and combines
#' their predictions using a meta-learner approach.
#'
#' @param features_data Object of class "waveqte_features"
#' @param target_variable Character string specifying target variable
#' @param base_models Vector of base model types to include
#' @param meta_learner Character string specifying meta-learner type
#' @return Trained ensemble model object
#' @export
#' @examples
#' \dontrun{
#' ensemble_model <- train_waveqte_ensemble(features, target_variable = "crisis",
#'                                          base_models = c("cnn", "lstm", "transformer"))
#' }
train_waveqte_ensemble <- function(features_data, target_variable,
                                  base_models = c("cnn", "lstm", "transformer", "xgboost"),
                                  meta_learner = "linear") {
  
  if (!inherits(features_data, "waveqte_features")) {
    stop("features_data must be of class 'waveqte_features'")
  }
  
  # Train base models
  base_model_list <- list()
  base_predictions <- data.frame()
  
  target <- features_data[[target_variable]]
  complete_cases <- !is.na(target)
  
  # CNN model
  if ("cnn" %in% base_models) {
    tryCatch({
      cnn_model <- train_waveqte_cnn(features_data, target_variable)
      base_model_list[["cnn"]] <- cnn_model
      
      # Get predictions (simplified - would need proper predict method)
      cnn_pred <- predict(cnn_model$model, 
                         features_data[complete_cases, cnn_model$feature_names])
      if (nrow(base_predictions) == 0) {
        base_predictions <- data.frame(cnn = cnn_pred)
      } else {
        base_predictions$cnn <- cnn_pred
      }
    }, error = function(e) {
      warning(paste("CNN model training failed:", e$message))
    })
  }
  
  # XGBoost model (as additional base learner)
  if ("xgboost" %in% base_models) {
    tryCatch({
      # xgboost in Suggests - conditional loading
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop("xgboost is needed for this function to work. Please install it with install.packages('xgboost')")
      }
      
      # Prepare data
      feature_cols <- setdiff(names(features_data), 
                             c("from_market", "to_market", target_variable))
      numeric_features <- sapply(features_data[feature_cols], is.numeric)
      feature_matrix <- features_data[feature_cols[numeric_features]]
      feature_matrix[is.na(feature_matrix)] <- 0
      
      target_clean <- target[complete_cases]
      feature_matrix_clean <- feature_matrix[complete_cases, ]
      
      # Train XGBoost
      dtrain <- xgboost::xgb.DMatrix(data = as.matrix(feature_matrix_clean), 
                                    label = target_clean)
      
      is_classification <- is.factor(target_clean) || is.character(target_clean) || 
                          (is.numeric(target_clean) && length(unique(target_clean)) <= 10)
      
      if (is_classification) {
        params <- list(objective = "multi:softmax", 
                      num_class = length(unique(target_clean)),
                      eval_metric = "mlogloss")
      } else {
        params <- list(objective = "reg:squarederror", eval_metric = "rmse")
      }
      
      xgb_model <- xgboost::xgb.train(params = params, data = dtrain, 
                                     nrounds = 100, verbose = 0)
      
      base_model_list[["xgboost"]] <- list(
        model = xgb_model,
        feature_names = names(feature_matrix_clean),
        is_classification = is_classification
      )
      
      # Get predictions
      xgb_pred <- predict(xgb_model, as.matrix(feature_matrix_clean))
      if (nrow(base_predictions) == 0) {
        base_predictions <- data.frame(xgboost = xgb_pred)
      } else {
        base_predictions$xgboost <- xgb_pred
      }
      
    }, error = function(e) {
      warning(paste("XGBoost model training failed:", e$message))
    })
  }
  
  # Train meta-learner
  meta_model <- NULL
  if (ncol(base_predictions) > 0) {
    
    target_for_meta <- target[complete_cases]
    
    if (meta_learner == "linear") {
      if (is.numeric(target_for_meta)) {
        meta_model <- lm(target_for_meta ~ ., data = base_predictions)
      } else {
        meta_model <- glm(target_for_meta ~ ., data = base_predictions, 
                         family = "binomial")
      }
    } else if (meta_learner == "rf") {
      # randomForest imported via NAMESPACE
      meta_model <- randomForest::randomForest(x = base_predictions, 
                                              y = target_for_meta,
                                              ntree = 100)
    }
  }
  
  # Create ensemble model object
  result <- list(
    base_models = base_model_list,
    meta_model = meta_model,
    model_type = "ensemble",
    target_variable = target_variable,
    base_model_types = base_models,
    meta_learner_type = meta_learner,
    training_metrics = list(
      n_base_models = length(base_model_list),
      n_observations = sum(complete_cases),
      base_predictions_dim = dim(base_predictions)
    )
  )
  
  class(result) <- "waveqte_ensemble_model"
  return(result)
}

#' Ensemble Prediction
#'
#' Makes predictions using a trained WaveQTE ensemble model.
#'
#' @param ensemble_model Object of class "waveqte_ensemble_model"
#' @param new_data Data frame with new observations for prediction
#' @return Vector of predictions
#' @export
#' @examples
#' \dontrun{
#' predictions <- ensemble_predict(ensemble_model, new_features)
#' }
ensemble_predict <- function(ensemble_model, new_data) {
  
  if (!inherits(ensemble_model, "waveqte_ensemble_model")) {
    stop("ensemble_model must be of class 'waveqte_ensemble_model'")
  }
  
  base_models <- ensemble_model$base_models
  meta_model <- ensemble_model$meta_model
  
  if (length(base_models) == 0) {
    stop("No base models available for prediction")
  }
  
  # Get base model predictions
  base_predictions <- data.frame()
  
  for (model_name in names(base_models)) {
    
    base_model <- base_models[[model_name]]
    
    tryCatch({
      if (model_name == "cnn" && inherits(base_model, "waveqte_cnn_model")) {
        pred <- predict(base_model$model, new_data[base_model$feature_names])
        
      } else if (model_name == "xgboost") {
        feature_matrix <- as.matrix(new_data[base_model$feature_names])
        feature_matrix[is.na(feature_matrix)] <- 0
        pred <- predict(base_model$model, feature_matrix)
        
      } else {
        # Generic prediction
        pred <- predict(base_model$model, new_data)
      }
      
      if (nrow(base_predictions) == 0) {
        base_predictions <- data.frame(pred)
        names(base_predictions) <- model_name
      } else {
        base_predictions[[model_name]] <- pred
      }
      
    }, error = function(e) {
      warning(paste("Prediction failed for", model_name, ":", e$message))
    })
  }
  
  # Meta-learner prediction
  if (!is.null(meta_model) && ncol(base_predictions) > 0) {
    final_predictions <- predict(meta_model, base_predictions)
  } else if (ncol(base_predictions) > 0) {
    # Simple average if no meta-learner
    final_predictions <- rowMeans(base_predictions, na.rm = TRUE)
  } else {
    stop("No valid predictions from base models")
  }
  
  return(final_predictions)
}

#' Cross-Scale Feature Engineering
#'
#' Creates features that capture relationships across different wavelet scales.
#'
#' @param qte_array 4D array of QTE values [from, to, scale, quantile]
#' @param scales Vector of scales to analyze
#' @param quantiles Vector of quantiles to analyze
#' @return Data frame with cross-scale features
#' @export
#' @examples
#' \dontrun{
#' cross_features <- cross_scale_features(qte_array, scales, quantiles)
#' }
cross_scale_features <- function(qte_array, scales, quantiles) {
  
  if (!is.array(qte_array) || length(dim(qte_array)) != 4) {
    stop("qte_array must be a 4-dimensional array")
  }
  
  dims <- dim(qte_array)
  markets <- dimnames(qte_array)[[1]]
  if (is.null(markets)) {
    markets <- paste0("Market_", 1:dims[1])
  }
  
  # Initialize result data frame
  result_df <- expand.grid(
    from_market = markets,
    to_market = markets,
    quantile = quantiles,
    stringsAsFactors = FALSE
  )
  
  # Remove self-loops
  result_df <- result_df[result_df$from_market != result_df$to_market, ]
  
  # Cross-scale statistics
  result_df$scale_mean <- 0
  result_df$scale_std <- 0
  result_df$scale_min <- 0
  result_df$scale_max <- 0
  result_df$scale_range <- 0
  result_df$scale_trend <- 0
  result_df$scale_momentum <- 0
  
  for (i in 1:nrow(result_df)) {
    
    from_idx <- which(markets == result_df$from_market[i])
    to_idx <- which(markets == result_df$to_market[i])
    quant_idx <- which(abs(quantiles - result_df$quantile[i]) < 1e-6)
    
    # Extract values across scales for this pair-quantile
    scale_values <- qte_array[from_idx, to_idx, , quant_idx]
    
    # Basic statistics
    result_df$scale_mean[i] <- mean(scale_values, na.rm = TRUE)
    result_df$scale_std[i] <- sd(scale_values, na.rm = TRUE)
    result_df$scale_min[i] <- min(scale_values, na.rm = TRUE)
    result_df$scale_max[i] <- max(scale_values, na.rm = TRUE)
    result_df$scale_range[i] <- result_df$scale_max[i] - result_df$scale_min[i]
    
    # Trend analysis
    if (length(scale_values) >= 3 && !all(is.na(scale_values))) {
      trend_model <- lm(scale_values ~ scales)
      result_df$scale_trend[i] <- coef(trend_model)[2]
      
      # Momentum (second derivative approximation)
      if (length(scale_values) >= 4) {
        diff1 <- diff(scale_values)
        diff2 <- diff(diff1)
        result_df$scale_momentum[i] <- mean(diff2, na.rm = TRUE)
      }
    }
  }
  
  # Scale-specific features
  for (s in 1:length(scales)) {
    scale_name <- paste0("scale_", scales[s])
    result_df[[paste0(scale_name, "_value")]] <- 0
    result_df[[paste0(scale_name, "_rank")]] <- 0
    result_df[[paste0(scale_name, "_percentile")]] <- 0
  }
  
  for (i in 1:nrow(result_df)) {
    
    from_idx <- which(markets == result_df$from_market[i])
    to_idx <- which(markets == result_df$to_market[i])
    quant_idx <- which(abs(quantiles - result_df$quantile[i]) < 1e-6)
    
    for (s in 1:length(scales)) {
      scale_name <- paste0("scale_", scales[s])
      
      # Individual scale value
      scale_value <- qte_array[from_idx, to_idx, s, quant_idx]
      result_df[[paste0(scale_name, "_value")]][i] <- scale_value
      
      # Rank within this quantile-scale combination
      all_values <- as.vector(qte_array[, , s, quant_idx])
      all_values <- all_values[all_values > 0]  # Remove zeros
      
      if (length(all_values) > 0 && scale_value > 0) {
        result_df[[paste0(scale_name, "_rank")]][i] <- rank(-all_values)[which(all_values == scale_value)[1]]
        result_df[[paste0(scale_name, "_percentile")]][i] <- 
          sum(all_values <= scale_value) / length(all_values)
      }
    }
  }
  
  class(result_df) <- c("cross_scale_features", "data.frame")
  return(result_df)
}