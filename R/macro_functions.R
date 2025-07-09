# WaveQTEX: Macroprudential Applications
# Real-time Systemic Risk Monitoring and Early Warning Systems
# Version 1.0.0

#' Calculate Systemic Risk Index
#'
#' Computes a comprehensive systemic risk index based on WaveQTE networks
#' across multiple scales and quantiles with dynamic weighting.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param scale_weights Named vector of weights for different scales
#' @param quantile_weights Named vector of weights for different quantiles
#' @param time_varying Logical indicating whether to calculate time-varying SRI
#' @return Numeric value or time series of systemic risk index
#' @export
#' @examples
#' \dontrun{
#' sri <- calculate_systemic_risk_index(networks, 
#'                                     scale_weights = c(1, 1, 2, 2, 3, 3),
#'                                     quantile_weights = c(3, 1, 3))
#' }
calculate_systemic_risk_index <- function(networks_result, 
                                         scale_weights = NULL,
                                         quantile_weights = NULL,
                                         time_varying = FALSE) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  qte_array <- networks_result$original_qte$qte_array
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  markets <- networks_result$markets
  
  n_markets <- length(markets)
  n_scales <- length(scales)
  n_quantiles <- length(quantiles)
  
  # Default weights if not provided
  if (is.null(scale_weights)) {
    # Higher weights for longer-term scales
    scale_weights <- scales / sum(scales)
  }
  
  if (is.null(quantile_weights)) {
    # Higher weights for extreme quantiles
    quantile_weights <- c(3, 1, 3)  # 0.05, 0.50, 0.95
    quantile_weights <- quantile_weights / sum(quantile_weights)
  }
  
  # Ensure weights are normalized
  scale_weights <- scale_weights / sum(scale_weights)
  quantile_weights <- quantile_weights / sum(quantile_weights)
  
  # Calculate weighted average QTE across all connections
  total_sri <- 0
  n_connections <- n_markets * (n_markets - 1)  # Exclude self-loops
  
  for (s in 1:n_scales) {
    for (q in 1:n_quantiles) {
      
      # Get QTE matrix for this scale-quantile
      qte_matrix <- qte_array[, , s, q]
      diag(qte_matrix) <- 0  # Remove self-loops
      
      # Calculate average QTE for this scale-quantile
      avg_qte <- sum(qte_matrix) / n_connections
      
      # Add weighted contribution to SRI
      weight <- scale_weights[s] * quantile_weights[q]
      total_sri <- total_sri + weight * avg_qte
    }
  }
  
  # Create SRI result object
  result <- list(
    sri_value = total_sri,
    scale_weights = scale_weights,
    quantile_weights = quantile_weights,
    n_markets = n_markets,
    calculation_date = Sys.Date(),
    components = list()
  )
  
  # Calculate component contributions
  for (s in 1:n_scales) {
    for (q in 1:n_quantiles) {
      qte_matrix <- qte_array[, , s, q]
      diag(qte_matrix) <- 0
      avg_qte <- sum(qte_matrix) / n_connections
      weight <- scale_weights[s] * quantile_weights[q]
      
      component_name <- paste0("scale_", scales[s], "_q_", quantiles[q])
      result$components[[component_name]] <- list(
        value = avg_qte,
        weight = weight,
        contribution = weight * avg_qte
      )
    }
  }
  
  # Alert levels
  result$alert_level <- determine_alert_level(total_sri)
  
  class(result) <- "systemic_risk_index"
  return(result)
}

#' Determine Alert Level
#'
#' Internal function to determine alert level based on SRI value
#' @param sri_value Numeric systemic risk index value
#' @return Character string indicating alert level
determine_alert_level <- function(sri_value) {
  
  # These thresholds should be calibrated based on historical data
  # For now, using reasonable defaults
  
  if (sri_value < 0.5) {
    return("Green")
  } else if (sri_value < 1.5) {
    return("Amber")
  } else {
    return("Red")
  }
}

#' Create Early Warning System
#'
#' Establishes a comprehensive early warning system for financial crises
#' based on WaveQTE analysis and machine learning models.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param historical_data Historical data for model training
#' @param crisis_indicators Vector of known crisis periods for validation
#' @param prediction_horizon Number of periods ahead to predict (default: 60)
#' @param confidence_threshold Threshold for crisis probability (default: 0.7)
#' @return Object of class "early_warning_system"
#' @export
#' @examples
#' \dontrun{
#' ews <- create_early_warning_system(networks, historical_data, crisis_dates)
#' }
create_early_warning_system <- function(networks_result, historical_data, 
                                       crisis_indicators = NULL,
                                       prediction_horizon = 60,
                                       confidence_threshold = 0.7) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  # Calculate base indicators
  sri <- calculate_systemic_risk_index(networks_result)
  centrality_data <- calculate_centrality(networks_result)
  
  # Prepare feature matrix for EWS model
  features <- prepare_ews_features(networks_result, centrality_data, historical_data)
  
  # If crisis indicators are provided, train a prediction model
  prediction_model <- NULL
  if (!is.null(crisis_indicators)) {
    prediction_model <- train_crisis_prediction_model(features, crisis_indicators)
  }
  
  # Calculate risk components
  risk_components <- calculate_risk_components(networks_result, centrality_data)
  
  # Generate current assessment
  current_assessment <- assess_current_risk(sri, risk_components, prediction_model)
  
  # Create EWS object
  ews <- list(
    sri = sri,
    risk_components = risk_components,
    current_assessment = current_assessment,
    prediction_model = prediction_model,
    features = features,
    thresholds = list(
      sri_amber = 0.5,
      sri_red = 1.5,
      confidence_threshold = confidence_threshold
    ),
    parameters = list(
      prediction_horizon = prediction_horizon,
      n_markets = length(networks_result$markets),
      scales = networks_result$scales,
      quantiles = networks_result$quantiles
    ),
    creation_date = Sys.time()
  )
  
  class(ews) <- "early_warning_system"
  return(ews)
}

#' Prepare Features for Early Warning System
#'
#' Internal function to prepare comprehensive features for crisis prediction
#' @param networks_result WaveQTE networks
#' @param centrality_data Network centrality measures
#' @param historical_data Historical market data
#' @return Data frame with EWS features
prepare_ews_features <- function(networks_result, centrality_data, historical_data) {
  
  qte_array <- networks_result$original_qte$qte_array
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  markets <- networks_result$markets
  
  # Initialize feature data frame
  features <- data.frame(
    date = as.Date(Sys.Date()),  # Placeholder - would use actual dates
    stringsAsFactors = FALSE
  )
  
  # Network-level features
  for (s in 1:length(scales)) {
    for (q in 1:length(quantiles)) {
      
      scale_name <- paste0("scale_", scales[s])
      quant_name <- paste0("q_", quantiles[q])
      
      # Network density
      network_matrix <- networks_result$networks[, , s, q]
      n_edges <- sum(network_matrix > 0)
      n_possible <- length(markets) * (length(markets) - 1)
      density <- n_edges / n_possible
      
      features[[paste0("density_", scale_name, "_", quant_name)]] <- density
      
      # Average QTE
      qte_matrix <- qte_array[, , s, q]
      diag(qte_matrix) <- 0
      avg_qte <- mean(qte_matrix[qte_matrix > 0], na.rm = TRUE)
      features[[paste0("avg_qte_", scale_name, "_", quant_name)]] <- avg_qte
      
      # Maximum QTE
      max_qte <- max(qte_matrix, na.rm = TRUE)
      features[[paste0("max_qte_", scale_name, "_", quant_name)]] <- max_qte
      
      # QTE concentration (Gini coefficient approximation)
      qte_values <- qte_matrix[qte_matrix > 0]
      if (length(qte_values) > 1) {
        gini_approx <- (2 * sum(rank(qte_values) * qte_values)) / 
                      (length(qte_values) * sum(qte_values)) - 
                      (length(qte_values) + 1) / length(qte_values)
        features[[paste0("concentration_", scale_name, "_", quant_name)]] <- gini_approx
      } else {
        features[[paste0("concentration_", scale_name, "_", quant_name)]] <- 0
      }
    }
  }
  
  # Centrality-based features
  if (!is.null(centrality_data)) {
    
    # Average centralities across all scales and quantiles
    avg_centrality <- centrality_data %>%
      dplyr::group_by(market) %>%
      dplyr::summarise(
        avg_degree = mean(degree_total, na.rm = TRUE),
        avg_eigen = mean(eigenvector, na.rm = TRUE),
        avg_between = mean(betweenness, na.rm = TRUE),
        .groups = "drop"
      )
    
    # System-wide centrality statistics
    features$max_degree <- max(avg_centrality$avg_degree, na.rm = TRUE)
    features$mean_degree <- mean(avg_centrality$avg_degree, na.rm = TRUE)
    features$std_degree <- sd(avg_centrality$avg_degree, na.rm = TRUE)
    
    features$max_eigen <- max(avg_centrality$avg_eigen, na.rm = TRUE)
    features$mean_eigen <- mean(avg_centrality$avg_eigen, na.rm = TRUE)
    
    features$max_between <- max(avg_centrality$avg_between, na.rm = TRUE)
    features$mean_between <- mean(avg_centrality$avg_between, na.rm = TRUE)
    
    # Centrality concentration
    features$degree_concentration <- sd(avg_centrality$avg_degree) / mean(avg_centrality$avg_degree)
    features$eigen_concentration <- sd(avg_centrality$avg_eigen) / (mean(avg_centrality$avg_eigen) + 1e-6)
  }
  
  # Cross-scale features
  for (q in 1:length(quantiles)) {
    quant_name <- paste0("q_", quantiles[q])
    
    # QTE evolution across scales
    scale_qte_values <- numeric(length(scales))
    for (s in 1:length(scales)) {
      qte_matrix <- qte_array[, , s, q]
      diag(qte_matrix) <- 0
      scale_qte_values[s] <- mean(qte_matrix[qte_matrix > 0], na.rm = TRUE)
    }
    
    # Trend across scales
    if (length(scale_qte_values) >= 3 && !all(is.na(scale_qte_values))) {
      trend_model <- lm(scale_qte_values ~ scales)
      features[[paste0("scale_trend_", quant_name)]] <- coef(trend_model)[2]
    } else {
      features[[paste0("scale_trend_", quant_name)]] <- 0
    }
    
    # Volatility across scales
    features[[paste0("scale_volatility_", quant_name)]] <- sd(scale_qte_values, na.rm = TRUE)
  }
  
  # Regional features (if market classifications available)
  if (length(markets) >= 3) {
    
    # Calculate regional connectivity
    regions <- classify_market_region(markets)
    unique_regions <- unique(regions)
    
    for (region in unique_regions) {
      region_markets <- which(regions == region)
      
      if (length(region_markets) >= 2) {
        # Intra-regional connectivity
        intra_regional_qte <- numeric(0)
        
        for (s in 1:length(scales)) {
          for (q in 1:length(quantiles)) {
            qte_matrix <- qte_array[, , s, q]
            intra_qte <- qte_matrix[region_markets, region_markets]
            diag(intra_qte) <- 0
            intra_regional_qte <- c(intra_regional_qte, as.vector(intra_qte))
          }
        }
        
        intra_regional_qte <- intra_regional_qte[intra_regional_qte > 0]
        if (length(intra_regional_qte) > 0) {
          features[[paste0("intra_", region, "_mean")]] <- mean(intra_regional_qte)
          features[[paste0("intra_", region, "_max")]] <- max(intra_regional_qte)
        }
      }
    }
  }
  
  return(features)
}

#' Train Crisis Prediction Model
#'
#' Internal function to train a machine learning model for crisis prediction
#' @param features Feature matrix
#' @param crisis_indicators Binary vector of crisis periods
#' @return Trained prediction model
train_crisis_prediction_model <- function(features, crisis_indicators) {
  
  # randomForest imported via NAMESPACE
  
  # Prepare data
  numeric_features <- sapply(features, is.numeric)
  feature_matrix <- features[numeric_features]
  feature_matrix[is.na(feature_matrix)] <- 0
  
  # Ensure crisis_indicators is binary
  crisis_binary <- as.factor(ifelse(crisis_indicators > 0, "Crisis", "Normal"))
  
  # Remove rows with missing crisis indicators
  complete_cases <- !is.na(crisis_binary)
  feature_matrix <- feature_matrix[complete_cases, ]
  crisis_binary <- crisis_binary[complete_cases]
  
  if (nrow(feature_matrix) < 10) {
    warning("Insufficient data for crisis prediction model training")
    return(NULL)
  }
  
  # Train random forest model
  tryCatch({
    model <- randomForest::randomForest(
      x = feature_matrix,
      y = crisis_binary,
      ntree = 500,
      mtry = max(1, floor(sqrt(ncol(feature_matrix)))),
      importance = TRUE,
      class.weights = c("Normal" = 1, "Crisis" = 3)  # Higher weight for crisis class
    )
    
    return(list(
      model = model,
      feature_names = names(feature_matrix),
      n_observations = nrow(feature_matrix),
      n_features = ncol(feature_matrix),
      accuracy = 1 - model$err.rate[nrow(model$err.rate), "OOB"]
    ))
    
  }, error = function(e) {
    warning(paste("Crisis prediction model training failed:", e$message))
    return(NULL)
  })
}

#' Calculate Risk Components
#'
#' Internal function to calculate individual risk components
#' @param networks_result WaveQTE networks
#' @param centrality_data Network centrality measures
#' @return List of risk components
calculate_risk_components <- function(networks_result, centrality_data) {
  
  # Connectivity risk
  connectivity_risk <- calculate_connectivity_risk(networks_result)
  
  # Concentration risk
  concentration_risk <- calculate_concentration_risk(centrality_data)
  
  # Contagion risk
  contagion_risk <- calculate_contagion_risk(networks_result)
  
  # Volatility risk
  volatility_risk <- calculate_volatility_risk(networks_result)
  
  return(list(
    connectivity = connectivity_risk,
    concentration = concentration_risk,
    contagion = contagion_risk,
    volatility = volatility_risk
  ))
}

#' Calculate Connectivity Risk
#'
#' Internal function to assess connectivity-based systemic risk
#' @param networks_result WaveQTE networks
#' @return Connectivity risk score
calculate_connectivity_risk <- function(networks_result) {
  
  qte_array <- networks_result$original_qte$qte_array
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  n_markets <- length(networks_result$markets)
  
  connectivity_scores <- numeric(0)
  
  for (s in 1:length(scales)) {
    for (q in 1:length(quantiles)) {
      
      # Network density
      network_matrix <- networks_result$networks[, , s, q]
      n_edges <- sum(network_matrix > 0)
      n_possible <- n_markets * (n_markets - 1)
      density <- n_edges / n_possible
      
      # Weight by scale and quantile importance
      scale_weight <- scales[s] / sum(scales)
      quant_weight <- ifelse(q %in% c(1, 3), 0.4, 0.2)  # Higher weight for extremes
      
      connectivity_scores <- c(connectivity_scores, density * scale_weight * quant_weight)
    }
  }
  
  return(sum(connectivity_scores))
}

#' Calculate Concentration Risk
#'
#' Internal function to assess concentration-based systemic risk
#' @param centrality_data Network centrality measures
#' @return Concentration risk score
calculate_concentration_risk <- function(centrality_data) {
  
  if (is.null(centrality_data) || nrow(centrality_data) == 0) {
    return(0)
  }
  
  # Calculate concentration of centrality measures
  avg_centrality <- centrality_data %>%
    dplyr::group_by(market) %>%
    dplyr::summarise(
      avg_degree = mean(degree_total, na.rm = TRUE),
      avg_eigen = mean(eigenvector, na.rm = TRUE),
      avg_between = mean(betweenness, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Gini coefficient for centrality concentration
  degree_gini <- calculate_gini(avg_centrality$avg_degree)
  eigen_gini <- calculate_gini(avg_centrality$avg_eigen)
  between_gini <- calculate_gini(avg_centrality$avg_between)
  
  # Weighted average
  concentration_risk <- 0.4 * degree_gini + 0.3 * eigen_gini + 0.3 * between_gini
  
  return(concentration_risk)
}

#' Calculate Gini Coefficient
#'
#' Internal function to calculate Gini coefficient
#' @param x Numeric vector
#' @return Gini coefficient
calculate_gini <- function(x) {
  x <- x[!is.na(x) & x >= 0]
  if (length(x) <= 1) return(0)
  
  n <- length(x)
  x <- sort(x)
  
  gini <- (2 * sum(1:n * x)) / (n * sum(x)) - (n + 1) / n
  
  return(max(0, min(1, gini)))
}

#' Calculate Contagion Risk
#'
#' Internal function to assess contagion-based systemic risk
#' @param networks_result WaveQTE networks
#' @return Contagion risk score
calculate_contagion_risk <- function(networks_result) {
  
  qte_array <- networks_result$original_qte$qte_array
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  
  # Focus on extreme quantiles for contagion assessment
  extreme_quantiles <- c(1, 3)  # 0.05 and 0.95
  
  contagion_scores <- numeric(0)
  
  for (s in 1:length(scales)) {
    for (q_idx in extreme_quantiles) {
      
      qte_matrix <- qte_array[, , s, q_idx]
      diag(qte_matrix) <- 0
      
      # Maximum contagion potential
      max_qte <- max(qte_matrix, na.rm = TRUE)
      
      # Average contagion
      avg_qte <- mean(qte_matrix[qte_matrix > 0], na.rm = TRUE)
      
      # Scale weight (higher for longer scales)
      scale_weight <- scales[s] / sum(scales)
      
      contagion_score <- scale_weight * (0.6 * max_qte + 0.4 * avg_qte)
      contagion_scores <- c(contagion_scores, contagion_score)
    }
  }
  
  return(mean(contagion_scores, na.rm = TRUE))
}

#' Calculate Volatility Risk
#'
#' Internal function to assess volatility-based systemic risk
#' @param networks_result WaveQTE networks
#' @return Volatility risk score
calculate_volatility_risk <- function(networks_result) {
  
  qte_array <- networks_result$original_qte$qte_array
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  
  volatility_scores <- numeric(0)
  
  # Calculate volatility across scales for each quantile
  for (q in 1:length(quantiles)) {
    
    scale_averages <- numeric(length(scales))
    
    for (s in 1:length(scales)) {
      qte_matrix <- qte_array[, , s, q]
      diag(qte_matrix) <- 0
      scale_averages[s] <- mean(qte_matrix[qte_matrix > 0], na.rm = TRUE)
    }
    
    # Calculate volatility across scales
    if (length(scale_averages) >= 3 && !all(is.na(scale_averages))) {
      volatility <- sd(scale_averages, na.rm = TRUE)
      
      # Weight extreme quantiles higher
      quant_weight <- ifelse(q %in% c(1, 3), 0.4, 0.2)
      volatility_scores <- c(volatility_scores, volatility * quant_weight)
    }
  }
  
  return(sum(volatility_scores, na.rm = TRUE))
}

#' Assess Current Risk
#'
#' Internal function to provide comprehensive current risk assessment
#' @param sri Systemic risk index
#' @param risk_components Individual risk components
#' @param prediction_model Crisis prediction model (optional)
#' @return Current risk assessment
assess_current_risk <- function(sri, risk_components, prediction_model = NULL) {
  
  # Base assessment from SRI
  base_risk <- sri$sri_value
  alert_level <- sri$alert_level
  
  # Component analysis
  component_risks <- sapply(risk_components, function(x) {
    if (is.numeric(x)) x else 0
  })
  
  # Overall risk score (weighted combination)
  overall_risk <- 0.4 * base_risk + 
                 0.2 * component_risks["connectivity"] +
                 0.2 * component_risks["concentration"] +
                 0.1 * component_risks["contagion"] +
                 0.1 * component_risks["volatility"]
  
  # Crisis probability (if prediction model available)
  crisis_probability <- NA
  if (!is.null(prediction_model)) {
    # This would require current feature values
    # For now, provide a placeholder
    crisis_probability <- pmin(1, overall_risk / 2)
  }
  
  # Risk assessment
  assessment <- list(
    overall_risk_score = overall_risk,
    alert_level = alert_level,
    crisis_probability = crisis_probability,
    component_contributions = component_risks,
    recommendations = generate_risk_recommendations(overall_risk, alert_level, component_risks),
    assessment_time = Sys.time()
  )
  
  return(assessment)
}

#' Generate Risk Recommendations
#'
#' Internal function to generate risk management recommendations
#' @param overall_risk Overall risk score
#' @param alert_level Current alert level
#' @param component_risks Individual component risks
#' @return List of recommendations
generate_risk_recommendations <- function(overall_risk, alert_level, component_risks) {
  
  recommendations <- list()
  
  # General recommendations based on alert level
  if (alert_level == "Green") {
    recommendations$general <- "Continue regular monitoring. Consider stress testing."
    
  } else if (alert_level == "Amber") {
    recommendations$general <- "Increased monitoring required. Prepare contingency plans."
    
  } else if (alert_level == "Red") {
    recommendations$general <- "Immediate attention required. Activate crisis protocols."
  }
  
  # Component-specific recommendations
  if (component_risks["connectivity"] > 0.6) {
    recommendations$connectivity <- "High network connectivity detected. Monitor for cascade effects."
  }
  
  if (component_risks["concentration"] > 0.6) {
    recommendations$concentration <- "High market concentration. Focus on systemically important institutions."
  }
  
  if (component_risks["contagion"] > 0.8) {
    recommendations$contagion <- "High contagion risk. Consider circuit breakers or intervention."
  }
  
  if (component_risks["volatility"] > 0.7) {
    recommendations$volatility <- "High volatility across scales. Implement stabilization measures."
  }
  
  return(recommendations)
}

#' Intervention Targeting
#'
#' Identifies optimal targets for regulatory intervention based on
#' network centrality and systemic importance measures.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param centrality_data Object of class "waveqte_centrality"
#' @param intervention_type Type of intervention ("capital", "liquidity", "activity")
#' @param n_targets Number of markets to target (default: 3)
#' @return Data frame with intervention recommendations
#' @export
#' @examples
#' \dontrun{
#' targets <- intervention_targeting(networks, centrality_data, 
#'                                  intervention_type = "capital")
#' }
intervention_targeting <- function(networks_result, centrality_data, 
                                  intervention_type = "capital", n_targets = 3) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  if (!inherits(centrality_data, "waveqte_centrality")) {
    stop("centrality_data must be of class 'waveqte_centrality'")
  }
  
  markets <- networks_result$markets
  scales <- networks_result$scales
  quantiles <- networks_result$quantiles
  
  # Calculate systemic importance scores
  importance_scores <- centrality_data %>%
    dplyr::group_by(market) %>%
    dplyr::summarise(
      avg_degree = mean(degree_total, na.rm = TRUE),
      avg_eigen = mean(eigenvector, na.rm = TRUE),
      avg_between = mean(betweenness, na.rm = TRUE),
      max_degree = max(degree_total, na.rm = TRUE),
      max_eigen = max(eigenvector, na.rm = TRUE),
      max_between = max(betweenness, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate intervention-specific scores
  if (intervention_type == "capital") {
    # For capital requirements: focus on overall systemic importance
    importance_scores$intervention_score <- 
      0.3 * scale(importance_scores$avg_degree) +
      0.3 * scale(importance_scores$avg_eigen) +
      0.4 * scale(importance_scores$avg_between)
    
  } else if (intervention_type == "liquidity") {
    # For liquidity requirements: focus on connectivity
    importance_scores$intervention_score <- 
      0.5 * scale(importance_scores$avg_degree) +
      0.3 * scale(importance_scores$avg_eigen) +
      0.2 * scale(importance_scores$avg_between)
    
  } else if (intervention_type == "activity") {
    # For activity restrictions: focus on potential for contagion spread
    importance_scores$intervention_score <- 
      0.2 * scale(importance_scores$avg_degree) +
      0.3 * scale(importance_scores$avg_eigen) +
      0.5 * scale(importance_scores$avg_between)
    
  } else {
    # Default: equal weights
    importance_scores$intervention_score <- 
      0.33 * scale(importance_scores$avg_degree) +
      0.33 * scale(importance_scores$avg_eigen) +
      0.34 * scale(importance_scores$avg_between)
  }
  
  # Add QTE-based measures
  qte_array <- networks_result$original_qte$qte_array
  
  # Calculate outgoing and incoming QTE totals
  importance_scores$total_outgoing_qte <- 0
  importance_scores$total_incoming_qte <- 0
  importance_scores$max_outgoing_qte <- 0
  importance_scores$max_incoming_qte <- 0
  
  for (i in 1:nrow(importance_scores)) {
    market <- importance_scores$market[i]
    market_idx <- which(markets == market)
    
    # Sum across all scales and quantiles
    outgoing_qte <- sum(qte_array[market_idx, , , ], na.rm = TRUE)
    incoming_qte <- sum(qte_array[, market_idx, , ], na.rm = TRUE)
    
    max_outgoing <- max(qte_array[market_idx, , , ], na.rm = TRUE)
    max_incoming <- max(qte_array[, market_idx, , ], na.rm = TRUE)
    
    importance_scores$total_outgoing_qte[i] <- outgoing_qte
    importance_scores$total_incoming_qte[i] <- incoming_qte
    importance_scores$max_outgoing_qte[i] <- max_outgoing
    importance_scores$max_incoming_qte[i] <- max_incoming
  }
  
  # Final intervention priority score
  importance_scores$final_priority <- 
    0.6 * scale(importance_scores$intervention_score) +
    0.2 * scale(importance_scores$total_outgoing_qte) +
    0.2 * scale(importance_scores$total_incoming_qte)
  
  # Rank markets by intervention priority
  importance_scores <- importance_scores %>%
    dplyr::arrange(desc(final_priority)) %>%
    dplyr::mutate(
      intervention_rank = row_number(),
      recommended = intervention_rank <= n_targets
    )
  
  # Add intervention recommendations
  importance_scores$intervention_type <- intervention_type
  importance_scores$rationale <- ""
  
  for (i in 1:nrow(importance_scores)) {
    if (importance_scores$recommended[i]) {
      rationale_parts <- c()
      
      if (importance_scores$avg_between[i] > quantile(importance_scores$avg_between, 0.75)) {
        rationale_parts <- c(rationale_parts, "High betweenness centrality")
      }
      
      if (importance_scores$avg_degree[i] > quantile(importance_scores$avg_degree, 0.75)) {
        rationale_parts <- c(rationale_parts, "High degree centrality")
      }
      
      if (importance_scores$total_outgoing_qte[i] > quantile(importance_scores$total_outgoing_qte, 0.75)) {
        rationale_parts <- c(rationale_parts, "High outgoing contagion potential")
      }
      
      if (importance_scores$total_incoming_qte[i] > quantile(importance_scores$total_incoming_qte, 0.75)) {
        rationale_parts <- c(rationale_parts, "High incoming contagion vulnerability")
      }
      
      importance_scores$rationale[i] <- paste(rationale_parts, collapse = "; ")
    }
  }
  
  class(importance_scores) <- c("intervention_targets", "data.frame")
  return(importance_scores)
}

#' Stress Testing Framework
#'
#' Implements comprehensive stress testing using WaveQTE networks
#' to assess system resilience under various shock scenarios.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param shock_scenarios List of shock scenarios to test
#' @param propagation_steps Number of propagation steps to simulate (default: 10)
#' @param recovery_threshold Threshold for considering recovery (default: 0.1)
#' @return Object of class "stress_test_results"
#' @export
#' @examples
#' \dontrun{
#' stress_results <- stress_testing(networks, shock_scenarios, propagation_steps = 10)
#' }
stress_testing <- function(networks_result, shock_scenarios, 
                          propagation_steps = 10, recovery_threshold = 0.1) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  if (missing(shock_scenarios)) {
    # Create default shock scenarios
    shock_scenarios <- create_default_shock_scenarios(networks_result$markets)
  }
  
  markets <- networks_result$markets
  qte_array <- networks_result$original_qte$qte_array
  n_markets <- length(markets)
  
  # Initialize results storage
  stress_results <- list()
  
  for (scenario_name in names(shock_scenarios)) {
    
    scenario <- shock_scenarios[[scenario_name]]
    
    # Initialize market states
    market_states <- rep(1, n_markets)  # 1 = healthy, 0 = distressed
    names(market_states) <- markets
    
    # Apply initial shock
    if ("shocked_markets" %in% names(scenario)) {
      shocked_indices <- which(markets %in% scenario$shocked_markets)
      market_states[shocked_indices] <- scenario$shock_intensity
    }
    
    # Store propagation history
    propagation_history <- matrix(0, nrow = propagation_steps + 1, ncol = n_markets)
    colnames(propagation_history) <- markets
    propagation_history[1, ] <- market_states
    
    # Simulate propagation
    for (step in 1:propagation_steps) {
      
      new_states <- market_states
      
      # Calculate contagion effects
      for (i in 1:n_markets) {
        
        if (market_states[i] < 1) {  # If market is distressed
          
          # Calculate contagion to other markets
          for (j in 1:n_markets) {
            
            if (i != j && market_states[j] > recovery_threshold) {
              
              # Calculate average contagion effect across scales and quantiles
              contagion_effect <- 0
              weight_sum <- 0
              
              for (s in 1:dim(qte_array)[3]) {
                for (q in 1:dim(qte_array)[4]) {
                  
                  qte_value <- qte_array[i, j, s, q]
                  
                  if (qte_value > 0) {
                    # Weight by scale (higher scales = more persistent effects)
                    scale_weight <- networks_result$scales[s] / sum(networks_result$scales)
                    # Weight by quantile (extreme quantiles = stronger effects)
                    quant_weight <- ifelse(q %in% c(1, 3), 0.4, 0.2)
                    
                    weight <- scale_weight * quant_weight
                    contagion_effect <- contagion_effect + weight * qte_value
                    weight_sum <- weight_sum + weight
                  }
                }
              }
              
              if (weight_sum > 0) {
                contagion_effect <- contagion_effect / weight_sum
                
                # Apply contagion with some dampening
                shock_transmission <- contagion_effect * (1 - market_states[i]) * scenario$transmission_rate
                new_states[j] <- max(0, market_states[j] - shock_transmission)
              }
            }
          }
        }
        
        # Apply recovery (markets gradually return to health)
        if (market_states[i] < 1) {
          recovery_rate <- scenario$recovery_rate
          new_states[i] <- min(1, market_states[i] + recovery_rate)
        }
      }
      
      market_states <- new_states
      propagation_history[step + 1, ] <- market_states
      
      # Check for convergence
      if (all(market_states > 0.95) || all(market_states < 0.05)) {
        break
      }
    }
    
    # Calculate scenario metrics
    scenario_results <- list(
      scenario_name = scenario_name,
      scenario_params = scenario,
      propagation_history = propagation_history,
      final_states = market_states,
      max_impact = 1 - min(market_states),
      total_impact = sum(1 - market_states),
      affected_markets = sum(market_states < 0.9),
      recovery_time = find_recovery_time(propagation_history, recovery_threshold),
      systemic_risk_metric = calculate_systemic_risk_metric(propagation_history)
    )
    
    stress_results[[scenario_name]] <- scenario_results
  }
  
  # Overall stress test summary
  summary_stats <- data.frame(
    scenario = names(stress_results),
    max_impact = sapply(stress_results, function(x) x$max_impact),
    total_impact = sapply(stress_results, function(x) x$total_impact),
    affected_markets = sapply(stress_results, function(x) x$affected_markets),
    recovery_time = sapply(stress_results, function(x) x$recovery_time),
    systemic_risk = sapply(stress_results, function(x) x$systemic_risk_metric),
    stringsAsFactors = FALSE
  )
  
  # Create final results object
  final_results <- list(
    scenario_results = stress_results,
    summary = summary_stats,
    parameters = list(
      propagation_steps = propagation_steps,
      recovery_threshold = recovery_threshold,
      n_markets = n_markets,
      n_scenarios = length(shock_scenarios)
    ),
    test_date = Sys.time()
  )
  
  class(final_results) <- "stress_test_results"
  return(final_results)
}

#' Create Default Shock Scenarios
#'
#' Internal function to create default stress test scenarios
#' @param markets Vector of market names
#' @return List of shock scenarios
create_default_shock_scenarios <- function(markets) {
  
  scenarios <- list(
    
    # Single market shock (largest market)
    single_shock = list(
      shocked_markets = markets[1],
      shock_intensity = 0.2,  # 80% loss
      transmission_rate = 0.5,
      recovery_rate = 0.05
    ),
    
    # Regional shock (assume first 3 markets are in same region)
    regional_shock = list(
      shocked_markets = markets[1:min(3, length(markets))],
      shock_intensity = 0.3,  # 70% loss
      transmission_rate = 0.6,
      recovery_rate = 0.03
    ),
    
    # Global shock (all markets affected)
    global_shock = list(
      shocked_markets = markets,
      shock_intensity = 0.4,  # 60% loss
      transmission_rate = 0.7,
      recovery_rate = 0.02
    ),
    
    # Gradual shock (markets affected sequentially)
    gradual_shock = list(
      shocked_markets = markets[1:min(2, length(markets))],
      shock_intensity = 0.25,  # 75% loss
      transmission_rate = 0.4,
      recovery_rate = 0.04
    )
  )
  
  return(scenarios)
}

#' Find Recovery Time
#'
#' Internal function to find recovery time from propagation history
#' @param history Matrix of propagation history
#' @param threshold Recovery threshold
#' @return Recovery time in steps
find_recovery_time <- function(history, threshold) {
  
  n_steps <- nrow(history)
  
  for (step in 2:n_steps) {
    if (all(history[step, ] > (1 - threshold))) {
      return(step - 1)
    }
  }
  
  return(n_steps)  # No recovery within simulation period
}

#' Calculate Systemic Risk Metric
#'
#' Internal function to calculate systemic risk metric from stress test
#' @param history Matrix of propagation history
#' @return Systemic risk metric
calculate_systemic_risk_metric <- function(history) {
  
  # Calculate area under the curve (total distress over time)
  n_steps <- nrow(history)
  n_markets <- ncol(history)
  
  total_distress <- 0
  
  for (step in 1:n_steps) {
    step_distress <- sum(1 - history[step, ]) / n_markets
    total_distress <- total_distress + step_distress
  }
  
  # Normalize by number of steps
  systemic_risk <- total_distress / n_steps
  
  return(systemic_risk)
}