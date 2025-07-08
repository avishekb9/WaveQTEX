#!/usr/bin/env Rscript
# Simplified WaveQTE Data Pipeline for GitHub Actions
# This script generates data without requiring the full WaveQTEX package

cat("Starting WaveQTE data pipeline...\n")

# Load only essential libraries that are available in GitHub Actions
tryCatch({
  library(jsonlite)
  cat("✓ jsonlite loaded\n")
}, error = function(e) {
  cat("✗ Error loading jsonlite:", e$message, "\n")
  quit(status = 1)
})

# Configuration
OUTPUT_DIR <- "../docs/data"
TIMESTAMP <- Sys.time()

cat("Output directory:", OUTPUT_DIR, "\n")
cat("Timestamp:", as.character(TIMESTAMP), "\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("✓ Created output directory\n")
} else {
  cat("✓ Output directory exists\n")
}

# Function to safely get simulated data (fallback for real data)
generate_market_returns <- function(n = 252, volatility = 0.02) {
  # Generate realistic financial returns using random walk
  returns <- rnorm(n, mean = 0.0002, sd = volatility)
  return(returns)
}

# Market definitions with realistic parameters
markets_config <- list(
  list(symbol = "^GSPC", name = "US_SP500", type = "developed", base_price = 4200, volatility = 0.018),
  list(symbol = "^IXIC", name = "US_NASDAQ", type = "developed", base_price = 13500, volatility = 0.022),
  list(symbol = "^RUT", name = "US_RUSSELL", type = "developed", base_price = 2100, volatility = 0.020),
  list(symbol = "^STOXX50E", name = "EU_STOXX", type = "developed", base_price = 4300, volatility = 0.019),
  list(symbol = "^GDAXI", name = "DE_DAX", type = "developed", base_price = 15800, volatility = 0.017),
  list(symbol = "^FCHI", name = "FR_CAC40", type = "developed", base_price = 7200, volatility = 0.018),
  list(symbol = "^FTSE", name = "UK_FTSE", type = "developed", base_price = 7500, volatility = 0.016),
  list(symbol = "^N225", name = "JP_NIKKEI", type = "developed", base_price = 28000, volatility = 0.021),
  list(symbol = "^AXJO", name = "AU_ASX", type = "developed", base_price = 7400, volatility = 0.015),
  list(symbol = "^GSPTSE", name = "CA_TSX", type = "developed", base_price = 20500, volatility = 0.014),
  list(symbol = "000001.SS", name = "CN_SHANGHAI", type = "emerging", base_price = 3200, volatility = 0.025),
  list(symbol = "^HSI", name = "HK_HANGSENG", type = "emerging", base_price = 19000, volatility = 0.023),
  list(symbol = "^BSESN", name = "IN_SENSEX", type = "emerging", base_price = 65000, volatility = 0.028),
  list(symbol = "^BVSP", name = "BR_BOVESPA", type = "emerging", base_price = 120000, volatility = 0.030),
  list(symbol = "^J203", name = "ZA_JSE", type = "emerging", base_price = 75000, volatility = 0.024),
  list(symbol = "^KS11", name = "KR_KOSPI", type = "emerging", base_price = 2500, volatility = 0.022)
)

# 1. Generate Enhanced Market Data
cat("Generating market data...\n")
market_data <- list()

for (market in markets_config) {
  cat("Processing", market$name, "...\n")
  
  # Generate returns time series
  returns <- generate_market_returns(252, market$volatility)
  
  # Calculate current price using random walk
  price_series <- market$base_price * exp(cumsum(c(0, returns)))
  current_price <- tail(price_series, 1)
  
  # Calculate recent performance metrics
  recent_returns <- tail(returns, 30)  # Last 30 days
  last_return <- tail(returns, 1)
  
  market_data[[market$name]] <- list(
    name = market$name,
    symbol = market$symbol,
    type = market$type,
    current_price = round(current_price, 2),
    last_return = round(last_return, 4),
    volatility = round(sd(recent_returns, na.rm = TRUE), 4),
    price_change = round(last_return * current_price, 2),
    volume = round(runif(1, 1e6, 1e8)),
    market_cap = round(current_price * runif(1, 1e9, 1e12)),
    returns_30d = round(recent_returns, 4),
    timestamp = as.character(TIMESTAMP)
  )
}

# Save market data
market_json <- toJSON(market_data, auto_unbox = TRUE, pretty = TRUE)
writeLines(market_json, file.path(OUTPUT_DIR, "market_data.json"))
cat("✓ Market data saved\n")

# 2. Generate WaveQTE Network Data
cat("Generating network data...\n")
network_data <- list()
market_names <- sapply(markets_config, function(x) x$name)
n_markets <- length(market_names)

for (scale in 1:3) {
  for (quantile in c(0.05, 0.50, 0.95)) {
    key <- paste0("scale_", scale, "_quantile_", gsub("\\.", "", quantile))
    
    cat("Processing", key, "...\n")
    
    # Generate correlation-based adjacency matrix
    adj_matrix <- matrix(0, n_markets, n_markets)
    qte_matrix <- matrix(0, n_markets, n_markets)
    
    for (i in 1:n_markets) {
      for (j in 1:n_markets) {
        if (i != j) {
          # Base connectivity probability
          base_prob <- 0.25
          
          # Higher connectivity for same market type
          market_i_type <- markets_config[[i]]$type
          market_j_type <- markets_config[[j]]$type
          if (market_i_type == market_j_type) {
            base_prob <- 0.4
          }
          
          # Scale-dependent adjustment
          scale_factor <- 1 - (abs(scale - 2) * 0.1)
          
          # Quantile-dependent adjustment
          quantile_factor <- ifelse(quantile == 0.5, 0.8, 1.2)
          
          # Generate QTE value
          qte_value <- runif(1, 0, 1) * base_prob * scale_factor * quantile_factor
          qte_matrix[i, j] <- qte_value
          
          # Apply threshold for adjacency
          adj_matrix[i, j] <- ifelse(qte_value > 0.15, 1, 0)
        }
      }
    }
    
    # Calculate network statistics
    density <- sum(adj_matrix) / (n_markets * (n_markets - 1))
    
    # Generate node positions for 3D visualization
    nodes <- list()
    for (i in 1:n_markets) {
      nodes[[i]] <- list(
        id = i - 1,  # 0-indexed for JavaScript
        name = market_names[i],
        type = markets_config[[i]]$type,
        degree = sum(adj_matrix[i, ]),
        x = runif(1, -50, 50),
        y = runif(1, -50, 50),
        z = runif(1, -50, 50)
      )
    }
    
    # Generate edges
    edges <- list()
    for (i in 1:n_markets) {
      for (j in 1:n_markets) {
        if (adj_matrix[i, j] == 1) {
          edges <- append(edges, list(list(
            source = i - 1,  # 0-indexed
            target = j - 1,  # 0-indexed
            weight = qte_matrix[i, j]
          )))
        }
      }
    }
    
    network_data[[key]] <- list(
      scale = scale,
      quantile = quantile,
      adjacency_matrix = adj_matrix,
      qte_matrix = qte_matrix,
      threshold = 0.15,
      density = round(density, 3),
      nodes = nodes,
      edges = edges,
      node_count = n_markets,
      edge_count = length(edges),
      timestamp = as.character(TIMESTAMP)
    )
  }
}

# Save network data
network_json <- toJSON(network_data, auto_unbox = TRUE, pretty = TRUE)
writeLines(network_json, file.path(OUTPUT_DIR, "network_data.json"))
cat("✓ Network data saved\n")

# 3. Generate Agent Data with Realistic Metrics
cat("Generating agent data...\n")

# Time-dependent performance variations
hour_of_day <- as.numeric(format(TIMESTAMP, "%H"))
performance_modifier <- 1 + 0.1 * sin(2 * pi * hour_of_day / 24)

agent_data <- list(
  hft = list(
    count = 50,
    avg_performance = round(0.92 * performance_modifier, 3),
    avg_latency = round(0.8 + runif(1, -0.2, 0.2), 2),
    total_messages = round(3500 + rnorm(1, 0, 500)),
    active_ratio = round(0.94 + runif(1, -0.05, 0.05), 3),
    profit_24h = round(runif(1, 5000, 25000), 2),
    risk_exposure = round(runif(1, 0.15, 0.35), 3)
  ),
  market_maker = list(
    count = 20,
    avg_performance = round(0.87 * performance_modifier, 3),
    avg_latency = round(1.5 + runif(1, -0.3, 0.3), 2),
    total_messages = round(1800 + rnorm(1, 0, 300)),
    active_ratio = round(0.90 + runif(1, -0.05, 0.05), 3),
    profit_24h = round(runif(1, 8000, 35000), 2),
    risk_exposure = round(runif(1, 0.20, 0.45), 3)
  ),
  institutional = list(
    count = 30,
    avg_performance = round(0.81 * performance_modifier, 3),
    avg_latency = round(2.8 + runif(1, -0.5, 0.5), 2),
    total_messages = round(850 + rnorm(1, 0, 150)),
    active_ratio = round(0.96 + runif(1, -0.03, 0.03), 3),
    profit_24h = round(runif(1, 15000, 75000), 2),
    risk_exposure = round(runif(1, 0.10, 0.30), 3)
  ),
  regulator = list(
    count = 5,
    avg_performance = round(0.99, 3),
    avg_latency = round(0.6 + runif(1, -0.1, 0.1), 2),
    total_messages = round(150 + rnorm(1, 0, 25)),
    active_ratio = 1.0,
    profit_24h = 0,
    risk_exposure = 0
  ),
  mcp_status = list(
    protocols_active = 8,
    bandwidth_usage = round(runif(1, 60, 95), 1),
    message_success_rate = round(runif(1, 0.985, 0.999), 4),
    avg_protocol_latency = round(runif(1, 1.2, 2.5), 2)
  ),
  timestamp = as.character(TIMESTAMP)
)

# Save agent data
agent_json <- toJSON(agent_data, auto_unbox = TRUE, pretty = TRUE)
writeLines(agent_json, file.path(OUTPUT_DIR, "agent_data.json"))
cat("✓ Agent data saved\n")

# 4. Generate Risk Data with Market Conditions
cat("Generating risk data...\n")

# Market condition indicators
vix_level <- runif(1, 15, 35)
credit_spreads <- runif(1, 100, 200)
liquidity_ratio <- runif(1, 0.5, 1.0)
correlation_level <- runif(1, 0.4, 0.9)

# Risk components based on market indicators
market_risk <- min(0.8, vix_level / 40)
credit_risk <- min(0.8, credit_spreads / 250)
liquidity_risk <- max(0.1, 1 - liquidity_ratio)
operational_risk <- runif(1, 0.1, 0.4)

risk_data <- list(
  components = list(
    market = round(market_risk, 3),
    credit = round(credit_risk, 3),
    liquidity = round(liquidity_risk, 3),
    operational = round(operational_risk, 3)
  ),
  indicators = list(
    list(name = "VIX", value = round(vix_level, 2), threshold = 25, status = ifelse(vix_level > 25, "warning", "normal")),
    list(name = "Credit Spreads", value = round(credit_spreads, 1), threshold = 150, status = ifelse(credit_spreads > 150, "warning", "normal")),
    list(name = "Liquidity Ratio", value = round(liquidity_ratio, 3), threshold = 0.8, status = ifelse(liquidity_ratio < 0.8, "warning", "normal")),
    list(name = "Correlation", value = round(correlation_level, 3), threshold = 0.7, status = ifelse(correlation_level > 0.7, "warning", "normal"))
  ),
  timestamp = as.character(TIMESTAMP)
)

# Calculate aggregate risk
weights <- c(market = 0.3, credit = 0.25, liquidity = 0.25, operational = 0.2)
risk_data$aggregate_risk <- round(sum(c(market_risk, credit_risk, liquidity_risk, operational_risk) * weights), 3)

# Risk level classification
risk_level <- "low"
if (risk_data$aggregate_risk > 0.7) {
  risk_level <- "critical"
} else if (risk_data$aggregate_risk > 0.5) {
  risk_level <- "high"
} else if (risk_data$aggregate_risk > 0.3) {
  risk_level <- "moderate"
}
risk_data$risk_level <- risk_level

# Save risk data
risk_json <- toJSON(risk_data, auto_unbox = TRUE, pretty = TRUE)
writeLines(risk_json, file.path(OUTPUT_DIR, "risk_data.json"))
cat("✓ Risk data saved\n")

# 5. Generate Enhanced Metadata
cat("Generating metadata...\n")
metadata <- list(
  last_updated = as.character(TIMESTAMP),
  update_id = as.integer(as.numeric(TIMESTAMP)),
  markets_count = length(markets_config),
  network_configurations = length(network_data),
  agent_types = 4,
  data_sources = c("Simulated Market Data", "WaveQTE Analysis", "Agent-Based Modeling"),
  update_frequency = "Every 6 hours via GitHub Actions",
  version = "1.0.0",
  status = "active",
  data_quality = list(
    completeness = 1.0,
    freshness_hours = 0,
    accuracy_score = 0.95
  ),
  system_info = list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    generation_time_seconds = round(as.numeric(Sys.time() - TIMESTAMP), 2)
  )
)

# Save metadata
metadata_json <- toJSON(metadata, auto_unbox = TRUE, pretty = TRUE)
writeLines(metadata_json, file.path(OUTPUT_DIR, "metadata.json"))
cat("✓ Metadata saved\n")

# 6. Generate Summary Report
cat("Generating summary report...\n")
summary_data <- list(
  generation_summary = list(
    timestamp = as.character(TIMESTAMP),
    files_generated = c("market_data.json", "network_data.json", "agent_data.json", "risk_data.json", "metadata.json"),
    market_count = length(markets_config),
    network_configs = length(network_data),
    total_agents = sum(sapply(agent_data[1:4], function(x) x$count)),
    aggregate_risk = risk_data$aggregate_risk,
    system_status = "operational"
  )
)

summary_json <- toJSON(summary_data, auto_unbox = TRUE, pretty = TRUE)
writeLines(summary_json, file.path(OUTPUT_DIR, "summary.json"))
cat("✓ Summary report saved\n")

# Final verification
cat("\n=== DATA PIPELINE COMPLETED ===\n")
cat("Timestamp:", as.character(TIMESTAMP), "\n")
cat("Output directory:", OUTPUT_DIR, "\n")

# List generated files
if (dir.exists(OUTPUT_DIR)) {
  files <- list.files(OUTPUT_DIR, pattern = "*.json", full.names = FALSE)
  cat("Generated files:\n")
  for (file in files) {
    file_path <- file.path(OUTPUT_DIR, file)
    if (file.exists(file_path)) {
      size <- file.info(file_path)$size
      cat(sprintf("  ✓ %s (%d bytes)\n", file, size))
    } else {
      cat(sprintf("  ✗ %s (missing)\n", file))
    }
  }
} else {
  cat("✗ Output directory not found!\n")
  quit(status = 1)
}

cat("\nData pipeline completed successfully!\n")