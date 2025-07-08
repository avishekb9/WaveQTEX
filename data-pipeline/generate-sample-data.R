#!/usr/bin/env Rscript
# Generate sample data for web interface without full WaveQTEX package

# Load required libraries
library(jsonlite)

# Configuration
OUTPUT_DIR <- "docs/data"
TIMESTAMP <- Sys.time()

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat("Generating sample data for WaveQTE web interface...\n")

# 1. Generate Market Data
cat("Generating market data...\n")
markets <- list(
  "US_SP500" = list(name = "US_SP500", type = "developed", current_price = 4200, last_return = 0.012, volatility = 0.18),
  "US_NASDAQ" = list(name = "US_NASDAQ", type = "developed", current_price = 13500, last_return = 0.008, volatility = 0.22),
  "US_RUSSELL" = list(name = "US_RUSSELL", type = "developed", current_price = 2100, last_return = 0.006, volatility = 0.20),
  "EU_STOXX" = list(name = "EU_STOXX", type = "developed", current_price = 4300, last_return = 0.005, volatility = 0.19),
  "DE_DAX" = list(name = "DE_DAX", type = "developed", current_price = 15800, last_return = 0.007, volatility = 0.17),
  "FR_CAC40" = list(name = "FR_CAC40", type = "developed", current_price = 7200, last_return = 0.004, volatility = 0.18),
  "UK_FTSE" = list(name = "UK_FTSE", type = "developed", current_price = 7500, last_return = 0.003, volatility = 0.16),
  "JP_NIKKEI" = list(name = "JP_NIKKEI", type = "developed", current_price = 28000, last_return = 0.009, volatility = 0.21),
  "AU_ASX" = list(name = "AU_ASX", type = "developed", current_price = 7400, last_return = 0.006, volatility = 0.15),
  "CA_TSX" = list(name = "CA_TSX", type = "developed", current_price = 20500, last_return = 0.005, volatility = 0.14),
  "CN_SHANGHAI" = list(name = "CN_SHANGHAI", type = "emerging", current_price = 3200, last_return = 0.011, volatility = 0.25),
  "HK_HANGSENG" = list(name = "HK_HANGSENG", type = "emerging", current_price = 19000, last_return = 0.008, volatility = 0.23),
  "IN_SENSEX" = list(name = "IN_SENSEX", type = "emerging", current_price = 65000, last_return = 0.015, volatility = 0.28),
  "BR_BOVESPA" = list(name = "BR_BOVESPA", type = "emerging", current_price = 120000, last_return = 0.012, volatility = 0.30),
  "ZA_JSE" = list(name = "ZA_JSE", type = "emerging", current_price = 75000, last_return = 0.007, volatility = 0.24),
  "KR_KOSPI" = list(name = "KR_KOSPI", type = "emerging", current_price = 2500, last_return = 0.009, volatility = 0.22)
)

# Add timestamp to each market
for (i in seq_along(markets)) {
  markets[[i]]$timestamp <- as.character(TIMESTAMP)
}

# Save market data
writeLines(toJSON(markets, auto_unbox = TRUE, pretty = TRUE), 
           file.path(OUTPUT_DIR, "market_data.json"))

# 2. Generate Network Data
cat("Generating network data...\n")
network_data <- list()

market_names <- names(markets)
n_markets <- length(market_names)

for (scale in 1:3) {
  for (quantile in c(0.05, 0.50, 0.95)) {
    key <- paste0("scale_", scale, "_quantile_", gsub("\\.", "", quantile))
    
    # Generate random adjacency matrix
    adj_matrix <- matrix(0, n_markets, n_markets)
    for (i in 1:n_markets) {
      for (j in 1:n_markets) {
        if (i != j) {
          # Higher connectivity for same region
          base_prob <- 0.25
          if (markets[[i]]$type == markets[[j]]$type) {
            base_prob <- 0.4
          }
          adj_matrix[i, j] <- rbinom(1, 1, base_prob * (1 - abs(scale - 2) * 0.1))
        }
      }
    }
    
    network_data[[key]] <- list(
      scale = scale,
      quantile = quantile,
      adjacency_matrix = adj_matrix,
      threshold = 0.15,
      density = sum(adj_matrix) / (n_markets * (n_markets - 1)),
      nodes = market_names,
      timestamp = as.character(TIMESTAMP)
    )
  }
}

# Save network data
writeLines(toJSON(network_data, auto_unbox = TRUE, pretty = TRUE), 
           file.path(OUTPUT_DIR, "network_data.json"))

# 3. Generate Agent Data
cat("Generating agent data...\n")
agent_data <- list(
  hft = list(
    count = 50,
    avg_performance = 0.92,
    avg_latency = 0.8,
    total_messages = 3500,
    active_ratio = 0.94
  ),
  market_maker = list(
    count = 20,
    avg_performance = 0.87,
    avg_latency = 1.5,
    total_messages = 1800,
    active_ratio = 0.90
  ),
  institutional = list(
    count = 30,
    avg_performance = 0.81,
    avg_latency = 2.8,
    total_messages = 850,
    active_ratio = 0.96
  ),
  regulator = list(
    count = 5,
    avg_performance = 0.99,
    avg_latency = 0.6,
    total_messages = 150,
    active_ratio = 1.0
  ),
  timestamp = as.character(TIMESTAMP)
)

# Save agent data
writeLines(toJSON(agent_data, auto_unbox = TRUE, pretty = TRUE), 
           file.path(OUTPUT_DIR, "agent_data.json"))

# 4. Generate Risk Data
cat("Generating risk data...\n")
risk_components <- list(
  market = 0.45,
  credit = 0.32,
  liquidity = 0.38,
  operational = 0.25
)

risk_data <- list(
  components = risk_components,
  aggregate_risk = weighted.mean(
    unlist(risk_components),
    c(0.3, 0.25, 0.25, 0.2)
  ),
  indicators = list(
    list(name = "VIX", value = 22.5, threshold = 25),
    list(name = "Credit Spreads", value = 135, threshold = 150),
    list(name = "Liquidity Ratio", value = 0.78, threshold = 0.8),
    list(name = "Correlation", value = 0.65, threshold = 0.7)
  ),
  timestamp = as.character(TIMESTAMP)
)

# Save risk data
writeLines(toJSON(risk_data, auto_unbox = TRUE, pretty = TRUE), 
           file.path(OUTPUT_DIR, "risk_data.json"))

# 5. Generate Metadata
cat("Generating metadata...\n")
metadata <- list(
  last_updated = as.character(TIMESTAMP),
  markets_count = length(markets),
  data_sources = c("Simulated Market Data", "WaveQTE Analysis", "Agent Simulation"),
  update_frequency = "Every 6 hours via GitHub Actions",
  version = "1.0.0",
  status = "active"
)

# Save metadata
writeLines(toJSON(metadata, auto_unbox = TRUE, pretty = TRUE), 
           file.path(OUTPUT_DIR, "metadata.json"))

cat("Sample data generation completed successfully!\n")
cat("Generated files in", OUTPUT_DIR, ":\n")
cat("- market_data.json (", length(markets), "markets)\n")
cat("- network_data.json (", length(network_data), "network configurations)\n")
cat("- agent_data.json (", length(agent_data) - 1, "agent types)\n")
cat("- risk_data.json (", length(risk_data$components), "risk components)\n")
cat("- metadata.json\n")
cat("Timestamp:", as.character(TIMESTAMP), "\n")