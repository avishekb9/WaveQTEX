# WaveQTEX: Extended Wavelet-Based Quantile Transfer Entropy Networks

[![R-CMD-check](https://github.com/waveqte-enhanced/WaveQTEX/workflows/R-CMD-check/badge.svg)](https://github.com/waveqte-enhanced/WaveQTEX/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/WaveQTEX)](https://CRAN.R-project.org/package=WaveQTEX)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

WaveQTEX is a comprehensive R package that extends the foundational Wavelet-based Quantile Transfer Entropy (WaveQTE) methodology with machine learning integration, efficient market hypothesis testing, and macroprudential applications. The package provides advanced tools for analyzing financial contagion patterns across multiple temporal scales and risk levels.

## Key Features

### 🌊 Core WaveQTE Analysis
- **Multi-scale wavelet decomposition** of financial time series (6 temporal scales)
- **Quantile transfer entropy calculation** across extreme downside (5%), median (50%), and extreme upside (95%) conditions
- **Adaptive network thresholding** to resolve the "empty network problem"
- **Comprehensive network analysis** with centrality measures and visualization

### 🤖 Machine Learning Integration
- **Deep learning architectures**: CNN, LSTM, and Transformer models for WaveQTE features
- **Ensemble learning framework** combining multiple model types for robust prediction
- **Advanced feature engineering** with cross-scale and network-based features
- **Crisis prediction models** with early warning capabilities

### 📊 Market Efficiency Testing
- **Multi-scale efficiency analysis** using ADF, KPSS, and variance ratio tests
- **Scale-dependent efficiency scoring** revealing efficiency patterns across investment horizons
- **Adaptive market hypothesis testing** with time-varying efficiency measures
- **Comprehensive efficiency reporting** for academic and policy applications

### 🏛️ Macroprudential Applications
- **Real-time systemic risk monitoring** with comprehensive risk indices
- **Early warning systems** for financial crisis prediction (2-3 months lead time)
- **Intervention targeting** algorithms for optimal regulatory focus
- **Stress testing framework** with multiple shock scenarios
- **Interactive dashboards** for regulatory monitoring

### 📈 Advanced Visualization
- **Multi-panel network evolution** plots across temporal scales
- **Interactive heatmaps** for QTE pattern analysis
- **Real-time monitoring dashboards** with alert systems
- **Publication-quality plots** for academic research

## Installation

### From GitHub (Development Version)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install WaveQTEX
devtools::install_github("avishekb9/WaveQTEX")
```

### Dependencies

WaveQTEX requires several packages for full functionality:

```r
# Core dependencies
install.packages(c("wavelets", "RTransferEntropy", "igraph", "ggplot2", 
                   "dplyr", "tidyr", "magrittr"))

# Machine learning dependencies
install.packages(c("randomForest", "xgboost", "caret", "torch", "keras"))

# Financial analysis dependencies  
install.packages(c("quantmod", "TTR", "PerformanceAnalytics", "tseries", 
                   "urca", "vars", "forecast"))

# Visualization dependencies
install.packages(c("plotly", "corrplot", "DT", "shiny", "shinydashboard"))
```

## Quick Start

### Basic WaveQTE Analysis

```r
library(WaveQTEX)

# Load sample data
data(sample_stock_returns)
returns_data <- sample_stock_returns[, -1]  # Remove date column

# Step 1: Wavelet decomposition
decomp <- waveqte_decompose(returns_data, scales = 1:6)

# Step 2: Calculate quantile transfer entropy
qte_results <- calculate_qte(decomp, quantiles = c(0.05, 0.50, 0.95))

# Step 3: Apply adaptive thresholding
networks <- adaptive_threshold(qte_results, threshold_percentile = 0.75)

# Step 4: Calculate network centrality
centrality <- calculate_centrality(networks)

# Step 5: Visualize results
qte_heatmap <- plot_qte_heatmap(qte_results, scale = 1, quantile = 0.95)
print(qte_heatmap)
```

### Machine Learning Integration

```r
# Feature engineering for ML models
features <- feature_engineering(networks, centrality)

# Train ensemble model for crisis prediction
data(crisis_indicators)
ensemble_model <- train_waveqte_ensemble(
  features, 
  target_variable = "crisis_indicator",
  base_models = c("cnn", "lstm", "xgboost")
)

# Make predictions
predictions <- ensemble_predict(ensemble_model, features)
```

### Market Efficiency Testing

```r
# Multi-scale efficiency analysis
efficiency_results <- multi_scale_efficiency_test(
  decomp, 
  tests = c("adf", "kpss", "variance_ratio")
)

# Calculate efficiency scores
efficiency_scores <- efficiency_score(
  efficiency_results,
  weights = c(adf = 0.3, kpss = 0.3, vr = 0.4)
)

# View results
print(efficiency_scores)
```

### Macroprudential Monitoring

```r
# Calculate systemic risk index
sri <- calculate_systemic_risk_index(
  networks,
  scale_weights = c(1, 1, 2, 2, 3, 3),  # Higher weights for longer scales
  quantile_weights = c(3, 1, 3)         # Higher weights for extreme quantiles
)

# Create early warning system
ews <- create_early_warning_system(
  networks, 
  historical_data = returns_data,
  crisis_indicators = crisis_indicators$crisis_indicator
)

# Generate intervention targets
targets <- intervention_targeting(
  networks, 
  centrality, 
  intervention_type = "capital",
  n_targets = 3
)

# Run stress tests
stress_scenarios <- list(
  single_shock = list(
    shocked_markets = "SP500",
    shock_intensity = 0.3,
    transmission_rate = 0.5,
    recovery_rate = 0.05
  )
)

stress_results <- stress_testing(networks, stress_scenarios)
```

### Advanced Visualization

```r
# Network evolution across scales
network_plots <- plot_network_evolution(
  networks, 
  quantile = 0.95,
  layout = "spring",
  node_size_by = "betweenness"
)

# Display scale 3 network
print(network_plots$scale_3)

# Create monitoring dashboard
dashboard <- early_warning_dashboard(ews, interactive = FALSE)
print(dashboard$sri_gauge)
```

## Methodology

### Wavelet Decomposition

The package uses discrete wavelet transforms to decompose financial time series into multiple temporal scales:

```
r(t) = A_J(t) + ∑[j=1 to J] D_j(t)
```

Where:
- `A_J(t)` is the approximation component
- `D_j(t)` represents detail components at scale j
- Each scale captures different investment horizons

### Quantile Transfer Entropy

For each scale-quantile combination, transfer entropy is calculated as:

```
QTE_{X→Y}^{(s,τ)} = ∑ p(I_Y^{(τ)}, I_{Y,-1}^{(τ)}, I_{X,-1}^{(τ)}) × 
                     log₂[p(I_Y^{(τ)}|I_{Y,-1}^{(τ)}, I_{X,-1}^{(τ)}) / p(I_Y^{(τ)}|I_{Y,-1}^{(τ)})]
```

Where `I^{(τ)}` represents binary indicator variables based on quantile thresholds.

### Adaptive Thresholding

The adaptive threshold for each scale-quantile combination is:

```
θ^{(s,τ)} = Q_{0.75}{W^{(s,τ)}[W^{(s,τ)} > 0]}
```

This maintains consistent network density while preserving meaningful connections.

## Applications

### Academic Research
- **Financial contagion analysis** across multiple time horizons
- **Market efficiency testing** at different temporal scales  
- **Systemic risk measurement** with network-based approaches
- **Crisis prediction modeling** using machine learning

### Regulatory Policy
- **Real-time systemic risk monitoring** for central banks
- **Early warning systems** for financial stability
- **Intervention targeting** for optimal regulatory impact
- **Stress testing** for financial system resilience

### Financial Practice
- **Portfolio risk management** with multi-scale analysis
- **Crisis hedging strategies** based on contagion patterns
- **Market timing** using efficiency analysis
- **Algorithmic trading** with ML-enhanced features

## Package Structure

```
WaveQTEX/
├── R/
│   ├── core_functions.R          # Basic WaveQTE functionality
│   ├── emh_functions.R           # Market efficiency testing
│   ├── ml_functions.R            # Machine learning integration
│   ├── macro_functions.R         # Macroprudential applications
│   ├── visualization_functions.R # Plotting and dashboards
│   └── data.R                   # Data documentation
├── data/
│   ├── sample_stock_returns.rda  # Example financial data
│   ├── crisis_indicators.rda     # Crisis period indicators
│   └── market_classifications.rda # Market metadata
├── man/                         # Documentation files
├── tests/                       # Unit tests
├── vignettes/                   # Detailed tutorials
└── inst/                       # Additional package files
```

## Contributing

We welcome contributions to WaveQTEX! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details on:

- Submitting bug reports and feature requests
- Setting up the development environment
- Code style and testing requirements
- Submitting pull requests

## Citation

If you use WaveQTEX in your research, please cite both the package and the foundational methodology:

```bibtex
@misc{waveqtex2024,
  title = {WaveQTEX: Extended Wavelet-Based Quantile Transfer Entropy Networks},
  author = {{Extended WaveQTE Research Consortium}},
  year = {2024},
  url = {https://github.com/avishekb9/WaveQTEX},
  note = {R package version 1.0.0}
}

@article{bhandari2025waveqte,
  title = {Wavelet-Based Quantile Transfer Entropy Networks: A Multi-Scale Analysis of Global Financial Contagion and Systemic Risk Propagation},
  author = {Bhandari, Avishek},
  journal = {Working Paper},
  institution = {Indian Institute of Technology Bhubaneswar},
  year = {2025}
}
```

## License

WaveQTEX is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.

## Acknowledgments

We gratefully acknowledge:
- **Avishek Bhandari** for the foundational WaveQTE methodology
- The **R Core Team** and package maintainers for the robust R ecosystem
- **Financial research community** for theoretical foundations and empirical insights
- **Open source contributors** who make advanced analytics accessible globally

## Contact

For questions, suggestions, or collaboration opportunities:

- **Email**: research@waveqte-enhanced.org
- **GitHub Issues**: [Report bugs or request features](https://github.com/avishekb9/WaveQTEX/issues)
- **Discussions**: [Join community discussions](https://github.com/avishekb9/WaveQTEX/discussions)

---

*WaveQTEX: Advancing the frontier of multi-scale financial network analysis with machine learning and regulatory applications.*