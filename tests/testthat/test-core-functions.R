# Test core WaveQTE functions
library(testthat)
library(WaveQTEX)

# Test wavelet decomposition
test_that("waveqte_decompose works correctly", {
  
  # Create test data
  set.seed(123)
  test_data <- matrix(rnorm(500, 0, 0.02), nrow = 100, ncol = 5)
  colnames(test_data) <- paste0("Market_", 1:5)
  
  # Test decomposition
  decomp <- waveqte_decompose(test_data, scales = 1:3)
  
  # Check structure
  expect_s3_class(decomp, "waveqte_decomposition")
  expect_equal(decomp$scales, 1:3)
  expect_equal(length(decomp$markets), 5)
  expect_equal(names(decomp$decomposition), colnames(test_data))
  
  # Check that each market has decomposition for each scale
  for (market in decomp$markets) {
    for (scale in decomp$scales) {
      scale_name <- paste0("scale_", scale)
      expect_true(scale_name %in% names(decomp$decomposition[[market]]))
    }
  }
})

# Test QTE calculation
test_that("calculate_qte works correctly", {
  
  # Create test decomposition
  set.seed(123)
  test_data <- matrix(rnorm(500, 0, 0.02), nrow = 100, ncol = 3)
  colnames(test_data) <- c("A", "B", "C")
  
  decomp <- waveqte_decompose(test_data, scales = 1:2)
  
  # Calculate QTE
  qte_result <- calculate_qte(decomp, quantiles = c(0.05, 0.95), min_obs = 10)
  
  # Check structure
  expect_s3_class(qte_result, "waveqte_result")
  expect_equal(dim(qte_result$qte_array), c(3, 3, 2, 2))
  expect_equal(qte_result$markets, c("A", "B", "C"))
  expect_equal(qte_result$scales, 1:2)
  expect_equal(qte_result$quantiles, c(0.05, 0.95))
  
  # Check that diagonal is zero (no self-loops)
  for (s in 1:2) {
    for (q in 1:2) {
      diag_values <- diag(qte_result$qte_array[, , s, q])
      expect_true(all(diag_values == 0))
    }
  }
})

# Test adaptive thresholding
test_that("adaptive_threshold works correctly", {
  
  # Create test QTE result
  set.seed(123)
  test_data <- matrix(rnorm(200, 0, 0.02), nrow = 50, ncol = 4)
  colnames(test_data) <- paste0("M", 1:4)
  
  decomp <- waveqte_decompose(test_data, scales = 1:2)
  qte_result <- calculate_qte(decomp, quantiles = c(0.50, 0.95), min_obs = 5)
  
  # Apply thresholding
  networks <- adaptive_threshold(qte_result, threshold_percentile = 0.75)
  
  # Check structure
  expect_s3_class(networks, "waveqte_networks")
  expect_equal(dim(networks$networks), c(4, 4, 2, 2))
  expect_equal(dim(networks$thresholds), c(2, 2))
  
  # Check that thresholded values are non-negative
  expect_true(all(networks$networks >= 0))
  
  # Check that diagonal is zero
  for (s in 1:2) {
    for (q in 1:2) {
      diag_values <- diag(networks$networks[, , s, q])
      expect_true(all(diag_values == 0))
    }
  }
})

# Test network construction
test_that("construct_network works correctly", {
  
  # Create test networks
  set.seed(123)
  test_data <- matrix(rnorm(200, 0, 0.02), nrow = 50, ncol = 4)
  colnames(test_data) <- paste0("M", 1:4)
  
  decomp <- waveqte_decompose(test_data, scales = 1:2)
  qte_result <- calculate_qte(decomp, quantiles = c(0.95), min_obs = 5)
  networks <- adaptive_threshold(qte_result)
  
  # Construct network
  g <- construct_network(networks, scale = 1, quantile = 0.95)
  
  # Check that it's an igraph object
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 4)
  
  # Check vertex attributes
  expect_true("name" %in% igraph::vertex_attr_names(g))
  expect_true("market" %in% igraph::vertex_attr_names(g))
  
  # Check graph attributes
  expect_equal(igraph::graph_attr(g, "scale"), 1)
  expect_equal(igraph::graph_attr(g, "quantile"), 0.95)
})

# Test centrality calculation
test_that("calculate_centrality works correctly", {
  
  # Create test networks
  set.seed(123)
  test_data <- matrix(rnorm(200, 0, 0.02), nrow = 50, ncol = 4)
  colnames(test_data) <- paste0("M", 1:4)
  
  decomp <- waveqte_decompose(test_data, scales = 1:2)
  qte_result <- calculate_qte(decomp, quantiles = c(0.95), min_obs = 5)
  networks <- adaptive_threshold(qte_result)
  
  # Calculate centrality
  centrality <- calculate_centrality(networks)
  
  # Check structure
  expect_s3_class(centrality, "waveqte_centrality")
  expect_true(is.data.frame(centrality))
  
  # Check required columns
  required_cols <- c("market", "scale", "quantile", "degree_in", "degree_out", 
                    "degree_total", "eigenvector", "betweenness", "composite_centrality")
  expect_true(all(required_cols %in% names(centrality)))
  
  # Check that all markets and scales are represented
  expect_equal(length(unique(centrality$market)), 4)
  expect_equal(length(unique(centrality$scale)), 2)
  
  # Check that centrality values are non-negative
  expect_true(all(centrality$degree_total >= 0))
  expect_true(all(centrality$eigenvector >= 0))
  expect_true(all(centrality$betweenness >= 0))
})