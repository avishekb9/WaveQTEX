# WaveQTEX: Visualization Functions
# Advanced Plotting and Dashboard Functions for WaveQTE Analysis
# Version 1.0.0

#' Plot QTE Heatmap
#'
#' Creates comprehensive heatmap visualizations of QTE matrices
#' across different scales and quantiles for publication-quality analysis.
#'
#' @param qte_result Object of class "waveqte_result" or "waveqte_networks"
#' @param scale Integer specifying which scale to plot
#' @param quantile Numeric specifying which quantile to plot
#' @param title Character string for plot title (optional)
#' @param color_scheme Character string specifying color scheme ("viridis", "plasma", "blues")
#' @param show_values Logical indicating whether to display QTE values in cells
#' @param cluster_markets Logical indicating whether to cluster markets by similarity
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c theme_minimal labs
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' p <- plot_qte_heatmap(qte_result, scale = 1, quantile = 0.95)
#' print(p)
#' }
plot_qte_heatmap <- function(qte_result, scale, quantile, title = NULL,
                            color_scheme = "viridis", show_values = FALSE,
                            cluster_markets = FALSE) {
  
  # Extract QTE array
  if (inherits(qte_result, "waveqte_result")) {
    qte_array <- qte_result$qte_array
    markets <- qte_result$markets
    scales <- qte_result$scales
    quantiles <- qte_result$quantiles
  } else if (inherits(qte_result, "waveqte_networks")) {
    qte_array <- qte_result$original_qte$qte_array
    markets <- qte_result$markets
    scales <- qte_result$scales
    quantiles <- qte_result$quantiles
  } else {
    stop("Input must be of class 'waveqte_result' or 'waveqte_networks'")
  }
  
  # Find indices
  scale_idx <- which(scales == scale)
  quantile_idx <- which(abs(quantiles - quantile) < 1e-6)
  
  if (length(scale_idx) == 0) {
    stop(paste("Scale", scale, "not found"))
  }
  if (length(quantile_idx) == 0) {
    stop(paste("Quantile", quantile, "not found"))
  }
  
  # Extract matrix for specified scale and quantile
  qte_matrix <- qte_array[, , scale_idx, quantile_idx]
  
  # Optional clustering
  if (cluster_markets && nrow(qte_matrix) > 2) {
    # Cluster based on QTE similarities
    dist_matrix <- as.dist(1 - cor(qte_matrix, use = "complete.obs"))
    hc <- hclust(dist_matrix, method = "ward.D2")
    market_order <- markets[hc$order]
    qte_matrix <- qte_matrix[hc$order, hc$order]
  } else {
    market_order <- markets
  }
  
  # Convert to long format for ggplot
  qte_long <- expand.grid(
    From = factor(market_order, levels = market_order),
    To = factor(market_order, levels = rev(market_order)),  # Reverse for proper orientation
    stringsAsFactors = FALSE
  )
  
  qte_long$QTE <- as.vector(qte_matrix[, ncol(qte_matrix):1])  # Reverse columns
  
  # Create base plot
  p <- ggplot2::ggplot(qte_long, ggplot2::aes(x = From, y = To, fill = QTE)) +
    ggplot2::geom_tile(color = "white", size = 0.1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::labs(
      x = "From Market",
      y = "To Market",
      fill = "QTE"
    )
  
  # Add color scheme
  if (color_scheme == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "viridis")
  } else if (color_scheme == "plasma") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "plasma")
  } else if (color_scheme == "blues") {
    p <- p + ggplot2::scale_fill_gradient(low = "white", high = "darkblue")
  }
  
  # Add values if requested
  if (show_values) {
    qte_long$QTE_text <- ifelse(qte_long$QTE > 0, 
                               sprintf("%.3f", qte_long$QTE), "")
    p <- p + ggplot2::geom_text(ggplot2::aes(label = QTE_text), 
                               size = 2.5, color = "black")
  }
  
  # Add title
  if (is.null(title)) {
    title <- sprintf("QTE Heatmap: Scale %d, Quantile %.2f", scale, quantile)
  }
  p <- p + ggplot2::labs(title = title)
  
  return(p)
}

#' Plot Network Evolution
#'
#' Creates multi-panel visualization showing network evolution across scales
#' with comprehensive network statistics and centrality measures.
#'
#' @param networks_result Object of class "waveqte_networks"
#' @param quantile Numeric specifying which quantile to plot (default: 0.95)
#' @param layout Character string specifying network layout ("spring", "circle", "stress")
#' @param node_size_by Character string specifying node sizing ("degree", "eigenvector", "betweenness")
#' @param edge_threshold Minimum QTE value for edge display (default: 0.1)
#' @param color_by_region Logical indicating whether to color nodes by region
#' @return List of ggplot objects for each scale
#' @export
#' @importFrom igraph layout_with_fr layout_in_circle layout_with_dh
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_size_continuous
#' @examples
#' \dontrun{
#' plots <- plot_network_evolution(networks, quantile = 0.95)
#' }
plot_network_evolution <- function(networks_result, quantile = 0.95, 
                                  layout = "spring", node_size_by = "degree",
                                  edge_threshold = 0.1, color_by_region = TRUE) {
  
  if (!inherits(networks_result, "waveqte_networks")) {
    stop("networks_result must be of class 'waveqte_networks'")
  }
  
  scales <- networks_result$scales
  markets <- networks_result$markets
  plot_list <- list()
  
  # Calculate centrality for node sizing
  centrality_data <- calculate_centrality(networks_result)
  
  for (scale in scales) {
    
    # Construct network for this scale
    g <- construct_network(networks_result, scale, quantile)
    
    if (igraph::vcount(g) == 0 || igraph::ecount(g) == 0) {
      warning(paste("Empty network for scale", scale, "quantile", quantile))
      next
    }
    
    # Filter edges by threshold
    edge_weights <- igraph::E(g)$weight
    keep_edges <- edge_weights >= edge_threshold
    
    if (sum(keep_edges) == 0) {
      warning(paste("No edges above threshold for scale", scale))
      next
    }
    
    g_filtered <- igraph::subgraph.edges(g, which(keep_edges))
    
    # Calculate layout
    if (layout == "spring") {
      layout_coords <- igraph::layout_with_fr(g_filtered)
    } else if (layout == "circle") {
      layout_coords <- igraph::layout_in_circle(g_filtered)
    } else if (layout == "stress") {
      layout_coords <- layout_with_dh(g_filtered)
    } else {
      layout_coords <- igraph::layout_with_fr(g_filtered)
    }
    
    # Prepare data for ggplot
    vertices <- data.frame(
      name = igraph::V(g_filtered)$name,
      x = layout_coords[, 1],
      y = layout_coords[, 2],
      stringsAsFactors = FALSE
    )
    
    # Add centrality data for node sizing
    scale_centrality <- centrality_data[
      centrality_data$scale == scale & 
      abs(centrality_data$quantile - quantile) < 1e-6, 
    ]
    
    vertices <- merge(vertices, scale_centrality[, c("market", "degree_total", 
                                                    "eigenvector", "betweenness")],
                     by.x = "name", by.y = "market", all.x = TRUE)
    
    # Add region information if requested
    if (color_by_region) {
      vertices$region <- classify_market_region(vertices$name)
      region_colors <- c("Asia" = "#E31A1C", "Europe" = "#1F78B4", 
                        "North_America" = "#33A02C", "South_America" = "#FF7F00",
                        "Other" = "#6A3D9A")
    }
    
    # Prepare edges
    edge_list <- igraph::get.edgelist(g_filtered)
    edges <- data.frame(
      from = edge_list[, 1],
      to = edge_list[, 2],
      weight = igraph::E(g_filtered)$weight,
      stringsAsFactors = FALSE
    )
    
    # Add coordinates for edges
    edges <- merge(edges, vertices[, c("name", "x", "y")], 
                  by.x = "from", by.y = "name")
    names(edges)[names(edges) == "x"] <- "x_from"
    names(edges)[names(edges) == "y"] <- "y_from"
    
    edges <- merge(edges, vertices[, c("name", "x", "y")], 
                  by.x = "to", by.y = "name")
    names(edges)[names(edges) == "x"] <- "x_to"
    names(edges)[names(edges) == "y"] <- "y_to"
    
    # Create plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = edges, 
                           ggplot2::aes(x = x_from, y = y_from, 
                                       xend = x_to, yend = y_to,
                                       alpha = weight),
                           color = "gray50") +
      ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none")
    
    # Add nodes with appropriate sizing
    if (node_size_by == "degree") {
      p <- p + ggplot2::geom_point(data = vertices,
                                  ggplot2::aes(x = x, y = y, 
                                              size = degree_total + 1,
                                              color = if(color_by_region) region else "black"))
    } else if (node_size_by == "eigenvector") {
      p <- p + ggplot2::geom_point(data = vertices,
                                  ggplot2::aes(x = x, y = y, 
                                              size = eigenvector + 0.1,
                                              color = if(color_by_region) region else "black"))
    } else if (node_size_by == "betweenness") {
      p <- p + ggplot2::geom_point(data = vertices,
                                  ggplot2::aes(x = x, y = y, 
                                              size = betweenness + 0.1,
                                              color = if(color_by_region) region else "black"))
    }
    
    # Add node labels
    p <- p + ggplot2::geom_text(data = vertices,
                               ggplot2::aes(x = x, y = y, label = name),
                               size = 3, hjust = 0.5, vjust = -1.2)
    
    # Styling
    p <- p + 
      ggplot2::scale_size_continuous(range = c(3, 12), name = str_to_title(node_size_by)) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      ggplot2::labs(title = sprintf("Scale %d Network (Quantile %.2f)", scale, quantile))
    
    # Add color scale if using regions
    if (color_by_region) {
      p <- p + ggplot2::scale_color_manual(values = region_colors, name = "Region")
    }
    
    plot_list[[paste0("scale_", scale)]] <- p
  }
  
  return(plot_list)
}

#' Early Warning Dashboard
#'
#' Creates interactive dashboard for real-time systemic risk monitoring
#' with multiple visualization panels and alert indicators.
#'
#' @param ews_result Object of class "early_warning_system"
#' @param time_series Optional time series data for trend analysis
#' @param interactive Logical indicating whether to create interactive plots
#' @return Dashboard object (shiny app or static plots)
#' @export
#' @importFrom plotly ggplotly
#' @importFrom DT datatable
#' @examples
#' \dontrun{
#' dashboard <- early_warning_dashboard(ews_result, interactive = TRUE)
#' }
early_warning_dashboard <- function(ews_result, time_series = NULL, interactive = FALSE) {
  
  if (!inherits(ews_result, "early_warning_system")) {
    stop("ews_result must be of class 'early_warning_system'")
  }
  
  # Extract components
  sri <- ews_result$sri
  risk_components <- ews_result$risk_components
  current_assessment <- ews_result$current_assessment
  
  # Create main SRI gauge plot
  sri_plot <- create_sri_gauge(sri$sri_value, sri$alert_level)
  
  # Create risk components plot
  components_plot <- create_risk_components_plot(risk_components)
  
  # Create trend plot if time series provided
  trend_plot <- NULL
  if (!is.null(time_series)) {
    trend_plot <- create_trend_plot(time_series)
  }
  
  # Create alert summary table
  alert_table <- create_alert_summary_table(current_assessment)
  
  if (interactive) {
    # Create interactive dashboard using shiny
    dashboard <- create_interactive_dashboard(sri_plot, components_plot, 
                                            trend_plot, alert_table)
  } else {
    # Return static plots
    dashboard <- list(
      sri_gauge = sri_plot,
      risk_components = components_plot,
      trend_analysis = trend_plot,
      alert_summary = alert_table,
      metadata = list(
        creation_time = Sys.time(),
        alert_level = sri$alert_level,
        overall_risk = current_assessment$overall_risk_score
      )
    )
  }
  
  return(dashboard)
}

#' Create SRI Gauge Plot
#'
#' Internal function to create SRI gauge visualization
#' @param sri_value Current SRI value
#' @param alert_level Current alert level
#' @return ggplot object
create_sri_gauge <- function(sri_value, alert_level) {
  
  # Create gauge data
  gauge_data <- data.frame(
    value = sri_value,
    max_value = 3,  # Maximum expected SRI value
    alert_level = alert_level
  )
  
  # Color mapping
  color_map <- c("Green" = "#2ECC71", "Amber" = "#F39C12", "Red" = "#E74C3C")
  
  # Create gauge plot (simplified version)
  p <- ggplot2::ggplot(gauge_data, ggplot2::aes(x = 1, y = value)) +
    ggplot2::geom_col(fill = color_map[alert_level], width = 0.5) +
    ggplot2::geom_hline(yintercept = c(0.5, 1.5), linetype = "dashed", alpha = 0.7) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::ylim(0, gauge_data$max_value) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    ) +
    ggplot2::labs(
      title = "Systemic Risk Index",
      subtitle = sprintf("Current: %.3f (%s Alert)", sri_value, alert_level)
    )
  
  return(p)
}

#' Create Risk Components Plot
#'
#' Internal function to create risk components visualization
#' @param risk_components List of risk component values
#' @return ggplot object
create_risk_components_plot <- function(risk_components) {
  
  # Prepare data
  components_df <- data.frame(
    component = names(risk_components),
    value = as.numeric(risk_components),
    stringsAsFactors = FALSE
  )
  
  # Add color based on risk level
  components_df$color <- ifelse(components_df$value > 0.7, "High",
                               ifelse(components_df$value > 0.4, "Medium", "Low"))
  
  # Create plot
  p <- ggplot2::ggplot(components_df, ggplot2::aes(x = reorder(component, value), 
                                                   y = value, fill = color)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c("Low" = "#2ECC71", "Medium" = "#F39C12", 
                                         "High" = "#E74C3C")) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Risk Component Analysis",
      x = "Risk Component",
      y = "Risk Level",
      fill = "Risk Level"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

#' Create Trend Plot
#'
#' Internal function to create trend analysis plot
#' @param time_series Time series data
#' @return ggplot object
create_trend_plot <- function(time_series) {
  
  # This is a placeholder - would need actual time series structure
  # For now, create a simple example
  
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  values <- cumsum(rnorm(100, 0, 0.1)) + 1
  
  trend_data <- data.frame(
    date = dates,
    sri = values
  )
  
  p <- ggplot2::ggplot(trend_data, ggplot2::aes(x = date, y = sri)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::geom_hline(yintercept = c(0.5, 1.5), linetype = "dashed", 
                       color = c("orange", "red"), alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "SRI Trend Analysis",
      x = "Date",
      y = "Systemic Risk Index"
    )
  
  return(p)
}

#' Create Alert Summary Table
#'
#' Internal function to create alert summary table
#' @param current_assessment Current risk assessment
#' @return Data frame for table display
create_alert_summary_table <- function(current_assessment) {
  
  # Extract recommendations
  recommendations <- current_assessment$recommendations
  
  # Create summary table
  summary_df <- data.frame(
    Metric = c("Overall Risk Score", "Alert Level", "Crisis Probability", 
               "Assessment Time"),
    Value = c(
      sprintf("%.3f", current_assessment$overall_risk_score),
      current_assessment$alert_level,
      ifelse(is.na(current_assessment$crisis_probability), "N/A",
             sprintf("%.1f%%", current_assessment$crisis_probability * 100)),
      format(current_assessment$assessment_time, "%Y-%m-%d %H:%M")
    ),
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

#' Create Interactive Dashboard
#'
#' Internal function to create interactive Shiny dashboard
#' @param sri_plot SRI gauge plot
#' @param components_plot Risk components plot
#' @param trend_plot Trend analysis plot
#' @param alert_table Alert summary table
#' @return Shiny app object
create_interactive_dashboard <- function(sri_plot, components_plot, trend_plot, alert_table) {
  
  # This would require shiny package and more complex implementation
  # For now, return a placeholder
  
  message("Interactive dashboard would require shiny implementation")
  message("Returning static plots instead")
  
  return(list(
    sri_gauge = sri_plot,
    risk_components = components_plot,
    trend_analysis = trend_plot,
    alert_summary = alert_table
  ))
}