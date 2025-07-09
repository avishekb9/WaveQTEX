# Global variable bindings for R CMD check
# This file defines global variables used in non-standard evaluation

# Define global variables to avoid R CMD check NOTEs
globalVariables(c(
  # Variables from dplyr/tidyr operations
  "market",
  "window", 
  "degree_total",
  "eigenvector",
  "component", 
  "value",
  "color",
  "Period",
  "SRI",
  "final_priority",
  "intervention_rank",
  
  # Variables from ggplot2 operations
  "x_from", "y_from", "x_to", "y_to",
  "x", "y", "weight",
  "region", "name",
  "From", "To", "QTE", "QTE_text",
  "sri",
  
  # Variables from machine learning operations
  "from_market", "to_market", "qte_value",
  "cross_scale_mean", "cross_quantile_mean"
))