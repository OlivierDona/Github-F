# Function 1: Plot a histogram for a single variable with dynamic title and axis labels
plot_histogram <- function(data, var_name) {
  library(ggplot2)
  ggplot(data, aes_string(x = var_name)) +  # Use aes_string to reference the column by name
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(
      x = var_name,  # X-axis label as the variable name
      y = "Frequency"  # Y-axis label is set to "Frequency"
    )
}
