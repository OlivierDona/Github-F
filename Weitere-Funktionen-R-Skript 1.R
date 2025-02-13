

# Function 2: Plot histograms for each variable in the dataframe and show them inside a single plot
plot_histograms_for_all <- function(data, var_names) {
  library(ggplot2)
  library(gridExtra)
  source('Funktionen-R-Skript 2.R')
  
  # Create a list of plots
  plots <- lapply(var_names, function(var_name) {
    plot_histogram(data, var_name)  # Call plot_histogram for each column name
  })
  
  # Combine all plots into one using gridExtra::grid.arrange
  do.call(grid.arrange, c(plots, ncol = 2))  # Adjust ncol to change the number of columns in the grid
}
 


