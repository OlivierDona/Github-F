##Funktion für deskriptive Statistiken metrischer Variablen

describe_metric <- function(data,variables) {
  summary_stats <- data %>%
    select(all_of(variables)) %>%
    summarise_all(list(
      Mittelwert = mean,
      Median = median,
      Standardabweichung = sd,
      Minimum = min,
      Maximun = max,
      Q1 = ~quantile(., 0.25), 
      Q3 = ~quantile(., 0.75)
    ), na.rm = TRUE)
  
  return(summary_stats)
}



##Funktion für deskriptive Statistiken kategorialer Variablen


describe_categorical <- function(data, variables) {
  stats <- list()
  
  for (var in variables) {
    stats[[var]] <- data %>%
      group_by(!!sym(var)) %>%
      summarise(Anzahl = n(), Anteil = n() / nrow(data))
  }
  
  return(stats)
}



##Funktion für bivariate Analyse (Zusammenhang zwischen zwei kategorialen Variablen)

bivariate_categorical <- function(data, var1, var2) {
  table_result <- table(data[[var1]], data[[var2]])
  chi_test <- chisq.test(table_result)
  
  return(list(Tabelle = table_result, Chi_Quadrat_Test = chi_test))
}



##Funktion für bivariate Analyse (Zusammenhang zwischen metrischer und dichotomer Variable)

bivariate_metric_dichotomous <- function(data, metric_var, dichotomous_var) {
  t_test_result <- t.test(data[[metric_var]] ~ data[[dichotomous_var]], data = data, na.action = na.omit)
  
  return(t_test_result)
}

## Funktion zur Visualisierung von 3-4 kategorialen Variablen
plot_categorical <- function(df, var1, var2, var3, var4 = NULL) {
  vars <- c(var1, var2, var3, var4)
  vars <- vars[!sapply(vars, is.null)]  # Entferne NULL-Werte
  
  # Erstelle proportionale Tabelle
  prop_table <- create_prop_table(df, vars)
  
  # Erstelle Plot
  p <- ggplot(prop_table, aes_string(x = var1, y = "prop", fill = var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(paste(var3, "~", ifelse(is.null(var4), ".", var4))) +
    labs(y = "Proportion", fill = var2) +
    theme_minimal()
  
  print(p)
}

## Funktion für ein Streudiagramm mit Regressionsgerade für zwei metrische Variablen
plot_metric_correlation <- function(data,var1,var2){
  corr_as_text = calc_correlation(data[[var1]],data[[var2]])
  
  p = ggplot(data,aes_string(x = var1,y = var2)) + geom_point(color = "blue") + 
               geom_smooth(method = "lm",color = "red") + 
               annotate("text",x = Inf, y = Inf, label = corr_as_text, hjust = 1, vjust = 1.5, size = 5) + 
    labs(title = paste("Korrelation zwischen",var1,"und",var2), x = var1, y = var2) +
    theme_minimal()
  
  print(p)
}
