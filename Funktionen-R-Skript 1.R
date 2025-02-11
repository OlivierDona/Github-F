##2ai) Funktion für deskriptive Statistiken metrischer Variablen

decribe_metric <- function(data,variables) {
  summary_stats <- data %>%
    select(all_of(varaiables)) %>%
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



##2aii) Funktion für deskriptive Statistiken kategorialer Variablen


describe_categorical <- function(data, variables) {
  stats <- list()
  
  for (var in variables) {
    stats[[var]] <- data %>%
      group_by(!!sym(var)) %>%
      summarise(Anzahl = n(), Anteil = n() / nrow(data))
  }
  
  return(stats)
}



##2a(iii) Funktion für bivariate Analyse (Zusammenhang zwischen zwei kategorialen Variablen)

bivariate_categorical <- function(data, var1, var2) {
  table_result <- table(data[[var1]], data[[var2]])
  chi_test <- chisq.test(table_result)
  
  return(list(Tabelle = table_result, Chi_Quadrat_Test = chi_test))
}



##2a(iv) Funktion für bivariate Analyse (Zusammenhang zwischen metrischer und dichotomer Variable)

bivariate_metric_dichotomous <- function(data, metric_var, dichotomous_var) {
  t_test_result <- t.test(data[[metric_var]] ~ data[[dichotomous_var]], data = data, na.action = na.omit)
  
  return(t_test_result)
}
