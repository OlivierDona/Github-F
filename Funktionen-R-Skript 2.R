# Hilfsfunktion zum Erstellen einer Häufigkeitstabelle 
create_prop_table <- function(df, vars) {
  df %>%
    count(across(all_of(vars))) %>%
    group_by(across(all_of(vars[-length(vars)]))) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
}

# Hilfsfunktion zur Berechnung der Korrelation und Rückgabe als Text
calc_correlation <- function(x, y) {
  corr_value <- cor(x, y, use = "complete.obs")
  return(paste0("Korrelation: r = ", round(corr_value, 2)))
}

