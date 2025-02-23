#' Hilfsfunktion zum Erstellen einer Häufigkeitstabelle
#'
#' Diese Funktion erstellt eine Häufigkeitstabelle für eine oder mehrere kategoriale Variablen
#' und berechnet die relative Häufigkeit jeder Kategorie.
#'
#' @param df Ein DataFrame mit den zu analysierenden Variablen.
#' @param vars Eine Zeichenfolgenliste der kategorialen Variablen, für die eine Häufigkeitsanalyse durchgeführt wird.
#'
#' @return Ein DataFrame mit absoluten und relativen Häufigkeiten.
create_prop_table <- function(df, vars) {
  df %>%
    count(across(all_of(vars))) %>%
    group_by(across(all_of(vars[-length(vars)]))) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
}

#' Hilfsfunktion zur Berechnung der Korrelation und Rückgabe als Text
#'
#' Diese Funktion berechnet die Pearson-Korrelation zwischen zwei metrischen Variablen
#' und gibt das Ergebnis als formatierten Text zurück.
#'
#' @param x Die erste metrische Variable.
#' @param y Die zweite metrische Variable.
#'
#' @return Eine Zeichenfolge mit der berechneten Korrelation.
calc_correlation <- function(x, y) {
  corr_value <- cor(x, y, use = "complete.obs")
  return(paste0("Korrelation: r = ", round(corr_value, 2)))
}
