#source("C:/Users/UNI/Desktop/RWD/Funktionen-R-Skript 2.R")
source("Funktionen-R-Skript 2.R")
##Funktion für deskriptive Statistiken metrischer Variablen
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)#!!!

#' Funktion für deskriptive Statistiken metrischer Variablen
#'
#' Diese Funktion berechnet verschiedene deskriptive Statistiken für metrische Variablen,
#' darunter Mittelwert, Median, Standardabweichung, Minimum, Maximum und Quartile.
#'
#' @param data Ein DataFrame mit den zu analysierenden Variablen.
#' @param variables Eine Zeichenfolgenliste der zu analysierenden metrischen Variablen.
#'
#' @return Ein DataFrame mit deskriptiven Statistiken für jede Variable.
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

#' Funktion für deskriptive Statistiken kategorialer Variablen
#'
#' Diese Funktion berechnet die Häufigkeit und den Anteil jeder Kategorie
#' innerhalb einer oder mehrerer kategorialer Variablen.
#'
#' @param data Ein DataFrame mit den zu analysierenden Variablen.
#' @param variables Eine Zeichenfolgenliste der zu analysierenden kategorialen Variablen.
#'
#' @return Eine Liste mit Häufigkeitsverteilungen für jede Variable.
describe_categorical <- function(data, variables) {
  stats <- list()
  
  for (var in variables) {
    stats[[var]] <- data %>%
      group_by(!!sym(var)) %>%
      summarise(Anzahl = n(), Anteil = n() / nrow(data))
  }
  
  return(stats)
}

#' Funktion für bivariate Analyse zwischen zwei kategorialen Variablen
#'
#' Diese Funktion erstellt eine Kreuztabelle für zwei kategoriale Variablen
#' und führt einen Chi-Quadrat-Test durch, um den statistischen Zusammenhang zu bewerten.
#'
#' @param data Ein DataFrame mit den zu analysierenden Variablen.
#' @param var1 Die erste kategoriale Variable.
#' @param var2 Die zweite kategoriale Variable.
#'
#' @return Eine Liste mit der Kreuztabelle und den Ergebnissen des Chi-Quadrat-Tests.
bivariate_categorical <- function(data, var1, var2) {
  table_result <- table(data[[var1]], data[[var2]])
  chi_test <- chisq.test(table_result)
  
  return(list(Tabelle = table_result, Chi_Quadrat_Test = chi_test))
}

#' Funktion für bivariate Analyse zwischen einer metrischen und einer dichotomen Variable
#'
#' Diese Funktion führt einen t-Test durch, um zu prüfen, ob ein signifikanter Unterschied
#' zwischen den Mittelwerten einer metrischen Variable für zwei Gruppen einer dichotomen Variable besteht.
#'
#' @param data Ein DataFrame mit den zu analysierenden Variablen.
#' @param metric_var Die metrische Variable.
#' @param dichotomous_var Die dichotome Variable.
#'
#' @return Das Ergebnis des t-Tests.
bivariate_metric_dichotomous <- function(data, metric_var, dichotomous_var) {
  t_test_result <- t.test(data[[metric_var]] ~ data[[dichotomous_var]], data = data, na.action = na.omit)
  
  return(t_test_result)
}

#' Funktion zur Visualisierung von 3-4 kategorialen Variablen
#'
#' Diese Funktion erstellt eine proportionale Balkendiagramm-Darstellung
#' für bis zu vier kategoriale Variablen.
#'
#' @param df Ein DataFrame mit den zu analysierenden Variablen.
#' @param var1 Die erste kategoriale Variable (X-Achse).
#' @param var2 Die zweite kategoriale Variable (Farbfüllung).
#' @param var3 Die dritte kategoriale Variable (Facetten).
#' @param var4 (Optional) Die vierte kategoriale Variable.
#'
#' @return Ein ggplot-Objekt mit dem erstellten Diagramm.
plot_categorical <- function(df, var1, var2, var3, var4 = NULL) {
  vars <- c(var1, var2, var3, var4)
  vars <- vars[!sapply(vars, is.null)]  
  
  prop_table <- create_prop_table(df, vars)
  
  p <- ggplot(prop_table, aes_string(x = var1, y = "prop", fill = var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(paste(var3, "~", ifelse(is.null(var4), ".", var4))) +
    labs(y = "Proportion", fill = var2) +
    theme_minimal()
  
  print(p)
}

#' Funktion für ein Streudiagramm mit Regressionsgerade für zwei metrische Variablen
#'
#' Diese Funktion erstellt ein Streudiagramm mit einer Regressionsgerade
#' und zeigt die berechnete Korrelation zwischen den beiden Variablen an.
#'
#' @param data Ein DataFrame mit den zu analysierenden Variablen.
#' @param var1 Die erste metrische Variable (X-Achse).
#' @param var2 Die zweite metrische Variable (Y-Achse).
#'
#' @return Ein ggplot-Objekt mit dem erstellten Diagramm.
plot_metric_correlation <- function(data,var1,var2){
  corr_as_text = calc_correlation(data[[var1]],data[[var2]])
  
  p = ggplot(data,aes_string(x = var1,y = var2)) + geom_point(color = "blue") + 
    geom_smooth(method = "lm",color = "red") + 
    annotate("text",x = Inf, y = Inf, label = corr_as_text, hjust = 1, vjust = 1.5, size = 5) + 
    labs(title = paste("Korrelation zwischen",var1,"und",var2), x = var1, y = var2) +
    theme_minimal()
  
  print(p)
}
#' Klassifizierung der Passagiere basierend auf der Familiengröße
#'
#' Diese Funktion fügt dem Datensatz zwei neue Spalten hinzu: `FamilySize`, 
#' die die Anzahl der Familienmitglieder an Bord (einschließlich des Passagiers) darstellt,
#' und `FamilyType`, die die Kategorie der Familiengröße angibt.
#'
#' @param data Ein DataFrame, der die Passagierdaten enthält und die Spalten `SibSp` (Anzahl der Geschwister/Ehepartner an Bord) 
#' und `Parch` (Anzahl der Eltern/Kinder an Bord) umfasst.
#'
#' @return Ein DataFrame mit den hinzugefügten Spalten `FamilySize` und `FamilyType`.
#'
#' @details
#' Die Funktion berechnet die Gesamtzahl der Familienmitglieder an Bord, indem sie die Werte von `SibSp` und `Parch` addiert 
#' und 1 für den Passagier selbst hinzufügt. Basierend auf der resultierenden Familiengröße wird die Familie in eine der 
#' folgenden Kategorien eingeteilt:
#' - `Single`: Alleinreisende (1 Person)
#' - `Small`: Kleine Familie (2-3 Personen)
#' - `Medium`: Mittlere Familie (4-5 Personen)
#' - `Large`: Große Familie (mehr als 5 Personen)
classify_family_type <- function(data) {
  # Erstellen der Variablen FamilySize (Anzahl der Familienmitglieder an Bord + Passagier selbst)
  data$FamilySize <- data$SibSp + data$Parch + 1
  
  # Klassifizierung der Familiengröße in FamilyType
  data$FamilyType <- cut(
    data$FamilySize,
    breaks = c(0, 1, 3, 5, Inf),
    labels = c("Single", "Small", "Medium", "Large"),
    right = TRUE
  )
  
  # Rückgabe des aktualisierten DataFrames
  return(data)
}


