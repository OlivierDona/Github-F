#Working directory auf Aktuellen pfad setzen!
source("Funktionen-R-Skript 1.R")

titanic_data = read.csv("titanic_cleaned.csv",
                        header = TRUE,
                        sep = c(",", "„"))
titanic_data <- classify_family_type(titanic_data)
#Deskription der Metrischen Variablen
describe_metric(titanic_data, "SibSp")
describe_metric(titanic_data, "Parch")
describe_metric(titanic_data, "Age")
describe_metric(titanic_data, "Fare")

##Visualisierung der Metrischen Stetigen Variablen
par(mfrow = c(1, 2))
boxplot(titanic_data$Age, main = "Age")
boxplot(titanic_data$Fare, main = "Fare")

#Betrachte mögliche interessante Korrelationen
plot_metric_correlation(titanic_data, "Age", "SibSp")
plot_metric_correlation(titanic_data, "Age", "Parch")
plot_metric_correlation(titanic_data, "Age", "Fare")

##Deskription der Kategorischen Variablen/Nicht Metrischen Variablen
describe_categorical(titanic_data, "Sex")
describe_categorical(titanic_data, "Anrede")
describe_categorical(titanic_data, "Embarked")
describe_categorical(titanic_data, "Deck")
describe_categorical(titanic_data, "Side")
describe_categorical(titanic_data, "Survived")#Binär, daher als Kategorisch betrachtet
describe_categorical(titanic_data, "Pclass")#Ordinal, daher als Kategorisch betrachtet
describe_categorical(titanic_data, "FamilyType")
#Visualisierung von kategorischen Zusammenhängen
plot_categorical(titanic_data, "Sex", "Anrede", "Survived")#
plot_categorical(titanic_data, "Pclass", "Deck", "Side", "Survived")

##Bivariater Vergleichen mit Überlebensrate
bivariate_metric_dichotomous(titanic_data, "Fare", "Survived")
bivariate_metric_dichotomous(titanic_data, "Age", "Survived")
bivariate_metric_dichotomous(titanic_data, "SibSp", "Survived")

##Bivariater Vergleichen
bivariate_categorical(titanic_data, "Survived", "Sex")
bivariate_categorical(titanic_data,  "Survived", "FamilyType")

