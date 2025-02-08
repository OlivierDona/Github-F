library(stringr)
library(dplyr)
library(tidyr)


titanic_data = read.csv("titanic.csv",header = TRUE, sep = c(",","„"))
#Extrahieren der Anrede aus den Namen
anreden = c("Mrs.","Miss.","Mlle","Ms.","Mr.","Master.","Don.")
titanic_data = titanic_data %>% 
  mutate(Anrede = str_extract(Name, paste(anreden, collapse = "|")))

# "Survived","Sex" und "Embarked" als Faktor
titanic_data$Survived = as.factor(titanic_data$Survived)
titanic_data$Sex = as.factor(titanic_data$Sex)
titanic_data$Embarked = as.factor(titanic_data$Embarked)

# "Pclass" als ordered-factor
titanic_data$Pclass = as.ordered(titanic_data$Pclass)

# Imputation der fehlenden Werte in "Age" mithilfe der Anreden
na_indices = which(is.na(titanic_data$Age))
titanic_data$Age[na_indices] = apply(as.matrix(na_indices),1,function(x){
  if(any(titanic_data$Anrede == "Miss",titanic_data$Anrede == "Ms.", titanic_data$Anrede == "Mlle.")){
    titanic_data$Age[x]= median(titanic_data$Age[titanic_data$Anrede == "Ms." | 
                                               titanic_data$Anrede == "Miss." | 
                                  titanic_data$Anrede == "Mlle."],na.rm = TRUE)}
  else if(titanic_data$Anrede == "Mrs."){
    titanic_data$Age[x]= median(titanic_data$Age[titanic_data$Anrede == "Mrs."],na.rm = TRUE)}
  else if(titanic_data$Anrede == "Mr."){
    titanic_data$Age[x]= median(titanic_data$Age[titanic_data$Anrede == "Mr."],na.rm = TRUE)}
  else if(titanic_data$Anrede == "Master."){
    titanic_data$Age[x]= median(titanic_data$Age[titanic_data$Anrede == "Master."],na.rm = TRUE)}
  else if(titanic_data$Anrede == "Don."){
    titanic_data$Age[x]= median(titanic_data$Age[titanic_data$Anrede == "Don."],na.rm = TRUE)}
}
)
  # Extrahieren von Kabineninformationen 

  titanic_data$Deck <- ifelse(!is.na(titanic_data$Cabin) & titanic_data$Cabin != "", substr(titanic_data$Cabin, 1, 1), NA)
  titanic_data$Side <- ifelse(!is.na(titanic_data$Cabin) & titanic_data$Cabin != "", ifelse(as.numeric(str_extract(titanic_data$Cabin, "[0-9]+")) %% 2 == 1, "Starboard", "Port"), NA)
 
 # Entfernen nicht benötigter Variablen - ID, Name, Ticket, Cabin
  titanic_data <- select(titanic_data, -PassengerId, -Name, -Ticket, -Cabin)
# Speichern des bereinigten Datensatzes
 write.csv(titanic_data, "titanic_cleaned.csv", row.names = FALSE)

# Speichern des Skripts
 dump("titanic", file = "titanic_preprocessing.R")


