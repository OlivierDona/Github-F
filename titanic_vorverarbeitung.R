library(stringr)
library(dplyr)

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

# Imputation der fehlenden Werte in "Age" mithilfe der Anrede
titanic_data$Age = apply(which(titanic_data$Age == 0),1,function(x){
  if(titanic_data$Anrede == "Ms." | titanic_data$Age == "Miss." | titanic_data$Age == "Mlle."){
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