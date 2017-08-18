


#### EJERCICIO. Modelización ####
trainSize <- round(nrow(data) * 0.7 )
testSize <- nrow(data) - trainSize
set.seed(123)
trainPosition <- sample(x = nrow(data), size = trainSize)
trainSet <- data[trainPosition,]
testSet <- data[-trainPosition,]

modelizo_lm_BlackWell <- function(datasource){
  #Lineal model (Regression linear)
  model <- lm(formula = datasource$Volume ~ . , data = datasource)
  summary(model)
  return(model)
}
modelizo_svm_BlackWell <- function(datasource){
  #Supported Vector Machines
  #install.packages("e1071")  #solo la primera vez. Las siguientes molesta pero no impide
  library(e1071)
  model <- svm(formula = datasource$Volume ~ . , data = datasource)
  summary(model)
  return(model)
}
modelizo_RF_BlackWell <- function(datasource){
  #Random Forest
  #install.packages("randomForest")  #solo la primera vez. Las siguientes molesta pero no impide
  library(randomForest)
  model <- randomforest(formula = datasource$Volume ~ . , data = datasource)
  summary(model)
  return(model)
}

modelo <- modelizo_lm_BlackWell(data)
summary(modelo)

