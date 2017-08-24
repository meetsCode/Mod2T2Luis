

#### EJERCICIO. Absorción y preparación de los datos ####
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/absorcionYPreparacionDatos.R", echo = TRUE)


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
  model <- randomForest(formula = datasource$Volume ~ . , data = datasource, 
                        importance=TRUE,proximity=TRUE)

  summary(model)
  return(model)
}


modelo <- modelizo_lm_BlackWell(data)
summary(modelo)

#predictions <- predict( modelo, trainSet, interval = "predict", level = 0.95)
#comparison <- predictions


#### EJERCICIO.  pruebo el trainSet  ####
comprobando <- function(modeloM, datos){
  predictions <- predict( modeloM, datos, interval = "predict", level = 0.95)
  comparison <- cbind(datos, predictions)
  return(comparison)
}
addComparativa <- function(comparison){
  comparacion <- comparison
  posFit <- which(colnames(comparacion) == "fit")
  colnames(comparacion)[posFit] <- "predictions"
  
  comparacion$absoluteDiff <- abs(comparacion$Volume - comparacion$predictions)
  comparacion$absolutePercentError <- abs(comparacion$Volume - comparacion$predictions) / abs(comparacion$Volume)
  comparacion$absolutePercentError <- comparacion$absolutePercentError * 100
  return(comparacion)
}
mi_mape <- function(unDataSet){
  #El dataSet debe tener una columna llamada absolutePercentError
  #mape (:= mean absolute percent error)
  #encuentro las filas con valor "Inf" (infinito, las que tenían numerador=0) que solo dan problemas
  infinitePosition <- which(unDataSet$absolutePercentError == Inf)
  infinitePosition <- infinitePosition * -1
  #los elimino
  # return(length(mape) == 0)
  if (length(infinitePosition) == 0) {
    dataDeTrabajo <- unDataSet
  } else {
    dataDeTrabajo <- unDataSet[infinitePosition, ]
  }
  
  #calculo el mape con el resto
  mape <- sum(dataDeTrabajo$absolutePercentError) / nrow(dataDeTrabajo)
  
  return(mape)
}

comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)



#### EJERCICIO.  pruebo el testSet  #### 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)

mapeTest
mapeTrain




