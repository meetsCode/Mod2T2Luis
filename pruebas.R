
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

predict( modelo, data, interval = "predict", level = 0.95)
comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)


