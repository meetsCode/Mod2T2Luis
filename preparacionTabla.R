
# lo que hago aquí es observar los datos gráfica y visualmente para 
# limpiarlos.
# solo cambio nombre de columnas, busco instancias (líneas) erróneas y 
# las borro o arreglo
#### EJERCICIO. Observacion/adaptacion de datos ####
preparacionDatos <- function(dataRAW){
  dataNew <- dataRAW
  colnames(dataNew) <- c("ProductType","Product","Price",                        
                         "x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
                         "x1StarReviews","X.Positive.Service.Review.",
                         "NegativeServiceReview","WouldConsumerRecommendProduct","BestSellersRank",
                         "ShippingWeightLbs","ProductDepth","ProductWidth","ProductHeight",
                         "ProfitMargin","Volume")
  
  str(dataNew)
  #veo que BestSellersRank ha quedado como sting cuando debería ser num. 
  #Eso es porque debe haber vacíos. Los transformo
  dataNew$BestSellersRank <- as.numeric(dataNew$BestSellersRank)
  #me aviso que NAs introducidos por coerción. Los Reparo(relleno) con la media
  dataNew$BestSellersRank[is.na(dataNew$BestSellersRank)] <- mean(dataNew$BestSellersRank, na.rm = TRUE)
  
  #pienso que ProductType es un Factor:
  dataNew$ProductType <- factor(dataNew$ProductType)
  
  return(dataNew)
}

seleccionFilas <- function(dataRAW){
  #veo que algunas instancias tienen datos trampa.
  #Son datos que alguien ha repetido porque son iguales en todos campos
  filasDeTrampas <- -41:-35
  dataNew <- dataRAW[filasDeTrampas, ]
  return(dataNew)
}

data <- preparacionDatos(dataRaw)
data <- seleccionFilas(data)
str(data)