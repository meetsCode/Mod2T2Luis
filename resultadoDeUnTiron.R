

# comienzo ejercicio Modulo 2 task 2 --------------------------------------


dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#como no pueden empezar por número me pone una X delante. 
#Los espacios en blanco los pasó a puntos.
#Ahora les quito los espacios en blanco.

colnames(dataRaw) <- c("ProductType","Product","Price",                        
                    "x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
                    "x1StarReviews","X.Positive.Service.Review.",
                    "NegativeServiceReview","WouldConsumerRecommendProduct","BestSellersRank",
                    "ShippingWeightLbs","ProductDepth","ProductWidth","ProductHeight",
                    "ProfitMargin","Volume")
data <- dataRaw


#### EJERCICIO. Observacion/cambios datos ####
str(data)
#veo que BestSellersRank ha quedado como sting cuando debería ser num. 
#Eso es porque debe haber vacíos. 

#Trato el campo BestSellersRank
data$BestSellersRank <- as.numeric(data$BestSellersRank)
#me aviso que NAs introducidos por coerción.
#Queda reparados los NA
data$BestSellersRank[is.na(data$BestSellersRank)] <- mean(data$BestSellersRank, na.rm = TRUE)

#pienso que ProductType es un Factor:
data$ProductType <- factor(data$ProductType)

#La columna product es un código interno sin relación con el código.
data$Product <- NULL



#### EJERCICIO. evitar Overfitting ####
dataSoloNumericos <- data[,2:17] #me daba error con una columna de string. La elimino
correlacion <- cor(dataSoloNumericos)
#observo y decido que sobran: Todo aquello que tiene correlación >0.85
#Son: 5Star, 3star, 1 star, NegativeService: Columnas 2,4,6 y 8 +1 por la primera 
#eliminada
columnasDeOverfit <- c(-3, -4, -5, -7, -9, -11)
dataDefinitivos <- data[, columnasDeOverfit] #El -4 es 4S. ¿Pierdo mucha información o no?
filasDeTrampas <- -35:-41
dataDefinitivos <- data[filasDeTrampas, ]
correlacion <- cor(dataDefinitivos[,-1]) #para echar un vistazo con el resultado.



# 
# plot(data$ProductType)
# plot(data$Price)
# 
# plot(data$x5StarReviews)
# plot(data$x4StarReviews)
# 
# plot(data$x3StarReviews)
# plot(data$x2StarReviews)
# 
# plot(data$x1StarReviews)
# plot(data$X.Positive.Service.Review.)
#                        
# 
# plot(data$NegativeServiceReview)
# plot(data$WouldConsumerRecommendProduct)
# 
# plot(data$BestSellersRank)
# plot(data$ShippingWeightLbs)
# 
# plot(data$ProductDepth)
# plot(data$ProductWidth)
# 
# plot(data$ProductHeight)
# plot(data$ProfitMargin)
# 
# plot(data$Volume)






#### EJERCICIO. Modelización ####
trainSize <- round(nrow(dataDefinitivos) * 0.7 )
testSize <- nrow(dataDefinitivos) - trainSize
set.seed(123)
trainPosition <- sample(x = nrow(dataDefinitivos), size = trainSize)
trainSet <- dataDefinitivos[trainPosition,]
testSet <- dataDefinitivos[-trainPosition,]

modelizo_lm_BlackWell <- function(datasource){
  model <- lm(formula = datasource$Volume ~ . , data = datasource)
  summary(model)
  return(model)
}

model <- modelizo_lm_BlackWell(dataDefinitivos)
summary(model)



#### EJERCICIO.  pruebo el trainSet  #### 
comprobando <- function(modelo, datos){
  predictions <- predict(modelo, datos, interval = "predict", level = 0.95)
  comparison <- cbind(datos, predictions)
  
  posFit <- which(colnames(comparison) == "fit") 
  colnames(comparison)[posFit] <- "predicted"
  
  comparison$absoluteDiff <- (comparison$Volume - comparison$predicted)
  absolutePercentError <- abs(comparison$Volume - comparison$predicted) / 
    abs(comparison$Volume)
  comparison$absolutePercentError <- absolutePercentError * 100
  
  return(comparison)
}

comparison <- comprobando(model, trainSet)
mape <- sum(comparison[c(-3,-8), ]$absolutePercentError) / (nrow(comparison)-2)
#mape (:= mean absolute percent error)



#### EJERCICIO.  pruebo el testSet  #### 
comparison <- comprobando(model, testSet)
mape <- sum(comparison[-10, ]$absolutePercentError) / nrow(comparison)
#como hay un valor que vale 0 como valor inicial su división es infinito.
#Un desastre. SE lo quito para ver si arreglo algo.



#### EJERCICIO.  Making new predictions  #### 
data <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)
colnames(data) <- c("ProductType","Product","Price",                        
                    "x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
                    "x1StarReviews","X.Positive.Service.Review.",
                    "NegativeServiceReview","WouldConsumerRecommendProduct","BestSellersRank",
                    "ShippingWeightLbs","ProductDepth","ProductWidth","ProductHeight",
                    "ProfitMargin","Volume")
data$Product <- NULL
dataDefinitivos <- data[filasDeTrampas, columnasDeOverfit]
predictions <- predict(model, dataDefinitivos, interval = "predict", level = 0.95)
posFit <- which(colnames(comparison) == "fit") 
colnames(comparison)[posFit] <- "predicted"
resultado <- cbind(data, predictions)
write.csv(resultado, file = "/Users/luis/Desktop/existingResult.csv" )

