#libLuisMod2Task2.R
# uso: 
# source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")

pruebaSource <- function(dataRAW = 0){
  dataNew <- 5
  return(dataNew)
}

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
  # dataNew[73,1] <- "'Game Console'"
  # dataNew[80,1] <- "GameConsole"
  dataNew[which(dataNew[,1] == "'Game Console'"),1] <- "GameConsole"
  dataNew[which(dataNew[,1] == "'Extended Warranty'"),1] <- "Extended_Warranty"
  dataNew$ProductType <- factor(dataNew$ProductType)
  
  return(dataNew)
}

lineas_Include <- function( vectorRaw, clave){
  coincidencias <- vectorRaw == clave
  return(coincidencias)
}

lineas_Includes <- function( vectorRaw, claves){
  eliminables <- list()
  for (i in 1:length(claves)) {
    resultado <- lineas_Include( vectorRaw, claves[i])
    eliminables[[i]] <- resultado
  }
  #  miro para cada vectorRaw: el dato i está en claves --->si --> lo ignoro
  #                                                  --->no --> incluyo i en eliminables.
  acumuladoEliminables <- c(1:length(vectorRaw))
  acumuladoEliminables <- acumuladoEliminables & FALSE
  for (i in 1:length(claves)) {
    acumuladoEliminables <- eliminables[[i]] | acumuladoEliminables
  }
  return(acumuladoEliminables)
}

seleccionFilasBuenas <- function(dataRAW ){
  #veo que algunas instancias tienen datos trampa.
  #Son datos que alguien ha repetido porque son iguales en todos campos
  
  #filasDeTrampas <- -41:-35
  # filasDeTrampas <- c(filasDeTrampas, c(1, 2, 3, 4, 5, 26, 35, 36, 49, 50, 51, 52, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73) )
  filasCorrectas <- c(1, 2, 3, 4, 5, 26, 35, 36, 49, 50, 51, 52, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 80)
  dataNew <- dataRAW[filasCorrectas, ]
  return(dataNew)
}

seleccionFilasMas <- function(dataRAW, filas){
  #veo que algunas instancias tienen datos trampa.
  #Son datos que alguien ha repetido porque son iguales en todos campos
  filasDeTrampas <- -41:-35
  filasDeTrampas <- c(filasDeTrampas, filas)
  dataNew <- dataRAW[filasDeTrampas, ]
  return(dataNew)
}



#### EJERCICIO. evitar Overfitting ####
foo <- function(){
  #CODIGO QUE REPITO PARA DETECTAR COLUMNAS FEAS Y ELIMINARLAS
  columnasBorrables <- c(2, 4, 12, 17 ,13)
  columnaEnAnalisis <- 13
  data <- seleccionColumnas_En( c(columnaEnAnalisis, columnasBorrables) , dataRenombrada )
  #data <- seleccionColumnasConsolidadas( dataRenombrada )
  
  modelo  <- modelizo_lm_BlackWell(data)
  summary(modelo)
  correlacion <- cor(data[,-1])
}

seleccionColumnas_En <- function( columnasBorrables , dataRAW ){
  #observo y decido que sobran: Todo aquello que tiene correlación >0.85
  #Son: 5Star, 3star, 1 star, NegativeService: Columnas 2,4,6 y 8 +1 por la primera 
  #eliminada. La 2 va fuera por ser arbitraria. 
  dataNew <- dataRAW[, -columnasBorrables] #para que las quite las hago negativas
  return(dataNew)
}

seleccionColumnasConsolidadas <- function(dataRAW){
  #observo y decido que sobran: Todo aquello que tiene correlación >0.85
  #Son: 5Star, 3star, 1 star, NegativeService: Columnas 2,4,6 y 8 +1 por la primera 
  #eliminada. La 2 va fuera por ser arbitraria.
  columnasDeOverfit <- c(2, 4, 12, 17 , 13, 5, 7, 8) 
  dataNew <- seleccionColumnas_En(columnasDeOverfit, dataRAW)
  return(dataNew)



# #Razones:
# 1- "ProductType", 2- "Product",3- "Price",                        
# 4- "x5StarReviews",5- "x4StarReviews",6- "x3StarReviews",7- "x2StarReviews",
# 8- "x1StarReviews",9- "X.Positive.Service.Review.",
# 10- "NegativeServiceReview",11- "WouldConsumerRecommendProduct",12- "BestSellersRank",
# 13- "ShippingWeightLbs",14- "ProductDepth",15- "ProductWidth",16- "ProductHeight",
# 17- "ProfitMargin",18- "Volume"
# 
# 2- "Product" es un codigo interno (referencia) sin valor ni sentido de asignación. R^0.5: 1
# 4- "x5StarReviews", El 5 está TRAMPEADO con el volumen. Lo decubro porque tiene un R-squared = 1 o sea perfercto con una 
#     coef. de 4 perfecto en relación volumen . Si lo quito R^2 baja a 0.929.  
# 12- "BestSellersRank" porque en los nuevos productos es un dato no existente o inutil. Es la = opinión que jefa. 
#     ¿porque tiene un coef.cor de solo 0.033?   
#     Lo quito y R^0.5:  0.9272 empeora logicamente porque está relacionado con Volumen.
# 13- "ShippingWeightLbs" porque al cliente no le preocupa el peso del envío sino el del producto final.
#     logicamente está relacionado pero incluyen cargadores y demás..
#     ¿Me ayudan estos datos?  ShippingWeightLbs   Estimate 2.4992  Std. Error 10.3873   t 0.241  Pr(>|t|)0.810743
#     Al quitarlo obtengo   R^0.5: 0.9272 o sea que no cambia al quietarlo.
# 17- "ProfitMargin" porque al cliente no le importa mi margen. Ni tan siquiera lo conoce.
#     sus valores 18.4930  2095.8854   0.009 0.992992 me parece alto el coef. pero un sigma de 2095 es como si no vale nada.
#     al quitarlo queda igual que antes R^0.5: 0.9272 No afecta por lo tanto el retirarlo. Creo que solo aumenta el error.

  # VAMOS CON LAS LÍNEAS.
# Debo encontrar las líneas muy sucias (con muchos NAs o valores disparatados); repetidas por 
# error (copia y pega para completar); y aquellas que no tienen nada que ver con el tema a 
# tratar (ej: categoricas que eliminan).
# Solo he borrado las que no tienen nada que ver con el tema que son la que NO empiezan por el test
# 
  
  # VAMOS CON LA CORRELACIÓN ENTRE VARIABLES INDEPENDIENTES.
  

# 5- "x4StarReviews" está realcionado con 6- "x3StarReviews",7- "x2StarReviews". Prefiero borrar el
     #5- "x4StarReviews", porque tiene peor coeficiente que el 6- "x3StarReviews" con el volumen
     # Tras borrar los valores son: R^0.5:  0.9995   
  
# 6- "x3StarReviews", está relacionado con  7-"x2StarReviews"0.960, 8- "x1StarReviews"0.874. 
    # Borro 7-"x2StarReviews" porque tiene peor predicción con Volumen
    # Tras borrar los valores son: R^0.5:  0.9993

# 6- "x3StarReviews", está relacionado con  8- "x1StarReviews" 0.874. 
  # Borro 8- "x1StarReviews" porque tiene peor predicción con Volumen(0.984 vs. 0.882)
  # Tras borrar los valores continua: R^0.5:  0.9993


#Siempre debo atender a estas preguntas. 
#  ¿El dato no nos sirve de nada porque nadie lo lee o algo así?
#  ¿El título no es lo que parece? 
}


#### EJERCICIO. Modelización ####

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



#### EJERCICIO.  pruebo un Set  ####
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





