#!Rscript


# probando protocolo actuación --------------------------------------------
cat("\014")       #limpio consola
rm(list = ls())   #borro todos los objetos de memoria

#### 1- ABSORCION/TIPO DE DATOS ####
#por columna: Renombrar columnas y ver si tu tipo de datos es correcto.
#Es posible que en este campo ya necesite observar los NA

source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")
dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
dataRenombrada <- preparacionDatos(dataRaw) # a posteriori cuando 2-, 3-, 4- ya están terminados
dataRenombrada <- seleccionFilasBuenas(dataRenombrada) # a posteriori cuando 2-, 3-, 4- ya están terminados

#### 2- ANALIZO TEMA DE FEATURES por tema ####
#por columna: observo de que van y elimino aquellos que
# no tienen nada que ver(ej: margen producto vs. clientes) o que solo se conocen datos a posteriori (ej: ventas)
## data <- seleccionColumnasConsolidadas(dataRenombrada)
# columnasBorrables <- c(2 , 4, 12, 17, 13)
# columnaEnAnalisis <- 2
# data <- seleccionColumnas_En( c(columnaEnAnalisis, columnasBorrables) , dataRenombrada )
# 
# 
# ## 
# modelo  <- modelizo_lm_BlackWell(data)
# summary(modelo)
## correlacion <- cor(data[,-1])

#### 3- ANALIZO TEMA DE LAS LÍNEAS ####
#por líneas: observo de que van y elimino aquellos que
# no tienen nada que ver. Me fijo para ello en la primera columna que es una gran 
# forzardora

# dataNewExtrapolacion <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)
# lineasUtiles <- lineas_Includes( dataRenombrada[,1], dataNewExtrapolacion[,1])
# lineasUtiles <- which( lineasUtiles == TRUE)
# dataRenombrada <- dataRenombrada[ lineasUtiles, ]
# str(dataRenombrada)
# 
# plot(data)
# plot(data[,1])
# plot(data[,2])
# plot(data[,3])
# 
# 
# modelo  <- modelizo_lm_BlackWell(data)
# summary(modelo)
# correlacion <- cor(data[,-1])
# 
# lineas_Include( data[,1], "PC")
# lineas_Includes( data[,1], c("PC", "hola"))


#### 4- ANALIZO TEMA DE FEATURES DE NUEVO por correlación ####
#plot(dataRaw$X.Product.Type.)   da error!!!! porque???
#por columna: observo de que van y elimino aquellos que
# no tienen nada que ver(ej: margen producto vs. clientes) o que solo se conocen datos a posteriori (ej: ventas)
##data <- seleccionColumnasConsolidadas(dataRenombrada)
columnasBorrables <- c(2, 4, 12, 17 , 13, 5, 7, 8)
columnaEnAnalisis <- 2
data <- seleccionColumnas_En( c(columnaEnAnalisis, columnasBorrables) , dataRenombrada )

modelo  <- modelizo_lm_BlackWell(data)
summary(modelo)
correlacion <- cor(data[,-1])
which(correlacion >= 0.85)
# para terminar lo hago oficial:
dataRenombrada <- data # y paso estos valores a la funcion seleccionColumnasConsolidadas

#### 5- PREPARO EL TEST Y TRAINSET ####
# dataRenombrada
trainSize <- round(nrow(dataRenombrada) * 0.7 )
testSize <- nrow(dataRenombrada) - trainSize
set.seed(126)
trainPosition <- sample(x = nrow(dataRenombrada), size = trainSize)
trainSet <- dataRenombrada[trainPosition,]
testSet <- dataRenombrada[-trainPosition,]

#### 6- MODELIZO CON TRAINset ####

modelo <- modelizo_lm_BlackWell(trainSet)
summary(modelo)
#con 123 de semilla
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                    5.519e+02  5.736e+02   0.962   0.5122  
# ProductTypeDisplay             2.615e+02  7.807e+02   0.335   0.7942  
# ProductTypeExtended_Warranty  -7.313e+02  4.922e+02  -1.486   0.3771  
# ProductTypeGameConsole         1.027e+03  3.533e+02   2.907   0.2109  
# ProductTypePC                  1.126e+03  7.435e+02   1.515   0.3714  
# ProductTypePrinter             5.851e+02  4.890e+02   1.197   0.4432  
# ProductTypeSmartphone         -1.856e+02  3.577e+02  -0.519   0.6954  
# ProductTypeTablet             -1.190e+03  2.907e+02  -4.095   0.1525  
# Price                         -4.430e-03  4.445e-01  -0.010   0.9937  
# x3StarReviews                  5.525e+01  3.778e+00  14.624   0.0435 *
#   X.Positive.Service.Review.     5.523e+00  1.231e+00   4.486   0.1396  
# NegativeServiceReview         -2.020e+01  8.812e+00  -2.292   0.2619  
# WouldConsumerRecommendProduct -4.610e+02  6.337e+02  -0.727   0.5996  
# ProductDepth                  -3.007e+01  4.164e+01  -0.722   0.6018  
# ProductWidth                   2.177e+01  3.007e+01   0.724   0.6011  
# ProductHeight                 -4.689e+01  3.067e+01  -1.529   0.3688  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 191.4 on 1 degrees of freedom
# Multiple R-squared:  0.9997,	Adjusted R-squared:  0.996 
# F-statistic: 266.4 on 15 and 1 DF,  p-value: 0.04804

# con 126 de semilla:
#   
#   Call:
#   lm(formula = datasource$Volume ~ ., data = datasource)
# 
# Residuals:
#   ALL 17 residuals are 0: no residual degrees of freedom!
#   
#   Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)                   -6.864e+02         NA      NA       NA
# ProductTypeDisplay             9.084e+01         NA      NA       NA
# ProductTypeExtended_Warranty  -1.232e+04         NA      NA       NA
# ProductTypeGameConsole         6.093e+02         NA      NA       NA
# ProductTypeLaptop              1.807e+01         NA      NA       NA
# ProductTypeNetbook             6.748e+02         NA      NA       NA
# ProductTypePC                  6.561e+01         NA      NA       NA
# ProductTypePrinter             2.725e+02         NA      NA       NA
# ProductTypeSmartphone          3.137e+02         NA      NA       NA
# ProductTypeTablet              3.560e+02         NA      NA       NA
# Price                         -1.297e-15         NA      NA       NA
# x3StarReviews                  4.868e+00         NA      NA       NA
# X.Positive.Service.Review.     4.947e+01         NA      NA       NA
# NegativeServiceReview         -2.000e+01         NA      NA       NA
# WouldConsumerRecommendProduct  5.686e+02         NA      NA       NA
# ProductDepth                   3.993e+01         NA      NA       NA
# ProductWidth                  -4.189e+01         NA      NA       NA
# ProductHeight                         NA         NA      NA       NA
# 
# Residual standard error: NaN on 0 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:    NaN 
# F-statistic:   NaN on 16 and 0 DF,  p-value: NA


#### 7-(fallo) pruebo el trainSet  ####
#con 123 de semilla
comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)   #60.13542% de error absoluto

# el testSet  # 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)

mapeTest
mapeTrain
#No puedo comprobar con tan pocos datos, por lo que observo. 
#Paso a generar un modelo sin Test y que sea lo que Dios quiera.

#123 le faltan levels Laptop y Netbook 124 le falta Extended_Warranty
#126 dice:
# > comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
# Warning messages:
#   1: In predict.lm(modeloM, datos, interval = "predict", level = 0.95) :
#   prediction from a rank-deficient fit may be misleading
# 2: In qt((1 - level)/2, df) : NaNs produced

#### 8- MODELIZO SIN TRAINset ####

modelo <- modelizo_lm_BlackWell(dataRenombrada)
summary(modelo)


comparisonTrain <- comprobando(modeloM = modelo, datos = dataRenombrada)
comparisonTrain <- addComparativa(comparisonTrain)  
mapeTrain <- mi_mape(comparisonTrain)    #76.18027% de error absoluto

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     357.7941   368.0057   0.972 0.368474    
# ProductTypeDisplay              726.4499   466.7444   1.556 0.170613    
# ProductTypeExtended_Warranty   -814.0519   281.0135  -2.897 0.027447 *  
#   ProductTypeGameConsole          927.9134   211.0287   4.397 0.004583 ** 
#   ProductTypeLaptop              -251.4721   170.4050  -1.476 0.190470    
# ProductTypeNetbook             -304.2204   287.2389  -1.059 0.330309    
# ProductTypePC                   351.5530   187.6150   1.874 0.110097    
# ProductTypePrinter              562.2555   284.3682   1.977 0.095392 .  
# ProductTypeSmartphone          -239.7619   241.5493  -0.993 0.359232    
# ProductTypeTablet             -1252.3633   188.7887  -6.634 0.000566 ***
#   Price                             0.3674     0.2086   1.761 0.128749    
# x3StarReviews                    54.4020     2.5395  21.422 6.75e-07 ***
#   X.Positive.Service.Review.        5.4624     0.8044   6.790 0.000499 ***
#   NegativeServiceReview           -15.9870     4.1520  -3.850 0.008455 ** 
#   WouldConsumerRecommendProduct  -219.1185   389.8705  -0.562 0.594454    
# ProductDepth                     -1.0111    11.6764  -0.087 0.933813    
# ProductWidth                     -9.6243    11.7561  -0.819 0.444270    
# ProductHeight                   -52.8847    20.1418  -2.626 0.039290 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 131.2 on 6 degrees of freedom
# Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9974 
# F-statistic: 528.9 on 17 and 6 DF,  p-value: 4.176e-08



#### 9- Making new predictions  #### 
#dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#dataRaw <- read.csv("/Users/luis/Desktop/new product attributes copia.csv", stringsAsFactors=FALSE)
#dataNewP <- read.csv("/Users/luis/Desktop/new product attributesII.csv", stringsAsFactors=FALSE)
dataNewP <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)

dataNewRenombrado <- preparacionDatos(dataNewP)
dataP <- seleccionColumnasConsolidadas(dataNewRenombrado)

str(dataP)
# summary(modelo)

predictions <- comprobando(modeloM = modelo, datos = dataP)
VolPredictions <- predictions[,11]
resultado <- cbind(dataNewRenombrado, VolPredictions)
beneficios <- resultado$Price * resultado$ProfitMargin * resultado$VolPredictions
resultado$beneficios <- beneficios
resultado$beneficios <- as.integer(resultado$beneficios)
write.csv(resultado, file = "/Users/luis/Desktop/existingResult_LM.csv" )

resultado

#### 10- CARGO LA COMPARATIVA  #### 


