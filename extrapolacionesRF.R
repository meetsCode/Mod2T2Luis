
# probando protocolo actuación --------------------------------------------
cat("\014") #limpio consola
rm(list = ls())
#### 1- TIPO DE DATOS ####
#por columna: Renombrar columnas y ver si tu tipo de datos es correcto.
#Es posible que en este campo ya necesite observar los NA

source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")
dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
dataRenombrada <- preparacionDatos(dataRaw)
dataRenombrada <- seleccionFilasBuenas(dataRenombrada)

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

modelo <- modelizo_RF_BlackWell(trainSet)
summary(modelo)

# 
# Length Class  Mode     
# call              5    -none- call     
# type              1    -none- character
# predicted        17    -none- numeric  
# mse             500    -none- numeric  
# rsq             500    -none- numeric  
# oob.times        17    -none- numeric  
# importance       18    -none- numeric  
# importanceSD      9    -none- numeric  
# localImportance   0    -none- NULL     
# proximity       289    -none- numeric  
# ntree             1    -none- numeric  
# mtry              1    -none- numeric  
# forest           11    -none- list     
# coefs             0    -none- NULL     
# y                17    -none- numeric  
# test              0    -none- NULL     
# inbag             0    -none- NULL     
# terms             3    terms  call 

#### 7- pruebo el trainSet  ####

comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)   #269.7766% de error absoluto

# el testSet  # 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)  #141.1907% de error absoluto

#con 123 la cosa empeoró:
#   
# > mapeTest     590.4545
# > mapeTrain    3942.817

mapeTest
mapeTrain

#### 8- MODELIZO SIN TRAINset ####
modelo <- modelizo_RF_BlackWell(dataRenombrada)
summary(modelo)

# 
# Length Class  Mode     
# call              5    -none- call     
# type              1    -none- character
# predicted        24    -none- numeric  
# mse             500    -none- numeric  
# rsq             500    -none- numeric  
# oob.times        24    -none- numeric  
# importance       18    -none- numeric  
# importanceSD      9    -none- numeric  
# localImportance   0    -none- NULL     
# proximity       576    -none- numeric  
# ntree             1    -none- numeric  
# mtry              1    -none- numeric  
# forest           11    -none- list     
# coefs             0    -none- NULL     
# y                24    -none- numeric  
# test              0    -none- NULL     
# inbag             0    -none- NULL     
# terms             3    terms  call

comparisonTrain <- comprobando(modeloM = modelo, datos = dataRenombrada)
comparisonTrain <- addComparativa(comparisonTrain)  
mapeTrain <- mi_mape(comparisonTrain)    #590.0935% de error absoluto

# Me quedo con el del TrainSet

#### 9- Making new predictions  #### 
#dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#dataRaw <- read.csv("/Users/luis/Desktop/new product attributes copia.csv", stringsAsFactors=FALSE)
#dataNewP <- read.csv("/Users/luis/Desktop/new product attributesII.csv", stringsAsFactors=FALSE)
dataNewP <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)

dataP <- preparacionDatos(dataNewP)
dataP <- seleccionColumnasConsolidadas(dataP)

str(dataP)
# summary(modelo)

predictions <- comprobando(modeloM = modelo, datos = dataP)
resultado <- cbind(data.frame(dataNewP$X.Product...), predictions)
write.csv(resultado, file = "/Users/luis/Desktop/existingResult_RF.csv" )

resultado