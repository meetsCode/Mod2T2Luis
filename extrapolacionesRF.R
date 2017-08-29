

# probando protocolo actuación --------------------------------------------
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

modelo  <- modelizo_RF_BlackWell(data)
summary(modelo)
correlacion <- cor(data[,-1])
which(correlacion >= 0.85)
# para terminar lo hago oficial:
dataRenombrada <- data # y paso estos valores a la funcion seleccionColumnasConsolidadas

#### 5- PREPARO EL TEST Y TRAINSET ####
# dataRenombrada
trainSize <- round(nrow(dataRenombrada) * 0.7 )
testSize <- nrow(dataRenombrada) - trainSize
set.seed(123)
trainPosition <- sample(x = nrow(dataRenombrada), size = trainSize)
trainSet <- dataRenombrada[trainPosition,]
testSet <- dataRenombrada[-trainPosition,]

#### 6- MODELIZO CON TRAINset ####

modelo <- modelizo_RF_BlackWell(trainSet)
summary(modelo)

# Call:
#   svm(formula = datasource$Volume ~ ., data = datasource)
# 
# 
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.05 
# epsilon:  0.1 
# 
# 
# Number of Support Vectors:  14


#### 7-(fallo) pruebo el trainSet  ####

comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)   #261.3445% de error absoluto

# el testSet  # 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)  #65.67938% de error absoluto

#con 123 la cosa empeoró:
#   
# > mapeTest     3144.439
# > mapeTrain    838.4039

mapeTest
mapeTrain

#### 8- MODELIZO SIN TRAINset ####
modelo <- modelizo_RF_BlackWell(dataRenombrada)
summary(modelo)


comparisonTrain <- comprobando(modeloM = modelo, datos = dataRenombrada)
comparisonTrain <- addComparativa(comparisonTrain)  
mapeTrain <- mi_mape(comparisonTrain)    #941.7546% de error absoluto

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
write.csv(resultado, file = "/Users/luis/Desktop/existingResult_svm.csv" )

resultado
