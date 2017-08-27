#!Rscript 

#### Mi librería. ####
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")

#### EJERCICIO. Absorción y preparación de los datos ####
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/absorcionYPreparacionDatos.R", echo = TRUE)

#### EJERCICIO. Modelización ####
trainSize <- round(nrow(data) * 0.7 )
testSize <- nrow(data) - trainSize
set.seed(123)
trainPosition <- sample(x = nrow(data), size = trainSize)
trainSet <- data[trainPosition,]
testSet <- data[-trainPosition,]


modelo <- modelizo_lm_BlackWell(data)
summary(modelo)



#### EJERCICIO.  pruebo el trainSet  ####

comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)



#### EJERCICIO.  pruebo el testSet  #### 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)

mapeTest
mapeTrain




