
R.Version()
install.packages("e1071")
library("e1071")
library(e1071) #creo que también vale sin comillas en este caso.


data <- matrix(1:6, nrow = 2)
dim(data)

#### capítulo 14 de Analytics for dummies 1ed. PREPARACION DATOS ####
autos <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", header=FALSE, sep="", as.is=TRUE)

head(autos,5)
tail(autos,5)
summary(autos)
#con estos tres veo que no hay cabecera descriptiva, que algunas columnas son 
#claramente texto y otras categóricas.

str(autos)
#con este comando se me muestra el tipo de variable detectada. 
#Si a números los pone como caracter es porque faltan datos en esa columna.
#Sin embargo no detectó "Fator"s que quedan como enteros o como Strings

#TODO: 1-Renombrar las columnas; 2-pasar la columna 4 a numérica; 
#3-Reparar los datos ausentes; 4-Poner como Factors los valores discretos: 
#cylindros,año de modelo, origen; 5-Eliminar el V9-nombre del modelo porque 
#no aporta luego molesta.

#TODO: 1-Renombrar las columnas;
colnames(autos) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "modelYear", "origin", "carName")
#vuelvo a hacer un str() y resumme() para ver los cambios. Para ver si alguna columna tiene
#huecos lo que hago es visualizar el fichero y ordenar las columnas por su valor dos veces
#de mayor a menor y de menor a mayor. Así quedan delante los NA o los ? o los huecos si hubiere.
#En este caso la 4ª columna "horsePower" Tiene huecos.

#2-pasar la columna 4 a numérica;
autos$horsepower <- as.numeric(autos$horsepower)
#Ya sé que algunas celdas no tienen datos. En ese caso el programa me pondrá un NA 
#donde haya un "?" Si ordeno la columna los NA quedan siempre al final y ya no aparecen
#arriba.

##3-Reparar los datos ausentes
autos$horsepower [is.na(autos$horsepower)] <- mean(autos$horsepower, na.rm = TRUE)
#lo común es cambiar los ausentes (NA) por la media de los otros valores. 
#Así no modifican la media general y sus filas quedan como "ruido" casi sin importancia.
#yo habría pensado que is.na() me da un vector de posiciones pero en realidad me da un 
#vector con TRUE O FALSE. En cualquier caso todo funciona.
# al hacer la media es importante poner na.rm = TRUE para que ignore los NA que ya hay.

# 4-Poner como Factors los valores discretos: 
#cylindros,año de modelo, origen
autos$origin <- factor(autos$origin)
autos$modelYear <- factor(autos$modelYear)
autos$cylinders <- factor(autos$cylinders)

#5-Eliminar el V9-nombre del modelo porque lo que no aporta, molesta.
autos$carName <- NULL





#### capítulo 14 de Analytics for dummies 1ed. CREACIÓN DEL MODELO ####
trainSize <- round(nrow(autos) * 0.7)
testSize <- nrow(autos) - trainSize
#tamaño del entrenador y del testeador

set.seed(123)
training_indices <- sample(seq_len(nrow(autos)), size = trainSize)
trainSet <- autos[training_indices,]
testSet <- autos[-training_indices,]
#separamos unas líneas(instancias) como test y otras como train

model <- lm(formula = trainSet$mpg ~ . , data = trainSet)
#esto difiere de la foórmula lm(y ~ x) donde y ,x son vectores de datos.

summary(model)
#Resultado:
#  Multiple R-squared:  0.8741  El 87% de variabilidad es explicada por el modelo: Bien
#  p-value: < 2.2e-16           Como es <0.05 es bueno




#### capítulo 14 de Analytics for dummies 1ed. TEST EL MODELO ####

predictions <- predict(model, testSet, interval = "predict" ,level = .95)
head(predictions)
#fit       lwr      upr
#2 16.48993 10.530223 22.44964 donde fit es el calculado teórico con lm.
#4 18.16543 12.204615 24.12625 Debo comprararlo con el real para ver la diferencia
#5 18.39992 12.402524 24.39732
#6 12.09295  6.023341 18.16257
#7 11.37966  5.186428 17.57289
#8 11.66368  5.527497 17.79985

#comparo los datos reales con los del modelo.
comparison <- cbind(testSet$mpg, predictions[,1])
colnames(comparison) <- c("actual", "predicted")
head(comparison)
summary(comparison)
mape <- (sum(abs(comparison[,1]-comparison[,2]) / 
               abs(comparison[,1]))
         /nrow(comparison)*100)
#mape(mean absolute percent error ) es el error relativo absoluto.
#da 10.93
#también lo puedo obtener así:
mapeTable <- cbind(comparison, abs(comparison[,1]-comparison[,2]) / comparison[,1] *100
                   )
colnames(mapeTable)[3] <- "absolute percent error"
head(mapeTable)
sum(mapeTable[,3])/nrow(comparison)  #da [1] 10.93689
#esta es otra manera de calcular el mape y como se ve da lo mismo que antes.

#### capítulo 14 de Analytics for dummies 1ed. PREDECIR CON NUEVOS DATOS ####
newPrediction <- predict(model, 
                         list(cylinders = factor(4), 
                              displacement=370,
                              horsepower=150,
                              weight=3904,
                              acceleration=12,
                              modelYear=factor(70),
                              origin=factor(1)),
                         interval = "predict",
                         level = .95)
newPrediction
#### Cap. 14 de Analytics for dummies 1ed. MÉTODO DE CLASIFICACIÓN.Decision Tree ####
#En la página 266 nos enseñan a clasificar un tipo de lo que sea (clase)
#La clasificación se hace para valores categóricos. Me ayuda a saber los 
#distintos tipo de una clase en función de los otros valores independientes.
#un ejemplo es clasificar a los clientes por tipo. Ej: clientes de alto valor, 
#clientes regulares o clientes mercenarios,...
#En este ejemplo tenemos semillas con distintas caracteríasticas.
#### Cap. 14 de Analytics for dummies 1ed. Decision Tree. Absorbo datos ####
seeds <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", 
                    header=FALSE, 
                    sep="", 
                    as.is=TRUE)
#En los disintos tipos de seed 1->Kama; 2->Rosa; 3->Canadian
summary(seeds)
str(seeds) #está todo como numerico. No faltan datos.
#veo que el V8 es categórico.
#debo poner nombres descriptivos a las columnas:


#### Cap. 14 de Analytics for dummies 1ed. Decision Tree. Preparo datos ####
#renombro las columnas:
colnames(seeds) <- c("area","perimeter","compactness","length", 
                     "width", "asymmetry","length2","seedType")
#pongo seedType como categóricos (Factor)
seeds$seedType <- factor(seeds$seedType)
str(seeds)


#### Cap. 14 de Analytics for dummies 1ed. Decision Tree. Creo el modelo ####
trainSize <- round(nrow(seeds) * 0.7)
testSize <- nrow(seeds) - trainSize

set.seed(123)
training_indices <- sample(seq_len(nrow(seeds)), size = trainSize )
trainSet <- seeds[training_indices,]
testSet <- seeds[-training_indices,]

# Paquetes que tienen árboles de clasificación: party, rpart, 
#  tree, and randomForest. Nos quedamos con party
install.packages("party")
library(party)

#entrenamos el modelo:
model <- ctree(seedType ~ . , data = trainSet)
summary(model)
model
plot(model)



#### Cap. 14 de Analytics for dummies 1ed. Decision Tree. Inspeccino el modelo ####
table(predict(model), trainSet$seedType)
#Esto muestra la tabla diagonal Veo que fallo 11 de 147. -> 7.48% buen dato.

testPrediction <- predict(model, newdata = testSet)
#ME PIERDO A PARTIR DE AQUÍ.
#SOLO ESCRIBO EL CÓDIGO PARA AHORRARME ESFUERZO.
table(testPrediction, testSet$seedType)
newPrediction <- predict(model, list(area=11, 
                                     perimeter=13, 
                                     compactness=0.855, 
                                     length=5, 
                                     width=2.8, 
                                     asymmetry=6.5, 
                                     length2=5), 
                         interval="predict", 
                         level=.95)

newPrediction



#### APRENDIENDO SOLO ####
TRUE + 0   #[1] 1
TRUE + 1   #[1] 2
TRUE + 2  #[1] 3
2+ TRUE   #[1] 3
(2 & TRUE)  #[1] TRUE
(2 | TRUE)   #[1] TRUE
(2 | FALSE)  #[1] TRUE
TRUE + TRUE    #[1] 2



- - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



#### EJERCICIO. Absorción ####
data <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#como no pueden empezar por número me pone una X delante. 
#Los espacios en blanco los pasó a puntos.
#Ahora les quito los espacios en blanco.

colnames(data)
head(data,3)

#### EJERCICIO. Cabeceras de filas y columnas ####
# Es un problema que haya espacios vacíos en las cabeceras de las columnas.
#como no pueden empezar por número me pone una X delante.
# en la versión 3.3.1
#      Los espacios en blanco los pasa a puntos automáticamente.
#      Si empieza por número les pone una X.delante automáticamente. 
# Lo más práctico es cambiarles el nombre en cualquier caso para evitar errores del 
# copiado o corrupción del fichero.

#las pongo más legibles:
colnames(data) <- c("ProductType","Product","Price",                        
"x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
"x1StarReviews","X.Positive.Service.Review.",
"NegativeServiceReview","WouldConsumerRecommendProduct","BestSellersRank",
"ShippingWeightLbs","ProductDepth","ProductWidth","ProductHeight",
"ProfitMargin","Volume")



#### EJERCICIO. Observacion/cambios datos ####
str(data)
#veo que BestSellersRank ha quedado como sting cuando debería ser num. 
#Eso es porque debe haber vacíos. 

#Trato el campo BestSellersRank
data$BestSellersRank <- as.numeric(data$BestSellersRank)
#me aviso que NAs introducidos por coerción.
#Queda reparados los NA
data$BestSellersRank[is.na(data$BestSellersRank)] <- mean(data$BestSellersRank, na.rm = TRUE)

#ProductType es una variable categórica. La transformo en Factor 
# pero antes hay que eliminar los espacios en sus descripciones:
dataNew[which(dataNew[,1] == "'Game Console'"),1] <- "GameConsole"
dataNew[which(dataNew[,1] == "'Extended Warranty'"),1] <- "Extended_Warranty"
data$ProductType <- factor(data$ProductType)

#La columna product es un código interno sin relación con el código.
#La borro para no despistar al sistema:
#no funcionó    rm(data$Product)
data$Product <- NULL # Y creo la función seleccionColumnasConsolidadas(dataRAW) e introduzco el valor 2.


#### EJERCICIO. Modelización 1 ####
#hago una prueba de modelización a ver qué errores o aciertos me da.
trainSize <- round(nrow(data) * 0.7 )
testSize <- nrow(data) - trainSize
set.seed(123) #esto es para que en todas las veces que lo repita me salga igual.
#trainPosition <- sample( seq_len(nrow(data)), size = trainSize) la simplifico
trainPosition <- sample(x = nrow(data), size = trainSize)
trainSet <- data[trainPosition,]
testSet <- data[-trainPosition,]


model <- lm(formula = trainSet$Volume ~ . , data = trainSet)
summary(model)
#Residual standard error: 2.926e-13 on 28 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#p-value: < 2.2e-16
#
#Warning message:
#  In summary.lm(model) : essentially perfect fit: summary may be unreliable
#

#Vaya! parece que algo tan perfecto levanta perspicacias.
#¿Cómo se hacía para encontrar columnas relacionadas?



#### EJERCICIO. LIMPIO DE FEATURES no apropiados ####
#por columna: observo de que van y elimino aquellos que
# no tienen nada que ver(ej: margen producto vs. clientes) o que solo se conocen datos a posteriori (ej: ventas)
# este apartado lo ejecuto uno a uno. Por cada columna borrada realizo una revisión nueva de correlación y calidad d modelo
# ¿La columna que no aporta molesta? Es discutible. A veces sí otras puede ser una sorpresa la info que aparece.
# Al final las <<columnasBorrables>> debe ir a la función   seleccionColumnasConsolidadas(dataRenombrada)
columnasBorrables <- c(2 , 4)#, 12)#, 17)#, 13)
columnaEnAnalisis <- 12
data <- seleccionColumnas_En( c(columnaEnAnalisis, columnasBorrables) , dataRenombrada )
 
 
## 
modelo  <- modelizo_lm_BlackWell(data)
summary(modelo)
correlacion <- cor(data[,-1])



#### EJERCICIO. ANALIZO/LIMPIO LAS INSTANCIA (LÍNEAS), OUTSIDERS (o es outlier?) y otros ####
#por líneas: observo de que van y elimino aquellos que
# no tienen nada que ver. Me fijo para ello en la primera columna que es una gran 
# forzardora

# CURIOSAMENTE esto se observa también a través de las gráficas de las columnas. 
# Si algun dato sobre sale de los demás debo averiguar porqué. Son los OUTSIDERS
# Quizá mejor quitar esa línea que lo contiene OJO que esto
# también pasa luego con el fichero con datos a predecir pero lo veré luego
plot(data)
plot(data[,1])
plot(data[,2])
plot(data[,3])
plot(data[,4])
plot(data[,5])
plot(data[,6])
#¿Falta algún dato o se repite alguno? ¿Por qué? ¿Será que los reconoce como iguales/distintos respectivamente? 
# Mostrar la gráfica columna_X vs columna_A_Predecir también es útil? 
plot(data[,6],data[,18])
# ¿Cómo descubro líneas de relleno que son trampas? Lo he visto en este ejercicio por 
# casualidad pero existen. ¿Hay un cor() pero para instancias?


## ## ## MAS ADELANTE DEBO ASEGURARME DE QUE LOS DATOS A PREDECIR ESTÁN EN LOS DATOS DEL TRAINER? Y/O TESTER
# VER APARTADO 



#### EJERCICIO. FEATURES DE NUEVO por correlación. Solving Overfit ####

#Solving Overfit Through Feature Engineering
#Since your regression model is overiftting you will need to revisit the 
#feature in order to find the likely cause. 
#Check for collinearity - Using the cor() function of R check to see if any of the independent variables have correlation coefficients of .85 or higher with any of the other independent variables. If any of the independent variables are collinear you must remove one of the two to solve the issue; it is most common to keep the feature with the highest correlation to the dependent variable.
#TIP: The above process is often called filtering and is normally the first order of operations of feature engineering. There are two other areas, embedded methods and wrapper methods, that we'll cover later in the program.
#2. After removing any features with high correlating values rebuild the training set and use it to build a new linear model. If the new model is again overfitting you'll need to revisit your process until you've solved the collinearity that exists among the features.
#3. When you have a constructed a properly trained model that is no longer ovefitting use it with the related test set to predict the volume of new products from the New Products dataset - note the structure of the two datasets must be the same. In other words any features that were removed from the training set must also be removed from the testing set.
#4. Compare your results to those you found in task one when you used Weka for the analysis. Should the results be similar? Why or why not?
dataSoloNumericos <- data[,2:17] #me daba error con una columna de string. La elimino

#por columna: observo de que van y elimino aquellos que
# no tienen nada que ver(ej: margen producto vs. clientes) o que solo se conocen datos a posteriori (ej: ventas)
##data <- seleccionColumnasConsolidadas(dataRenombrada)
columnasBorrables <- c(2, 4, 12, 17, 13, 5, 7)#), 8) #observa que es continúo sobre los de dos puntos antes.
columnaEnAnalisis <- 2
data <- seleccionColumnas_En( c(columnaEnAnalisis, columnasBorrables) , dataRenombrada )

modelo  <- modelizo_lm_BlackWell(data)
summary(modelo)
correlacion <- cor(data[,-1])
which(correlacion >= 0.85)
# para terminar lo hago oficial:
dataRenombrada <- data # y paso estos valores a la funcion seleccionColumnasConsolidadas



correlacion <- cor(dataSoloNumericos)
#observando correlación se ve hay relación muy clara entre la columna x5StarReviews y Volumen 
#por lo tanto Borro x5StarReviews
dataDefinitivos <- dataSoloNumericos[,-2]
correlacion <- cor(dataDefinitivos)
#El resultado es bueno:




#### EJERCICIO. Modelización De nuevo?? en funcion ####
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")
model <- modelizo_lm_BlackWell(dataDefinitivos)
summary(model)
# Tiene buena pinta:
# Multiple R-squared:  0.9043
# p-value: < 2.2e-16

#hago un nuevo análisis
correlacion <- cor(dataSoloNumericos)
#Elimino todo aquello que tiene correlación >0.9
#Son: 3Star con 4 Star y 2star con 1 star: Columna 4 (3Star) y columna 6 (1Star)
dataDefinitivos <- dataSoloNumericos[,c(-2,-4,-6)]
model <- modelizo_lm_BlackWell(dataDefinitivos)
summary(model)
# Tiene buena pinta:
# Multiple R-squared:  0.8935
# p-value: < 2.2e-16

#hago un nuevo análisis
correlacion <- cor(dataSoloNumericos)
#Elimino todo aquello que tiene correlación >0.85
#Son: 5Star, 3star, 1 star, NegativeService: Columnas 2,4,6 y 8
dataDefinitivos <- dataSoloNumericos[,c(-2,-4,-6,-8)]
model <- modelizo_lm_BlackWell(dataDefinitivos)
summary(model)
# Tiene buena pinta:
# Multiple R-squared:  0.8777
# p-value: < 2.2e-16


comparisonTrain <- comprobando(modeloM = model, datos = dataDefinitivos)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)   #269.7766% de error absoluto


#¿Con cual me quedo de estas tres? El P-value es el mismo pero el error ha empeorado. 
#¿Cómo se ve esto con el testSet?


#### EJERCICIO. Modelización con train- y test-Set ####

# PREPARO EL TEST Y TRAINSET  #
trainSize <- round(nrow(dataRenombrada) * 0.7 )
testSize <- nrow(dataRenombrada) - trainSize
set.seed(126)
trainPosition <- sample(x = nrow(dataRenombrada), size = trainSize)
trainSet <- dataRenombrada[trainPosition,]
testSet <- dataRenombrada[-trainPosition,]

#  MODELIZO CON TRAINset   #
modelo <- modelizo_RF_BlackWell(trainSet)
summary(modelo)

# pruebo el trainSet   #

comparisonTrain <- comprobando(modeloM = modelo, datos = trainSet)
comparisonTrain <- addComparativa(comparisonTrain)
mapeTrain <- mi_mape(comparisonTrain)   #269.7766% de error absoluto

# pruebo el testSet  # 

comparisonTest <- comprobando(modeloM = modelo, datos = testSet)
comparisonTest <- addComparativa(comparisonTest)
mapeTest <- mi_mape(comparisonTest)  #141.1907% de error absoluto



#con 123 la cosa empeoró:  claro, depende de la semilla usada. Probar varias.
#   
# > mapeTest     590.4545
# > mapeTrain    3942.817

mapeTest
mapeTrain

#Quizás si no hay suficientes datos es mejor usar todos lo datos y no el train-,test-set?






#### EJERCICIO. Making new predictions  #### 
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


## ## ## DEBO ASEGURARME DE QUE LOS DATOS A PREDECIR ESTÁN EN LOS DATOS DEL TRAINER Y EL MODELO.
# Esto es especialmente crítico en el caso de categóricos (:=Factor)
# Para este fin incluí una función "include" como en Smalltalk
# lineasUtiles <- lineas_Includes( dataRenombrada[,1], dataNewExtrapolacion[,1])
# lineasUtiles <- which( lineasUtiles == TRUE)
# dataRenombrada <- dataRenombrada[ lineasUtiles, ]
# str(dataRenombrada)
# 
# 
# lineas_Include( data[,1], "PC")
# lineas_Includes( data[,1], c("PC", "hola"))





