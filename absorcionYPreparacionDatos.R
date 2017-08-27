#! Rscript
#/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis
# source("absorcionYPreparacionDatos.R", echo = TRUE)

source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")


dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#dataRaw <- read.csv(file.choose())

# lo que hago aquí es observar los datos gráfica y visualmente para 
# limpiarlos.
# solo cambio nombre de columnas, busco instancias (líneas) erróneas y 
# las borro o arreglo

source("/Users/luis/Desktop/Mooc_Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Tas2.R")


#### EJERCICIO. Observacion/adaptacion de datos ####

data <- preparacionDatos(dataRaw)
data <- seleccionFilas(data)
str(data)



# data <- data[, -4]
# correlacion <- cor(data[, -1])  Correlación le da distinto porque él ya ha limpiado algo según vio algo.
#### EJERCICIO. evitar Overfitting ####


#me daba error con una columna de string(1).
correlacion <- cor(data[,2:18])
correlacion  #lo que esté relacionado lo pongo en el vector de la siguiente función
write.csv(correlacion, file = "/Users/luis/Desktop/correlationAllColumns.csv" )

data <- seleccionColumnas(data)
str(data)

# #Razones:
# 1- "ProductType", 2- "Product",3- "Price",                        
# 4- "x5StarReviews",5- "x4StarReviews",6- "x3StarReviews",7- "x2StarReviews",
# 8- "x1StarReviews",9- "X.Positive.Service.Review.",
# 10- "NegativeServiceReview",11- "WouldConsumerRecommendProduct",12- "BestSellersRank",
# 13- "ShippingWeightLbs",14- "ProductDepth",15- "ProductWidth",16- "ProductHeight",
# 17- "ProfitMargin",18- "Volume"
# 
# La 2 (Product) es un codigo interno (referencia) sin valor ni sentido de asignación.
# 


# 
#4- "x5StarReviews",6- "x3StarReviews", 8- "x1StarReviews" El 5 está TRAMPEADO con el volumen y el 3 está relacionado con 4 y 2 estrellas. 
# El 1 está relacionado con 2 estrellas. Como el 4 estrellas es muy importante para el volumen (correlación de 0.89 se queda)

# 10- "NegativeServiceReview"  porque está correlacionado con  2Stars(col 7) que se queda porque tiene mayor correlacion (0.49 frente a 0.30)
# 11- "WouldConsumerRecommendProduct" ¿Quizás los nuevos productos no tienen este dato o no es fiable. Es la = opinión que jefa.
# 12- "BestSellersRank" porque en los nuevos productos es un dato no existente o inutil. 
# 17- "ProfitMargin" porque al cliente no le importa mi margen. Ni tan siquiera lo conoce.


