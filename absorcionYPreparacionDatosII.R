#! Rscript
#/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/
# source("absorcionYPreparacionDatos.R", echo = TRUE)

# lo que hago aquí es observar los datos gráfica y visualmente para 
# limpiarlos.
# solo cambio nombre de columnas, busco instancias (líneas) erróneas y 
# las borro o arreglo

#Cargo la librería
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")

#Cargo el fichero con los datos de entrenamiento y prueba
dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#dataRaw <- read.csv(file.choose())



#### EJERCICIO. Observacion/adaptacion de datos ####

data <- preparacionDatos(dataRaw) #cambia nombre atributos y rellena NA de campo detectado.
data <- seleccionFilas(data) #descubro que determinadas líneas son tramposas. Las elimino.
#elimino las líneas que no están las extrapolaciones
dataNewExtrapolacion <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)
lineasUtiles <- lineas_Includes( data[,1], dataNewExtrapolacion[,1]) 
lineasUtiles <- which( lineasUtiles == TRUE)
data <- data[ lineasUtiles, ]
str(data)

plot(data)
plot(data[,1])
plot(data[,2])
plot(data[,3])
# ...
# no consigo ver más cosas en las líneas. Busco algún dato que sea "raro" pero como son tan pocos no tengo mucho 
# con quien comparar.

#PASO A CORRELACION

# data <- data[, -4]
# correlacion <- cor(data[, -1])  Correlación le da distinto porque él ya ha limpiado algo según vio algo.
#### EJERCICIO. evitar Overfitting ####


#me daba error con una columna de string(1).
correlacion <- cor(data[,2:18])
correlacion  #lo que esté relacionado lo pongo en el vector de la siguiente función
write.csv(correlacion, file = "/Users/luis/Desktop/correlationAllColumns.csv" )

data <- seleccionColumnas(data)
str(data)

# #Razones: VER libLuisMod2Task2.R

