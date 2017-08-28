
source("/Users/luis/Desktop/Mooc Pharo/dd/Modulo 2/Mod2Task2/Mod2T2Luis/libLuisMod2Task2.R")
dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
dataRenombrada <- preparacionDatos(dataRaw)
dataFilasLimpias <- seleccionFilasBuenas(dataRenombrada)
dataLimpias <- seleccionColumnasConsolidadas(dataFilasLimpias)


modelo  <- modelizo_lm_BlackWell(dataLimpias)
summary(modelo)






#### EJERCICIO.  Making new predictions  #### 
#dataRaw <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
#dataRaw <- read.csv("/Users/luis/Desktop/new product attributes copia.csv", stringsAsFactors=FALSE)
#dataNewP <- read.csv("/Users/luis/Desktop/new product attributesII.csv", stringsAsFactors=FALSE)
dataNewP <- read.csv("/Users/luis/Desktop/new product attributes.csv", stringsAsFactors=FALSE)

dataP <- preparacionDatos(dataNewP)
dataP <- seleccionColumnas(dataP)

str(dataP)
# summary(modelo)

predictions <- comprobando(modeloM = modelo, datos = dataP)
resultado <- cbind(data.frame(dataNewP$X.Product...), predictions)
write.csv(resultado, file = "/Users/luis/Desktop/existingResult_LM.csv" )

resultado

mapeTest
mapeTrain