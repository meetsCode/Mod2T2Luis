
dataRawAntiguos <- read.csv("/Users/luis/Desktop/existing product attributes.csv", stringsAsFactors=FALSE)
dataRawNuevos <- read.csv("/Users/luis/Desktop/new product attributes copia.csv", stringsAsFactors=FALSE)

dataRaw <- rbind(dataRawAntiguos, dataRawNuevos)