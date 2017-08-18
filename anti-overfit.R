
#### EJERCICIO. evitar Overfitting ####


#me daba error con una columna de string(1).
correlacion <- cor(data[,2:18])
correlacion  #lo que esté relacionado lo pongo en el vector de la siguiente función

seleccionColumnas <- function(dataRAW){
  #observo y decido que sobran: Todo aquello que tiene correlación >0.85
  #Son: 5Star, 3star, 1 star, NegativeService: Columnas 2,4,6 y 8 +1 por la primera 
  #eliminada. La 2 va fuera por ser arbitraria.
  columnasDeOverfit <- c(2, 4, 5, 6, 8, 10, 12)
  columnasDeOverfit <- columnasDeOverfit * -1 #para que las quite las hago negativas
  dataNew <- dataRAW[, columnasDeOverfit]
  return(dataNew)
}

data <- seleccionColumnas(data)
str(data)


