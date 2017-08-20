# Módulo 2 Tarea 2
## Introducción
Es mi primera prueba en GitHub usando correctamente el Git. Nace de la necesidad de compartir con mis compañeros del bootcamp Data Analyst de Ubiqum mi programa para la tarea 2 del modulo2.
Así pues aprovecho esta Task para aprender Github, markdown y R. Todo de una tacada.

## Primer commit
En este primer commit pongo los dos ficheros básicos para empezar a trabajar.

## Segundo commit
Añado un README.md (este que lees) para explicar la evolución del programa

## Tercer commit
Descubro una posible causa del error. 
Aún no he subido el fichero .R original causante del hilo de discusión. Estoy en pruebas, aprendiendo a usar Git y Github

## Cuarto commit
La causa del error era cierta. Todo mi problema estaba en el fichero *”new product attributesII.csv”* 
Si alguien desea probar mi programa en sus datos se ejecuta de la siguiente manera.
Debe bajarse los ficheros siguientes:
* absorcionDatos.R
* preparacionTabla.R
* anti-overfit.R
* modelizacion.R
* pruebas.R
* extrapolaciones.R

Luego debe ejecutar cada fichero en este mismo orden citado.
### Antes de ejecución
Si hay que empezar de cero es aconsejable limpiar el "Enviroment" antes (icono de la escoba).\
Antes de ejecutar hay que cambiar el path de los ficheros fuente de datos. Esto es la línea 5 del fichero **extrapolaciones.R** y la línea  2 de **absorcionDatos.R** . Cada uno colocará la ruta (path) que le corresponda.\
También se pueden cambiar las líneas o columnas a borrar para evitar el overfit. Eso exige leer con atención el código y entenderlo. Esto es siempre costoso pero satisfactorio.


### Lectura del resultado  
El resultado del *trainSet* puede leerse en el data.frame llamado *comparisonTrain* y el del *testSet* en *comparisonTest*.
Interesantes me resulta leer el mape (:=main absolute percent error) de cada uno de esos grupos de datos. Se supone que cuanto más parecidos son mejor es el modelo. Ya veis que con **mapeTest=152.97** y **mapeTrain=488.41** no se parecen mucho.
Por último y como resultado final está el *data.frame* **resultado** que realmente contiene el cálculo de los volúmenes de ventas previstas.
Estoy cerca del final. Ahora me toca comparar con los anteriores resultados con *Weka*.

### Comienzo aquí un nuevo hilo en el que hace lo mismo que en otro (master) pero en menos ficheros.


