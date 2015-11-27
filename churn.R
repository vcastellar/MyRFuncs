## clasificación churn sobre datos churn del paquete C50

## carga de librerías
library(C50)
library(ggplot2)

## carga de datos
data(churn)

## examen previo variables
summary(churnTrain)
head(churnTrain)
sapply(churnTrain, class)

## limpieza de datos
## hay pocos conteos por estado, deberíamos eliminar esta variable
with(churnTest, table(state, area_code))
