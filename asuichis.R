#ejercicios 24JUN
library(MPV)
data()
#Realiza los ejercicios siguientes del libro de Regresión de Montgomery 7.2, 7.6,13.2, 13.5
data("p13.2")
ej1<-p13.2
ej1

#a. Ajustar un modelo de regresión logístico a la variable de respuesta y. Usar un modelo de regresión lineal simple como estructura para el predictor lineal.
#b. ¿Indica la desviación del modelo que es adecuado el modelo logístico de regresión de la parte a?
#c. Dé una interpretación del parámetro beta1 en este modelo.
#d. Desarrollar el predictor lineal para incluir un término cuadrático en el ingreso. ¿Hay algún indicio de que se requiere este término cuadrático en el modelo?

logistico<-glm(y~x,data=ej1,family="binomial")
summary(logistico)#el valor de beta1 es el estimate del x
linearsimple<-lm(y~x,data=ej1)
summary(linearsimple)
plot(linearsimple)
plot(logistico)
#parece ser que el modelo logístico si resulta adecuado debido a la disminución de la devianza
#aunque esta solo fue por una diferencia de 5
cuadratico<-lm(y~x+xcua,data=ej1)
xcua<-(ej1$x)^2
summary(cuadratico)
