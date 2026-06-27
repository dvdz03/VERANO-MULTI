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



#a.Ajustar un modelo de regresión logística a los· datos.
#b.¿Indica la desviación o devianza del modelo que es adecuado el modelo de regresión logística de la parte a?
#c. Interpretarlos coeficientes b1 y b2 del modelo.
#d. ¿Cuáles la probabilidad estimada de que una familia con ingresode 45 000 pesos y
#un coche de 5 años de antigüedad compre un nuevo vehículo durante lossiguientes 6
#meses?
#e. Desarrollar el predictor lineal para incluir un término de interacción. ¿Hay indicios de que se requiera este término en el modelo?
#f. Parael modelo de la parte a, calcular los estadísticos para cada parámetro del modelo.
#g. Calcular intervalos aproximados de 95% de confianza de los parámetros del modelo, para el modelo logística de regresión de la parte a.
data("p13.5")
ej2<-p13.5
ej2

logis1<-glm(y~x1+x2,data=ej2, family="binomial")
logis1
summary(logis1)
#aquí la devianza residual es menor a la nula por lo que incluir el ingreso así como la antiguedad del coche si disminuyó el error
#tabla de coeficientes, para b1 o x1 por cada unidad que aumenta el ingreso, el logaritmo de los momios a favor de comprar un 
#coche cambia en 0.00007.382 manteniendo la antigüedad constante
#Para b2 o x2 por cada año adicional de antiguedad del coche, el logaritmo de los momios a favor de la compra cambia en 0.987 manteniendo el ingreso constante

#D. para esto se usa la función predict usando el argumento type=response.
nuevocliente<-data.frame(x1=45000,x2=5)
incisod<-predict(logis1,newdata=nuevocliente,type="response")
incisod
#la probabilidad de que eso suceda es del 77.1%
#E
interaccion<-glm(y~x1*x2,data=ej2,family="binomial")
summary(interaccion)
#aquí muestra que la devianza baja aún más comparado con el otro, pero el valor de p es mayor a 0.05 entonces realmente no se requiere
#F
summary(logis1)#son los valores z
#G
confint(logis1)


#EJERCICIO 3
#a. Ajustar un polinomio de segundo orden.
#b. Probar la significancia de la regresión.
#c. Probar la falta de ajuste y llegar a conclusiones.
#d. ¿Contribuye al modelo el término de interacción, en forma significativa?
#e. ¿Contribuyen al modelo los términos de segundo orden, en forma significativa? 
data("p7.6")
ej3<-p7.6
ej3
#y carbonatación
#x1 temperatura
#x2 presión

poli<-lm(y~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=ej3)
summary(poli)

#SIGNIFICANCIA REGRESIÓN
#el valor de F tuvo un valor de p menor a 0.05 por lo tanto la regresión si es significativa.

#FALTA DE AJUSTE
#comparar con un modelo de errores
errores<-lm(y~as.factor(x1):as.factor(x2),data=ej3)
anova(poli,errores)
#no hay evidencia de falta de ajuste, el polinomio es adecuado para explicar el grado de carbonatación en base a la temperatura y presión

#TERMINO DE INTERACCIÓN
summary(poli)
#la interacción no contribuye de manera significativa 

modlinej3<-lm(y~x1+x2,data=ej3)
anova(modlinej3,poli)
#si es significativo el uso de los terminos de segundo orden, pero de manera marginal ya que solo es menor al limite de significancia por 0.01

#EJERCICIO 7.2
#a. Ajustar un polinomio de segundo orden que exprese la pérdida de peso en funciónde la cantidad de meses después de haber sido producido.
#b. Pruebe la significancia de la regresión.
#c. Pruebe la hipótesis H0: /32 = O. Comente la necesidad del término cuadrático en este modelo.
#d. ¿Hay riesgos potenciales al extrapolar con este modelo?
data("p7.2")
ej4<-p7.2
ej4
poli2<-lm(y~x+I(x^2),data=ej4)
summary(poli2)
#SIGNIFICANCIA DE REGRESIÓN
#el valor de p es mucho menor a 0.05 entonces pues si es significativa, o sea que si el tiempo transcurrido si ayuda a predecir la pérdida de peso

#HIPOTESIS NULA beta2= 0
#la beta 2 si es significativa tiene un valor mucho menor a 0.05

#RIESGOS POR EXTRAPOLAR
#no resultaría bueno tratar de predecir lo que pasará después del tiempo del experimento, primero porque 
#al tratarse de una curva, eso significa que llegará a un punto máximo o mínimo y luego va a cambiar de dirección. Además 
#este proceso normalmente es asintótico, es decir se llega a un peso deseado y se establiza, pero al usar una ecuación cuadrática al 
#predecir meses futuros se reflejará una ganancia de peso debido a la naturaleza de la ecuación porque nos da una curva, o que el peso se volvió negativo. 