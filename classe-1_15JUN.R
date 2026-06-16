install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
str("NivelesNutritivos")
datos<- read.table("C:/Users/100032608/Downloads/NivelesNutritivos.txt", header=T)
datos
str(datos)
datos$Region<-factor(datos$Region)
datos$Variedad<-factor(datos$Variedad)
#que hacer con los datos estos
boxplot(datos$NivelesNutritivos~datos$Region)
boxplot(datos$NivelesNutritivos~datos$Variedad)
#métodos paramétricos, modelo, ANOVA distribución F (fisher-varianzas), modelo estricto, no puede comparar distribuciones con distintas formas, varianzas semejantes
#si hay mucho sesgo ya valió, la distribución F requiere una distribución normal, verificar que los supuestos se cumplen. 
#media moda mediana, percentiles, quartiles quintiles. 
#Función asix, primero por región

mediasRegion <- tapply(datos$NivelesNutritivos, datos$Region, summary)
mediasRegion
#aquí se ve que la media y la mediana están muy cercanas entre sí, eso quiere decir que las distribuciones están más o menos centradas

mediasVariedad <- tapply(datos$NivelesNutritivos, datos$Variedad, summary)
mediasVariedad

desviacionregion <-tapply(datos$NivelesNutritivos, datos$Region, sd)
desviacionregion
desviacionvariedad <-tapply(datos$NivelesNutritivos, datos$Variedad, sd)
desviacionvariedad

install.packages("Rmisc")
library(Rmisc)

summarySE(data=datos, measurevar= "NivelesNutritivos", groupvars="Region")
summarySE(data=datos, measurevar= "NivelesNutritivos", groupvars="Variedad")
summarySE(data=datos, measurevar= "NivelesNutritivos", groupvars=c("Region","Variedad"))

#el error estandar o se es la desviación estandar de no se que, o sea la presición, entre más grande, el denominador es más grande y el resultado es más chiquito, más réplicas más robusto o algo así
#distribución Z es la normal, distribución de T eso que
#entonces ahí muestras pequeñas pueden tener forma acampanada, para eso es la T, están aplanadas,
#La de T es una familia de distribuciones, la forma de esa depende de sus grados de libertad, por ejemplo, 24 ind. (-30) o sea hay 23 grados de libertad, para esta serie necesitariamos la distribución de T para 23 gdl
#hay tablas pero R las da jeje


#para distribución de T 

zeta<- qnorm(0.025)
zeta
tcero25<- qt(0.025,24)
xi2cero25<-qchisq(0.025,24)
tcero25
xi2cero25

#cargar los datos
datos1<- read.table("C:/Users/100032608/Downloads/t5_p1.txt", header=TRUE)
datos1
#PENDIENTES LOS DE ABAJO estos todavía no se hacen alv y no se ocupan
datos2<- read.table("C:/Users/100032608/Downloads/t5_p2.txt", header=TRUE)
datos2
datos3<- read.table("C:/Users/100032608/Downloads/t5_p3.txt", header=TRUE)
datos3
datos5<- read.table("C:/Users/100032608/Downloads/t5_p5.txt", header=T)
datos5

#RESUMEN NÚMERICO: MINIMO MÁXIMO QUARTILES
#MEDIANA Y MEDIA
#RANGO DESVIACIÓN VARIANZA

### MEDIDS DE POSICIÓN
summary(datos1$edad)
#rango amplio, media y mediana como cerca entre si pero no tanto
rango <-max(datos1$edad)-min(datos$edad)
#la serie de datos incluye valores muy pequeños y grandes el valor del rango es quien sabe
#Medidas tendencia central y dispersión
library(Rmisc) # se usa este porque tiene un 
summarySE(data=datos1, measurevar= "edad")
# lo que nos da es la media, desviación estandar, el error, y el intervalo bueno el valor de e.
# esto para sacar el intervalo de confianza para la media 

#ya todo esto es el resumen numérico pero falta el resumen gráfico
#con un boxplot
boxplot (datos1$edad)
#la distribución tiene un sesgo positivo
hist(datos1$edad)
#es una distribución sesgada que no parece ser normal
#prueba de shapiro
shapiro.test(datos1$edad)
#el valor w se obtiene en una tabla de shapiro, se tiene que el valor de la probabilidad para esa distribucion es el p-value. 
#para entender el valor de p hay que plantear las hipótesis nula y alterna
#NULA: datos se dsitribuyen de manera normal
#ALTERNA: no se distribuyen de manera normal
#en las pruebas de hipótesis se prueba siempre la nula, el valor de 0.88
#el valor de p nos dice cual es la probabiliad de obtener un valor como el de 0.88 suponiendo que la hipóteis nula se cumple.
#entonces como el valor de p es menor al 0.05 entonces significa o poedmos decir que la distribución es significativamente diferente a la distribución normal. 

#Esto se tiene que hacer así de asuichis con todas las bases de datos, gráficos y numéricos. poner el intervalo de confianza, cuanto vale el mínimo y el máximo
#para que quede el análisis exploratorio de los datos