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
