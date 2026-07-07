#EXAMEN A CASA
library(MVA)
library(car)
library(tidyverse)
install.carinstall.packages("MVA")
#hacer análisis exploratorios, si vemos que los supuestos no se cumplen hacer modificaciones
# TODO JUSTIFICADO
#CONTESTAR LA PREGUNTA U OBJETIVO QUE CREAN QUE ES MÁS ADECUADO



#EJERCICIO 1
data("USairpollution")
view(USairpollution)
aire<-USairpollution
#saber cuales son los mejores indicadores o predictores de contaminación atmos medido según el contenido de SO2 en aire
tapply(aire$temp,aire$SO2,summary)
aire$SO2
#esta es una regresión linear múltiple
head(USairpollution)
ejercicio1<-lm(SO2~temp+manu+popul+wind+precip+predays,data=aire)
summary(ejercicio1)
ejercicio1.1<-step(ejercicio1)
summary(ejercicio1.1)#62% de ajuste
shapiro.test(ejercicio1.1$residuals)#no hay distribución normal 
leveneTest(aire$SO2,data=aire)
plot(ejercicio1.1)
library(car)
vif(ejercicio1.1)#manufactura y población están altamente correlacionados
boxCox(ejercicio1.1)
bc<-boxCox(ejercicio1)
lambda_optimo <- bc$x[which.max(bc$y)]
print(lambda_optimo)


ejercicio1.2<-lm(log(SO2)~temp+manu+wind+precip,data=aire)
summary(ejercicio1.2)#aquí la R ajustada es del 41% es menor, pero con las otras subió a 58%
vif(ejercicio1.2)
shapiro.test(ejercicio1.2$residuals)#si son normales
ncvTest(ejercicio1.2)#varianzas constantes 
plot(ejercicio1.2)
durbinWatsonTest(ejercicio1.2)#no hay correlación 
aire$SO2
barplot(aire$SO2)

ejercicio1.3<-lm(SO2^-0.3~temp+manu+wind+precip,data=aire)
summary(ejercicio1.3)#40% de ajuste, si pongo las otras sube al 60%
vif(ejercicio1.3)#no hay correlaciones
shapiro.test(ejercicio1.3$residuals)
ncvTest(ejercicio1.3)
durbinWatsonTest(ejercicio1.3)
#Análisis numérico 
head(aire)
aire
summary(aire)
view(aire)
library(Rmisc)
summarySE(aire,measurevar="SO2")
summarySE(aire,measurevar="temp")
summarySE(aire,measurevar="manu")
summarySE(aire,measurevar="popul")
summarySE(aire,measurevar="wind")
summarySE(aire,measurevar="precip")
summarySE(aire,measurevar="predays")
tapply(aire,aire$manu,summary)
boxplot(aire$SO2)














#EJERCICIO 2
data("CHFLS")
view(CHFLS)
ejercicio2<-CHFLS
head(ejercicio2)
#EJERCICIO 3
data("skulls")
view(skulls)
#EJERCICIO 4
data("watervoles")
view(watervoles)
