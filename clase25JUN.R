#ANCOVA
#analisis de varianza pero tienes una covariable, algo que no necesariamente te interesa medir pero lo quieres poner. 
#variable de respuesta continua, explicativa continua y covariable discreta

#cargar paquetes
library(tidyverse)
library(car)
library(faraway)
install.packages("Hmisc")
library(Hmisc)

data(cathedral)
cathedral
#relación altura ancho en catedrales góticas y r no se que

# Resumen numérico
lapply(split(cathedral,cathedral$style),summary)
library(Rmisc)
summarySE(data=cathedral, measurevar = "y", groupvars = "style")#desviación estándar si varía más o menos entre las dos 
summarySE(data=cathedral, measurevar = "x", groupvars = "style")#desviaciones son diferentes

# Resumen gráfico
plot(cathedral$x,cathedral$y,
     type="n",
     xlab="Altura",
     ylab="Largo")

text(cathedral$x,cathedral$y,as.character(cathedral$s))#para ver como están los datos, se ve una relación clara entre altura y largo (¿?), entre las góticas, las romanas varían mucho menos y están más dispersas
#las góticas no están tan dispersas como las otras. 

plot(cathedral$x,cathedral$y,
     pch=16,
     col=(rainbow(2))[cathedral$s],
     xlab="Altura",
     ylab="Largo")#esto es lo mismo pero ahora con colores

ggplot(cathedral, aes(x=x, y=y, color=style, shape=style)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)#lineas de regresión, se ven diferentes. Probar las hiptoesis si las pendientes de los dos son diferentes o no 

scatterplot(y ~ x + style | style, data=cathedral)#hay una curva cno las romanas lo que demuestra que se comportan más diferente que las gótcas. 

boxplot(x~style, data=cathedral)#góticas más variabilidad en comparación a las romanas
boxplot(y~style, data=cathedral)#lo mismo pasa con altura y ancho.

# ¿Existe alguna relación entre la altura y el estilo?

m0<-aov(x ~ style, data=cathedral)#anova para ver si hay relación entre altura y estilo
summary(m0)#no se rechaza la hipótesis nula. 
#el valor de p es la probabilidad de obtener la f que obtuvimos asumiendo que la hipótesis nula es verdadera.
#entonces no hay diferencias significativas
#primero haber verificado los supuestos antes de hacer el anova
##SUPUESTOS, PRIMERO NORMALIDAD
shapiro.test(m0$residuals)#si se distribuyen de manera normal
bartlett.test(cathedral$x~cathedral$style)#las varianzas no son iguales


# ANCOVA
m1 <- lm(y ~ x * style, data=cathedral)
shapiro.test(m1$residuals)
ncvTest(m1)
durbinWatsonTest(m1)

summary(m1)


m2<- lm(y ~ x + style, data=cathedral)
shapiro.test(m2$residuals)
ncvTest(m2)
durbinWatsonTest(m2)

summary(m2)

# Nuestra conclusión es que para catedrales de la misma altura, las románicas son 8.39 pies más largas. 
# Por cada pie adicional de altura, ambos tipos de catedral son aproximadamente 4,7 pies más largos. 
# Las catedrales góticas son tratadas como el nivel de referencia porque la “g” viene antes de la “r” en el alfabeto.

# Ejercicio data(twins)
?twins
# https://bpspsychub.onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1966.tb01014.x