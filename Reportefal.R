#reporte que me falta
data("twins")#base de datos entre comparación entre IQ de gemelos que fueron separados o algo así
R7<-twins
head(R7)
str(R7)
R7
#resumen numérico 
lapply(split(R7,R7$Social),summary)
library(Rmisc)
summarySE(R7,measurevar="Foster",groupvars="Social")
summarySE(R7,measurevar="Biological",groupvars="Social")
#Resumen gráfico
plot(R7$Biological,R7$Foster)
plot(R7$Foster,R7$Biological,
     type="n",
     xlab="Altura",
     ylab="Largo")
text(R7$Foster,R7$Biological,as.character(R7$Social))
plot(R7$Foster,R7$Biological,
     pch=16,
     col=(rainbow(3))[R7$Social],
     xlab="Foster",
     ylab="Biological")

#Supuestos anova 
shapiro.test(d0$residuals)#si hay distribución normal
bartlett.test(R7$Foster~R7$Social)#si hay homogeneidad de varianzas
shapiro.test(d1$residuals)
bartlett.test(R7$Biological~R7$Social)
#Relaciones entre las variables de Foster y Biological
# ¿Existe alguna relación entre la altura y el estilo?
d0<-aov(Foster~Social, data=R7)#esto es para determinar si hay alguna relación entre lo de adoptados y el factor social
summary(d0)#valor de p mayor a 0.05 entonces 
d1<-aov(Biological~Social,data=R7)
summary(d1)



# ANCOVA
#faraway usa la lm para el ancova. ancho como variable respuesta y altura como explicativa y su covariable es el estilo
an1<-lm(Foster~Biological*Social, data=R7)
shapiro.test(an1$residuals)#si es normal
ncvTest(an1)#no hay heterocisticidad
durbinWatsonTest(an1)#independientes
boxCox(an1)
summary(an1)#la ordenada al origen, la beta 0 si pasa por el origen, b1 no pasa, las interacciones no son significativas.
#se ajusta un 75% 



an2<-lm(Foster~Biological+Social,data=R7)
shapiro.test(an2$residuals)
ncvTest(an2)
durbinWatsonTest(an2)
summary(an2)#ordenada al origen 0, pendiente es diferente a 0,social igual a 0, ajuste del 77%
summary(m2)
#la ordenada al origen sigue siendo igual a 0, la pendiente de la altura es diferente de 0, el estilo es diferente de 0
#mejoró la explicación, el ajuste del modelo ahora es de 49%
#es un modelo de regresión en donde hay variables de diferente tipo


# Nuestra conclusión es que para catedrales de la misma altura, las románicas son 8.39 pies más largas. 
# Por cada pie adicional de altura, ambos tipos de catedral son aproximadamente 4,7 pies más largos. 
# Las catedrales góticas son tratadas como el nivel de referencia porque la “g” viene antes de la “r” en el alfabeto.
