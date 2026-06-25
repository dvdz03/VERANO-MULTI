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
#EMPEZAR CON TRANSFORMACIÓN DE VARIANZA
m01<-aov(x^0.5 ~ style, data=cathedral)
shapiro.test(m01$residuals)#si son normales, si no son normales es levene
bartlett.test(x^0.5~style,data=cathedral)#no sirvió
#AHORA EL INVERSO
bartlett.test(x^-1~style,data=cathedral)
#INVERSO RAÍZ
bartlett.test(x^-0.5~style,data=cathedral)
#LOGARITMO
bartlett.test(log(x)~style,data=cathedral)

#USAR LA FUNCIÓN LM SIN TRANSFORMAR variable explicativa cuantitativa continua y respuesta cuantitativa continua
#le estoy poniendo una nominal, que hace R= convierte la nominal en variable dummy porque no puede usar las categóricas en regresión
#la hace dummy pero pues asi bien asuich. 
m02<-lm(x^0.9~style,data=cathedral)
shapiro.test(m02$residuals)
bartlett.test(log(x)~style,data=cathedral)
#no queda de otra más que modelar la varianza
#boxCox
boxCox(m02,lambda=seq(-2,3,1/10))

# ANCOVA
#faraway usa la lm para el ancova. ancho como variable respuesta y altura como explicativa y su covariable es el estilo
m1 <- lm(y ~ x * style, data=cathedral)#ya con esto mejoró el problema de las varianzas
shapiro.test(m1$residuals)#no es normal
ncvTest(m1)#no hay heterocisticidad 
durbinWatsonTest(m1)#datos independientes
boxCox(m1,lambda=seq(-2,3,1/10))
#transformación raíz cuadrada
m1 <- lm(y^1.5 ~ x * style, data=cathedral)#con raíz no salió pero con 1.5 si
summary(m1)#la ordenada al orgien no es significativa o sea que el modelo pasa por el origen.
#la beta 0 (ordenada al origen) pasa por el origen esa es la hipótesis nula
#la altura es diferente a 0 si tiene un efecto porque el p valor es menor a 0.05
#el estilo no importa y tampoco la interacción
#47% de explicación, no tan bueno asuich
#como la interacción tiene el valor de p más grande se elimina del modelo adiós 


m2<- lm(y ~ x + style, data=cathedral)#aquí se quita la interacción, solo se suma
shapiro.test(m2$residuals)#no son normales, la desviación es muy pequeña
ncvTest(m2)
durbinWatsonTest(m2)

summary(m2)
#la ordenada al origen sigue siendo igual a 0, la pendiente de la altura es diferente de 0, el estilo es diferente de 0
#mejoró la explicación, el ajuste del modelo ahora es de 49%
#es un modelo de regresión en donde hay variables de diferente tipo


# Nuestra conclusión es que para catedrales de la misma altura, las románicas son 8.39 pies más largas. 
# Por cada pie adicional de altura, ambos tipos de catedral son aproximadamente 4,7 pies más largos. 
# Las catedrales góticas son tratadas como el nivel de referencia porque la “g” viene antes de la “r” en el alfabeto.

# Ejercicio data(twins)
?twins
# https://bpspsychub.onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1966.tb01014.x

#paquete para hacer gráficos y así bontio lindo yasí
install.packages("factoextra")
library(factoextra)
#utilizar los datos iris
data("iris")
iris
#componentes principales solo puedo usar variables numéricas
# ANÁLISIS DE COMPONENTES PRINCIPALES

library(tidyverse)
library(factoextra)
library(GGally)

iris_2<-iris[,1:4]
view(iris_2)
especies<-iris$Species

ggpairs(iris)#parece que las variables si tienen relación tal vez no perfecta pero si una relación, hay corrrelaciones significativas 
#pero lo ideal es que todas lo tengan alv


res.pca3 <- prcomp(iris[, -5],  scale = TRUE)

fviz_eig(res.pca3)
get_eig(res.pca3)

fviz_pca_ind(res.pca3, label="none", habillage=iris$Species,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")


fviz_pca_var(res.pca3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca3, lrepel=T, habillage=iris$Species, 
                palette="Dark2",
                col.var = "#080878") # Variables color


# Eigenvalues
eig.val <- get_eigenvalue(res.pca3)
eig.val

# Resultado para variables
res.var <- get_pca_var(res.pca3)
res.var$coord          # Coordenadas
res.var$contrib        # Contribución  PCs
res.var$cos2           # Calidad de la representación 

# Resultados para individuos
res.ind <- get_pca_ind(res.pca3)
res.ind$coord          # Coordenadas
res.ind$contrib        # Contribución  PCs
res.ind$cos2           # Calidad de la representación


#####################
otroscp<-princomp(iris_2)


PC1<-otroscp$scores[,1]
PC2<-otroscp$scores[,2]

plot(PC1,PC2,pch=16, col=rainbow(3)[especies])
