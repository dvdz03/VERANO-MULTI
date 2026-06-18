niveles<-c(rep("A",3), rep("B",3),rep("C",3),rep("D",3))
niveles
orden<-sample(niveles,12)
orden
barberia<-c(140,108,76,400)
sanguich<-c(2,21,0,42)
noticia<-c(40,5,5,0)
mean(barberia)
mean(sanguich)
mean(noticia)
mean(c(barberia,sanguich,noticia))

promediototal
mean(promediototal)
sumacuadradotrata<-sum((16.25-69.91)^2,(12.5-69.91)^2,(181-69.91)^2)
sumacuadradotrata
sumacuadradotrata*4
(12.5-69.91)^2
(181-69.91)^2

sumatota<-sum(((140-69.91)^2),((108-69.91)^2),((76-69.91)^2),((400-69.91)^2),(2-69.91)^2,(21-69.91)^2,(0-69.91)^2,
  (42-69.91)^2,(40-69.91)^2,(5-69.91)^2,(5-69.91)^2,(0-69.91)^2)
sumatota
cosa<-sumatota-73958.03
sumacuadradostratamientos1<-73958.03
#cuadrado medio de tratamientos
sumacuadradostratamientos1/2
cosa/9
cosa
#ahora necesitamos hacer la prueba de F con 2 grados de libertad en el numerador y 9 grados de libertad en el denominador
36979.01/7586.765
#la probabilidad de la curva f en 2 grados en el numerador y 9 en el denominador 
#buscar la probabilida de obtener un valor tan grande como el que obtuvimos suponiendo que la hipótesis nula es verdadera
pf(4.87,2,9,lower.tail=F)
pf(9,2,9, lower.tail=F)


#ejemplo
set.seed(45678)
sustrato<-c(rep("Arroz",8), rep("Alpiste",8), rep("Sorgo",8 ))
num_arroz<-ceiling(rnorm(8,443, 100))
num_arroz
num_alpiste<-ceiling(rnorm(8,231,100))
num_alpiste
num_sorgo<-ceiling(rnorm(8,172,100))
num_sorgo
rendimiento<-c(num_arroz,num_alpiste,num_sorgo)
tapply(rendimiento,sustrato, summary)
boxplot(rendimiento ~ sustrato)
respuesta<-sqrt(rendimiento)#la transformación más común es la raíz cuadrada, sería adecuado en el caso de tener porcentajes o unan variable cuantitativa discreta
modelo<-aov(respuesta~sustrato)#del lado izquierdo la respuesta y del derecho la predictora o predictoras
summary(modelo)#residuales==errores
#para ver si los residuales se distribuyen de manera noraml 
#primero se tiene que probar la normalidad de los residuales
shapiro.test(modelo$residuals)
#el valor de p aquí es más grande que 0.05 o sea que sin normales
#ahora si la prueba de barlett
bartlett.test(respuesta~sustrato)#esta prueba depende de la normalidad


#ilustrar las pruebas con gráficos de residuales
#valores ajustados en anova son los promedios de los tratamientos y los residuales los calculamos entre la diferencia entre la observación y el valor de la media del tratamiento
#lo que queremos observar en estos gráficos es que los residuales están dispersos por todos lados y no hay ningún patrón. 
#hay patrones que no están bien y que son así bien asuichis como que estén en forma de cono
#también un cono al reves, un rombo, o una curva
#todo esto es evidencia de que las varianzas no son iguales, o sea heterocedasticidad
#la curva es evidencia de no lineal, las heterocidástica son los conos y el rombo
#se grafica el modelo para ver todo esto
plot(modelo,which=1)
plot(modelo,which=2)#si los puntos están cerca de la línea es normal

#COMPARACIÓN DE MEDIAS
install.packages("multcompView")
library(multcompView)
TUK<-TukeyHSD(modelo)#tukey honest significant difference
TUK
#nos da las comparaciones de todas, la p ajustada es porque ajustó el alfa porque estamos haciendo 3 comparaciones entonces es necesario un ajuste 
#ya con eso se puede evaluar
#arroz vs alpiste son muy diferentes
#sorgo y alpiste son iguales
#sorgo y arroz son significativamente diferentes
plot(TUK)
#ESTO es de los intervalos de confianza
#ahí hay uno con 0 y entonces si uno incluye al 0 entonces son iguales estadísticamente

#esto de abajo es como para organizar las cosas, lo hace multcompView, para organizar por media o cosas así 
#ya con esto ya ilustraste el anova y así bien asuichis
#es más informativo con los boxplot porque muestras la distribución y tus pruebas post hoc
#lo más común son gráficos de barras que grafican las medias

generate_label_df <- function(TUK, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUK[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUK , "sustrato")

# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

# Draw the basic boxplot
a=boxplot(respuesta ~ sustrato ,  col=my_colors[as.numeric(LABELS[,1])] , ylab="valor" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over=0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(sustrato)) , a$stats[nrow(a$stats),] + over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )




#EJERCICIO SCRIPT2
library(faraway)
data("rats")
rats
attach(rats)
m1<-aov(time~poison*treat, data = rats)
summary(m1)
interaction.plot(poison, treat, time)

friedman.test(time~poison+treat, data = rats)

#era el otro ejercicio el de las ratas no mijo
library(tidyverse)
library(ggthemes)
library(multcompView)


#orimero un resumen numérico y gráfico
chickwts
summary(chickwts)
library(Rmisc)
summarySE(chickwts,measurevar="weight",groupvars="feed")
str(chickwts)
boxplot(chickwts$weight~chickwts$feed)
shapiro.test(chickwts$weight)#los datos si son normales
hist(chickwts$weight)
#prueba de shapiro a los residuales
shapiro.test(anova$residuals)
#concluyo que la distribución observada es muy semejante a la teórica entonces si se distribuyen de manera normal, ahora si la prueba de bartlett

bartlett.test(chickwts$weight~chickwts$feed)



boxplot(weight ~ feed, data = chickwts,las=2,
        col = c("green", "red","purple","blue","yellow","gray"),
        ylab = "Peso",
        xlab = "Tipo de alimento",
        main = "Diagrama de caja del peso de los pollos por alimento")


#los grados de libertad es todos las observaciones menos 1 o sea 70
#la de los tratamientos es tratamientos -1 o sea 5
#el error serían 71-6= 65

# Análisis de varianza
anova <- aov(weight ~ feed, data = chickwts)
summary(anova)#esto es para la tabla de anova, tiene un valor más chico de 0.05 y ahora si tukey, post hoc

bartlett.test(chickwts$weight~chickwts$feed)#residuales manera normal, varianzas homogéneas ahora si la tukey

# Pruebas poshoc de Tukey
tukey <- TukeyHSD(anova)
tukey
#la caseina y girasol son iguales, también con harina, todo es con los valores de p ajustados



# Organización de los trt 
cld <- multcompLetters4(anova, tukey)#sacar los grupos 
cld
#table with factors and 3rd quantile
library(dplyr)
dt <- group_by(chickwts, feed) %>%
  summarise(w=mean(weight), sd = sd(weight)) %>%
  arrange(desc(w))
#es una tabla con medias y desviaciones estandar ordenado por medias,el %>% concatena
dt
# Extracción de las letras
cld <- as.data.frame.list(cld$feed)
cld
dt$cld <- cld$Letters

print(dt)

# Gráfico de medias con grupos
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
  ylim(0,410) +
  theme_few()
